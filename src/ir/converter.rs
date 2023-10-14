use rustc_hir as hir;
use rustc_hir::def_id::LocalDefId;
use rustc_middle::ty::TypeckResults;

use std::{str::FromStr, sync::Arc};

use crate::{
    ir::{MatchGuard, OpaqueTypeMapping},
    rustc_worker::RustCContext,
    types::{FloatWidth, Mutability, Type, TypeKind},
};

use super::{
    BinaryOp, BindingMode, Block, Expr, ExprId, ExprKind, FieldPattern, IRFunction,
    IRFunctionBuilder, IRKind, LogicOp, LoopId, MatchArm, Pattern, PatternId, PatternKind,
    PointerCast, Stmt, UnaryOp, UpVar,
};

/// Converts rust IR to skitter IR.
///
/// Now uses TypeckResults instead of THIR because THIR can get randomly stolen when querying other THIR.
pub struct IRFunctionConverter<'vm, 'tcx, 'a> {
    ctx: &'a RustCContext<'vm, 'tcx>,
    func_id: LocalDefId,
    types: &'tcx TypeckResults<'tcx>,
    loops: Vec<(hir::HirId, LoopId)>,
    builder: IRFunctionBuilder<'vm>,
}

impl<'vm, 'tcx, 'a> IRFunctionConverter<'vm, 'tcx, 'a> {
    pub fn run(
        ctx: &'a RustCContext<'vm, 'tcx>,
        func_id: LocalDefId,
        body: &rustc_hir::Body,
        types: &'tcx TypeckResults<'tcx>,
        ir_kind: IRKind,
    ) -> IRFunction<'vm> {
        //println!("func = {:?}",func_id);
        assert!(body.generator_kind.is_none());

        let opaque_types = types
            .concrete_opaque_types
            .iter()
            .map(|(key, val)| {
                let (item, full_path) =
                    ctx.vm
                        .types
                        .opaque_type_from_rustc(key.def_id.into(), key.substs, ctx);

                let destination_ty = ctx.type_from_rustc(val.ty);

                OpaqueTypeMapping {
                    source_item: item,
                    source_full_path: full_path,
                    destination_ty,
                }
            })
            .collect();

        let mut converter = Self {
            ctx,
            func_id,
            types,
            loops: vec![],
            builder: Default::default(),
        };

        let params: Vec<_> = body
            .params
            .iter()
            .map(|param| converter.pattern(param.pat))
            .collect();

        let root_expr = converter.expr(body.value);

        converter
            .builder
            .finish(root_expr, ir_kind, params, opaque_types)
    }

    /// Convert binary op from rust IR. Does not handle logical ops.
    fn bin_op(&self, op: hir::BinOpKind) -> BinaryOp {
        use hir::BinOpKind;
        match op {
            BinOpKind::Add => BinaryOp::Add,
            BinOpKind::Sub => BinaryOp::Sub,
            BinOpKind::Mul => BinaryOp::Mul,
            BinOpKind::Div => BinaryOp::Div,
            BinOpKind::Rem => BinaryOp::Rem,

            BinOpKind::BitAnd => BinaryOp::BitAnd,
            BinOpKind::BitOr => BinaryOp::BitOr,
            BinOpKind::BitXor => BinaryOp::BitXor,
            BinOpKind::Shr => BinaryOp::ShiftR,
            BinOpKind::Shl => BinaryOp::ShiftL,

            BinOpKind::Eq => BinaryOp::Eq,
            BinOpKind::Ne => BinaryOp::NotEq,
            BinOpKind::Lt => BinaryOp::Lt,
            BinOpKind::Gt => BinaryOp::Gt,
            BinOpKind::Le => BinaryOp::LtEq,
            BinOpKind::Ge => BinaryOp::GtEq,
            _ => panic!(),
        }
    }

    fn expr(&mut self, expr: &hir::Expr) -> ExprId {
        let rs_ty = self.types.expr_ty(expr);
        let ty = self.ctx.type_from_rustc(rs_ty);

        let expr_kind = match expr.kind {
            hir::ExprKind::Lit(_) => self.expr_literal(expr, ty, false),
            hir::ExprKind::Cast(arg, _) => ExprKind::Cast(self.expr(arg)),
            hir::ExprKind::AddrOf(_, mutability, arg) => {
                let mutability = match mutability {
                    rustc_ast::Mutability::Mut => Mutability::Mut,
                    rustc_ast::Mutability::Not => Mutability::Const,
                };
                ExprKind::Ref(self.expr(arg), mutability)
            }
            hir::ExprKind::Unary(op, arg) => {
                let arg = self.expr(arg);

                if let Some(func_did) = self.types.type_dependent_def_id(expr.hir_id) {
                    let subs = self.types.node_substs(expr.hir_id);
                    let func_item = self.ctx.vm.types.def_from_rustc(func_did, subs, &self.ctx);
                    let func_ty = self.ctx.vm.ty_func_def(func_item);

                    let func = self.builder.add_expr(Expr {
                        kind: ExprKind::LiteralVoid,
                        ty: func_ty,
                    });

                    if let hir::UnOp::Deref = op {
                        self.expr_deref_overload(func, arg, ty)
                    } else {
                        ExprKind::Call {
                            func,
                            args: vec![arg],
                        }
                    }
                } else if let hir::UnOp::Deref = op {
                    ExprKind::DeRef(arg)
                } else {
                    let op = match op {
                        hir::UnOp::Not => UnaryOp::Not,
                        hir::UnOp::Neg => UnaryOp::Neg,
                        _ => panic!(),
                    };

                    ExprKind::Unary(op, arg)
                }
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                match op.node {
                    hir::BinOpKind::And => ExprKind::LogicOp(LogicOp::And, lhs, rhs),
                    hir::BinOpKind::Or => ExprKind::LogicOp(LogicOp::Or, lhs, rhs),
                    _ => {
                        if let Some(func_did) = self.types.type_dependent_def_id(expr.hir_id) {
                            let subs = self.types.node_substs(expr.hir_id);
                            let func_item =
                                self.ctx.vm.types.def_from_rustc(func_did, subs, &self.ctx);
                            let func_ty = self.ctx.vm.ty_func_def(func_item);

                            let func = self.builder.add_expr(Expr {
                                kind: ExprKind::LiteralVoid,
                                ty: func_ty,
                            });

                            ExprKind::Call {
                                func,
                                args: vec![lhs, rhs],
                            }
                        } else {
                            let op = self.bin_op(op.node);

                            ExprKind::Binary(op, lhs, rhs)
                        }
                    }
                }
            }
            hir::ExprKind::AssignOp(op, lhs, rhs) => {
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                if let Some(func_did) = self.types.type_dependent_def_id(expr.hir_id) {
                    let subs = self.types.node_substs(expr.hir_id);
                    let func_item = self.ctx.vm.types.def_from_rustc(func_did, subs, &self.ctx);
                    let func_ty = self.ctx.vm.ty_func_def(func_item);

                    let func = self.builder.add_expr(Expr {
                        kind: ExprKind::LiteralVoid,
                        ty: func_ty,
                    });

                    ExprKind::Call {
                        func,
                        args: vec![lhs, rhs],
                    }
                } else {
                    let op = self.bin_op(op.node);
                    ExprKind::AssignOp(op, lhs, rhs)
                }
            }
            hir::ExprKind::Assign(lhs, rhs, _) => {
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                ExprKind::Assign(lhs, rhs)
            }
            hir::ExprKind::Call(func, args) => {
                if let Some(func_did) = self.types.type_dependent_def_id(expr.hir_id) {
                    let old_func = self.expr(func);

                    // assume this is a Fn* trait call
                    let func_subs = self.types.node_substs(expr.hir_id);
                    let func_item = self
                        .ctx
                        .vm
                        .types
                        .def_from_rustc(func_did, func_subs, &self.ctx);

                    assert!(func_item.subs.list.len() == 2);

                    let arg_tup_ty = func_item.subs.list[1].assert_ty();

                    let func_ty = self.ctx.vm.ty_func_def(func_item);

                    let args: Vec<_> = args.iter().map(|arg| self.expr(arg)).collect();

                    let arg_tup = self.builder.add_expr(Expr {
                        kind: ExprKind::Tuple(args),
                        ty: arg_tup_ty,
                    });

                    let func = self.builder.add_expr(Expr {
                        kind: ExprKind::LiteralVoid,
                        ty: func_ty,
                    });

                    ExprKind::Call {
                        func,
                        args: vec![old_func, arg_tup],
                    }
                } else {
                    let func = self.expr(func);

                    let args = args.iter().map(|arg| self.expr(arg)).collect();

                    ExprKind::Call { func, args }
                }
            }
            hir::ExprKind::MethodCall(_, lhs, args, _) => {
                // We need to pull the actual method def from typeck results.
                let method_did = self.types.type_dependent_def_id(expr.hir_id).unwrap();
                let method_subs = self.types.node_substs(expr.hir_id);
                let method_item =
                    self.ctx
                        .vm
                        .types
                        .def_from_rustc(method_did, method_subs, &self.ctx);
                let method_ty = self.ctx.vm.ty_func_def(method_item);

                let lhs = self.expr(lhs);
                let args = args.iter().map(|arg| self.expr(arg));

                let full_args = std::iter::once(lhs).chain(args).collect();

                let func = self.builder.add_expr(Expr {
                    kind: ExprKind::LiteralVoid,
                    ty: method_ty,
                });

                ExprKind::Call {
                    func,
                    args: full_args,
                }
            }
            hir::ExprKind::If(cond, then, else_opt) => {
                let cond = self.expr(cond);
                let then = self.expr(then);
                let else_opt = else_opt.map(|e| self.expr(e));
                ExprKind::If {
                    cond,
                    then,
                    else_opt,
                }
            }
            hir::ExprKind::Match(arg, arms, _) => {
                use rustc_hir::Guard;

                let arg = self.expr(arg);
                let arms = arms
                    .iter()
                    .map(|arm| {
                        let guard = match arm.guard {
                            Some(Guard::If(expr)) => MatchGuard::If(self.expr(&expr)),
                            Some(Guard::IfLet(_)) => MatchGuard::IfLet,
                            None => MatchGuard::None,
                        };

                        MatchArm {
                            pattern: self.pattern(arm.pat),
                            body: self.expr(arm.body),
                            guard,
                        }
                    })
                    .collect();
                ExprKind::Match { arg, arms }
            }
            hir::ExprKind::Loop(body, ..) => {
                let loop_id = LoopId::new(self.loops.len() as u32);
                self.loops.push((expr.hir_id, loop_id));

                let body = self.block(body);
                ExprKind::Loop(body, loop_id)
            }
            hir::ExprKind::Break(dest, value) => {
                let target = dest.target_id.unwrap();
                let loop_id = self
                    .loops
                    .iter()
                    .find(|(h, _)| h == &target)
                    .copied()
                    .map(|(_, id)| id);

                if let Some(loop_id) = loop_id {
                    let value = value.map(|e| self.expr(e));
                    ExprKind::Break { loop_id, value }
                } else {
                    // TODO -- most likely caused by breaks bound to blocks instead of loops
                    ExprKind::Error("bad break".to_owned())
                }
            }
            hir::ExprKind::Continue(dest) => {
                let target = dest.target_id.unwrap();
                let (_, loop_id) = self
                    .loops
                    .iter()
                    .find(|(h, _)| h == &target)
                    .copied()
                    .unwrap();

                ExprKind::Continue { loop_id }
            }
            hir::ExprKind::Ret(value) => {
                let value = value.map(|e| self.expr(e));

                ExprKind::Return(value)
            }
            hir::ExprKind::Tup(args) => {
                let args = args.iter().map(|a| self.expr(a)).collect();
                ExprKind::Tuple(args)
            }
            hir::ExprKind::Struct(path, fields, rest) => {
                if rest.is_some() {
                    ExprKind::Error("adt with rest".to_owned())
                } else {
                    let res = self.types.qpath_res(&path, expr.hir_id);
                    let variant = Self::def_variant(&res, rs_ty);

                    let fields = fields
                        .iter()
                        .map(|field| {
                            let id = self.types.field_index(field.hir_id).as_u32();
                            let expr = self.expr(field.expr);
                            (id, expr)
                        })
                        .collect();

                    ExprKind::Adt { variant, fields }
                }
            }
            hir::ExprKind::Array(args) => {
                let args = args.iter().map(|a| self.expr(a)).collect();
                ExprKind::Array(args)
            }
            hir::ExprKind::Repeat(arg, _) => {
                let arg = self.expr(arg);
                let TypeKind::Array(_,size) = ty.kind() else {
                    panic!("bad type for repeated array")
                };
                ExprKind::ArrayRepeat(arg, size.clone())
            }
            hir::ExprKind::Index(lhs, index) => {
                let lhs = self.expr(lhs);
                let index = self.expr(index);
                ExprKind::Index { lhs, index }
            }
            hir::ExprKind::Field(lhs, _) => {
                let index = self.types.field_index(expr.hir_id);
                let lhs = self.expr(lhs);
                ExprKind::Field {
                    lhs,
                    variant: 0,
                    field: index.as_u32(),
                }
            }
            hir::ExprKind::Path(path) => {
                let res = self.types.qpath_res(&path, expr.hir_id);

                use hir::def::{CtorKind, DefKind, Res};
                match res {
                    Res::Def(def_kind, did) => {
                        match def_kind {
                            DefKind::Fn | DefKind::AssocFn | DefKind::Ctor(_, CtorKind::Fn) => {
                                // return void expression, any information needed
                                // can be pulled from the type
                                ExprKind::LiteralVoid
                            }
                            DefKind::Const
                            | DefKind::AssocConst
                            | DefKind::Ctor(_, CtorKind::Const) => {
                                let subs = self.types.node_substs(expr.hir_id);
                                let const_item =
                                    self.ctx.vm.types.def_from_rustc(did, subs, &self.ctx);
                                ExprKind::NamedConst(const_item)
                            }
                            DefKind::Static(_) => {
                                let static_item =
                                    self.ctx.vm.types.def_from_rustc(did, &[], &self.ctx);
                                ExprKind::Static(static_item)
                            }
                            _ => ExprKind::Error(format!("def = {:?}", def_kind)),
                        }
                    }
                    hir::def::Res::SelfCtor(_) => {
                        // return void expression, any information needed
                        // can be pulled from the type
                        ExprKind::LiteralVoid
                    }
                    hir::def::Res::Local(hir_id) => {
                        assert_eq!(hir_id.owner.def_id, self.func_id);
                        let local_id = hir_id.local_id.as_u32();

                        ExprKind::VarRef(local_id)
                    }
                    _ => panic!("path = {:?}", res),
                }
            }
            hir::ExprKind::Let(let_expr) => ExprKind::Let {
                pattern: self.pattern(let_expr.pat),
                init: self.expr(let_expr.init),
            },
            hir::ExprKind::Block(block, _) => {
                let block = self.block(block);
                ExprKind::Block(block)
            }
            hir::ExprKind::ConstBlock(const_block) => {
                let hir = self.ctx.tcx.hir();
                let types = self.ctx.tcx.typeck_body(const_block.body);
                let body = hir.body(const_block.body);

                let const_body =
                    IRFunctionConverter::run(self.ctx, self.func_id, body, types, IRKind::Constant);

                ExprKind::ConstBlock(Arc::new(const_body))
            }
            hir::ExprKind::DropTemps(e) => {
                // TODO TODO TODO
                // this may pose an issue when implementing destructors / drop
                return self.expr(e);
            }
            hir::ExprKind::Closure(closure) => {
                let body = self.ctx.tcx.hir().body(closure.body);
                let types = self.ctx.tcx.typeck_body(closure.body);

                let captures: Vec<_> = self
                    .types
                    .closure_min_captures_flattened(closure.def_id)
                    .collect();

                let capture_exprs: Vec<_> = captures
                    .iter()
                    .map(|capture| self.expr_capture(capture))
                    .collect();

                let closure = self
                    .ctx
                    .vm
                    .types
                    .closure_from_rustc(closure.def_id.into(), self.ctx);

                let mut ir =
                    IRFunctionConverter::run(self.ctx, self.func_id, body, types, IRKind::Function);
                replace_captures(&mut ir, &captures);

                let TypeKind::Closure(_, abstract_sig, abstract_subs) = ty.kind() else {
                    panic!("closure type is incorrect");
                };

                assert!(abstract_subs.is_identity());

                // assert capture types are correct!
                {
                    let cap_ty_a = capture_exprs.iter().map(|id| self.builder.expr(*id).ty);
                    if let TypeKind::Tuple(cap_ty_b) = abstract_sig.env_ty.kind() {
                        assert!(cap_ty_a.len() == cap_ty_b.len());
                        for (a, b) in cap_ty_a.zip(cap_ty_b) {
                            assert!(a == *b);
                        }
                    } else {
                        panic!("bad captures");
                    }
                }

                closure.set_abstract_sig(abstract_sig.clone());
                closure.set_ir_base(ir);

                ExprKind::Tuple(capture_exprs)
            }
            hir::ExprKind::InlineAsm(..) => ExprKind::Error("inline asm".to_owned()),
            _ => panic!("todo expr kind {:?}", expr.kind),
        };

        let mut expr_id = self.builder.add_expr(Expr {
            kind: expr_kind,
            ty,
        });

        // Keep track of the input types to adjustments so deref methods can be built easily.
        let mut adjust_ty_in_rs = rs_ty;
        let mut adjust_ty_in = ty;

        let adjust_list = self.types.expr_adjustments(expr);
        for adjust in adjust_list {
            let adjust_ty = self.ctx.type_from_rustc(adjust.target);

            use rustc_middle::ty::adjustment::Adjust;
            match adjust.kind {
                Adjust::NeverToAny => {
                    expr_id = self.builder.add_expr(Expr {
                        kind: ExprKind::Dummy(expr_id),
                        ty: adjust_ty,
                    });
                }
                Adjust::Deref(overloaded) => {
                    if let Some(overloaded) = overloaded {
                        let func_ty = overloaded.method_call(self.ctx.tcx, adjust_ty_in_rs);
                        let func_ty = self.ctx.type_from_rustc(func_ty);

                        let func = self.builder.add_expr(Expr {
                            kind: ExprKind::LiteralVoid,
                            ty: func_ty,
                        });

                        // for whatever reason, the rustc adjustments contain an extra deref
                        // before an overloaded deref, meaning we need to ref AGAIN in order
                        // for the deref to work

                        let arg = self.builder.add_expr(Expr {
                            kind: ExprKind::Ref(expr_id, Mutability::Const),
                            ty: adjust_ty_in.ref_to(Mutability::Const),
                        });

                        let kind = self.expr_deref_overload(func, arg, adjust_ty);
                        expr_id = self.builder.add_expr(Expr {
                            kind,
                            ty: adjust_ty,
                        });
                    } else {
                        expr_id = self.builder.add_expr(Expr {
                            kind: ExprKind::DeRef(expr_id),
                            ty: adjust_ty,
                        });
                    }
                }
                Adjust::Borrow(_) => {
                    expr_id = self.builder.add_expr(Expr {
                        kind: ExprKind::Ref(expr_id, Mutability::Const),
                        ty: adjust_ty,
                    });
                }
                Adjust::Pointer(ptr_cast) => {
                    use rustc_middle::ty::adjustment::PointerCast as PC;
                    let ptr_cast: PointerCast = match ptr_cast {
                        PC::ReifyFnPointer => PointerCast::ReifyFnPointer,
                        PC::UnsafeFnPointer => PointerCast::UnsafeFnPointer,
                        PC::ClosureFnPointer(_) => PointerCast::ClosureFnPointer,
                        PC::MutToConstPointer => PointerCast::MutToConstPointer,
                        PC::ArrayToPointer => PointerCast::ArrayToPointer,
                        PC::Unsize => PointerCast::UnSize,
                    };

                    expr_id = self.builder.add_expr(Expr {
                        kind: ExprKind::PointerCast(expr_id, ptr_cast),
                        ty: adjust_ty,
                    });
                }
                _ => panic!("todo adjust {:?}", adjust),
            }

            adjust_ty_in = adjust_ty;
            adjust_ty_in_rs = adjust.target;
        }

        expr_id
    }

    fn expr_deref_overload(
        &mut self,
        func: ExprId,
        arg: ExprId,
        res_ty: Type<'vm>,
    ) -> ExprKind<'vm> {
        // WARNING: ref mutability not necessarily correct
        // hopefully not an issue as the type shouldn't
        // be used for much
        let ref_ty = res_ty.ref_to(Mutability::Const);

        let res = self.builder.add_expr(Expr {
            kind: ExprKind::Call {
                func,
                args: vec![arg],
            },
            ty: ref_ty,
        });

        ExprKind::DeRef(res)
    }

    fn expr_literal(&self, expr: &hir::Expr, ty: Type<'vm>, negative: bool) -> ExprKind<'vm> {
        match expr.kind {
            hir::ExprKind::Lit(lit) => {
                use rustc_ast::ast::LitKind;
                match lit.node {
                    LitKind::Int(n, _) => {
                        let mut n = n as i128;
                        if negative {
                            n = -n;
                        }
                        ExprKind::LiteralValue(n)
                    }
                    LitKind::Float(sym, _) => {
                        let TypeKind::Float(float_width) = ty.kind() else {
                            panic!("non-float float literal");
                        };
                        match float_width {
                            FloatWidth::F32 => {
                                let mut n = f32::from_str(sym.as_str()).unwrap();
                                if negative {
                                    n = -n;
                                }
                                let x: i32 = unsafe { std::mem::transmute(n) };
                                ExprKind::LiteralValue(x as i128)
                            }
                            FloatWidth::F64 => {
                                let mut n = f64::from_str(sym.as_str()).unwrap();
                                if negative {
                                    n = -n;
                                }
                                let x: i64 = unsafe { std::mem::transmute(n) };
                                ExprKind::LiteralValue(x as i128)
                            }
                        }
                    }
                    LitKind::Bool(b) => {
                        assert!(!negative);
                        ExprKind::LiteralValue(if b { 1 } else { 0 })
                    }
                    LitKind::Char(c) => {
                        assert!(!negative);
                        ExprKind::LiteralValue(c as i128)
                    }
                    LitKind::Byte(b) => {
                        assert!(!negative);
                        ExprKind::LiteralValue(b as i128)
                    }
                    LitKind::Str(sym, _) => {
                        assert!(!negative);
                        let bytes = sym.as_str().as_bytes().to_owned();
                        ExprKind::LiteralBytes(self.ctx.vm.alloc_constant(bytes))
                    }
                    LitKind::ByteStr(ref bytes, _) => {
                        assert!(!negative);
                        let bytes = bytes.iter().copied().collect();
                        ExprKind::LiteralBytes(self.ctx.vm.alloc_constant(bytes))
                    }
                    _ => panic!("lit other {:?}", lit.node),
                }
            }
            hir::ExprKind::Unary(hir::UnOp::Neg, sub_expr) => self.expr_literal(sub_expr, ty, true),
            _ => panic!("lit = {:?}", expr.kind),
        }
    }

    fn expr_capture(&mut self, cap: &rustc_middle::ty::CapturedPlace<'tcx>) -> ExprId {
        let mut base_ty = self.ctx.type_from_rustc(cap.place.base_ty);

        let mut base = if let rustc_middle::hir::place::PlaceBase::Upvar(upvar) = cap.place.base {
            let local_id = upvar.var_path.hir_id.local_id.as_u32();

            self.builder.add_expr(Expr {
                kind: ExprKind::VarRef(local_id),
                ty: base_ty,
            })
        } else {
            panic!("cannot handle capture base {:?}", cap.place.base);
        };

        for proj in cap.place.projections.iter() {
            use rustc_middle::hir::place::ProjectionKind;
            let new_expr = match proj.kind {
                ProjectionKind::Field(field, variant) => ExprKind::Field {
                    lhs: base,
                    variant: variant.as_u32(),
                    field: field.as_u32(),
                },
                ProjectionKind::Deref => ExprKind::DeRef(base),
                _ => panic!("todo projection {:?}", proj),
            };

            base_ty = self.ctx.type_from_rustc(proj.ty);

            base = self.builder.add_expr(Expr {
                kind: new_expr,
                ty: base_ty,
            });
        }

        {
            use rustc_middle::ty::{BorrowKind, UpvarCapture};

            match cap.info.capture_kind {
                UpvarCapture::ByRef(BorrowKind::ImmBorrow) => {
                    base_ty = base_ty.ref_to(Mutability::Const);
                    base = self.builder.add_expr(Expr {
                        kind: ExprKind::Ref(base, Mutability::Const),
                        ty: base_ty,
                    });
                }
                UpvarCapture::ByRef(BorrowKind::UniqueImmBorrow)
                | UpvarCapture::ByRef(BorrowKind::MutBorrow) => {
                    base_ty = base_ty.ref_to(Mutability::Mut);
                    base = self.builder.add_expr(Expr {
                        kind: ExprKind::Ref(base, Mutability::Mut),
                        ty: base_ty,
                    });
                }
                UpvarCapture::ByValue => (),
            }
        }

        base
    }

    fn block(&mut self, block: &hir::Block) -> Block {
        let mut stmts = vec![];

        for stmt in block.stmts {
            if let Some(stmt) = self.stmt(&stmt) {
                stmts.push(stmt);
            }
        }

        let result = block.expr.map(|expr| self.expr(expr));

        Block { stmts, result }
    }

    fn stmt(&mut self, stmt: &hir::Stmt) -> Option<Stmt> {
        let res = match stmt.kind {
            hir::StmtKind::Expr(expr) | hir::StmtKind::Semi(expr) => Stmt::Expr(self.expr(&expr)),
            hir::StmtKind::Local(local) => {
                let pattern = self.pattern(local.pat);
                let init = local.init.map(|init| self.expr(init));
                let else_block = local.els.map(|else_block| self.block(else_block));

                Stmt::Let {
                    pattern,
                    init,
                    else_block,
                }
            }
            hir::StmtKind::Item(_) => return None,
        };

        Some(res)
    }

    fn pattern(&mut self, pat: &hir::Pat) -> PatternId {
        let rs_ty = self.types.pat_ty(pat);
        let ty = self.ctx.type_from_rustc(rs_ty);

        let pattern_kind = match pat.kind {
            hir::PatKind::Binding(_, hir_id, _, sub_pattern) => {
                let all_binding_modes = self.types.pat_binding_modes();
                let mode = all_binding_modes.get(pat.hir_id).unwrap();

                let mode = match mode {
                    rustc_middle::ty::BindingMode::BindByReference(_) => BindingMode::Ref,
                    rustc_middle::ty::BindingMode::BindByValue(_) => BindingMode::Value,
                };

                // should hold true, even for closures? when are we ever going
                // to bind a variable that doesn't belong to our current function?
                assert_eq!(hir_id.owner.def_id, self.func_id);
                let local_id = hir_id.local_id.as_u32();

                let sub_pattern = sub_pattern.map(|sub_pat| self.pattern(sub_pat));

                PatternKind::LocalBinding {
                    local_id,
                    mode,
                    sub_pattern,
                }
            }
            hir::PatKind::Lit(lit) => {
                let lit = self.expr_literal(lit, ty, false);

                match lit {
                    ExprKind::LiteralValue(val) => PatternKind::LiteralValue(val),
                    _ => PatternKind::Error(format!("pat lit {:?}", lit)),
                }
            }
            hir::PatKind::Tuple(children, gap_pos) => {
                let tup_size = if let TypeKind::Tuple(children) = ty.kind() {
                    children.len()
                } else {
                    panic!("tuple pattern is not a tuple?");
                };

                let gap_pos = gap_pos.as_opt_usize();

                let fields = children
                    .iter()
                    .enumerate()
                    .map(|(i, child)| {
                        let field = if let Some(gap_pos) = gap_pos {
                            if i >= gap_pos {
                                let back_offset = children.len() - i;
                                (tup_size - back_offset) as u32
                            } else {
                                i as u32
                            }
                        } else {
                            i as u32
                        };

                        FieldPattern {
                            pattern: self.pattern(child),
                            field,
                        }
                    })
                    .collect();

                PatternKind::Struct { fields }
            }
            hir::PatKind::TupleStruct(struct_path, children, gap_pos) => {
                let adt = if let TypeKind::Adt(adt) = ty.kind() {
                    adt
                } else {
                    panic!("struct pattern is not a struct?");
                };
                let adt_info = adt.item.adt_info();

                // It's not easy to get a variant from a ctor so we do some annoying stuff.
                let res = self.types.qpath_res(&struct_path, pat.hir_id);
                let def = res.def_id();
                let ctor_item = self.ctx.vm.types.def_from_rustc(def, &[], &self.ctx).item;

                let (_, variant_index) = ctor_item.ctor_info().unwrap();

                let tup_size = adt_info.variant_fields[variant_index as usize].len();

                let gap_pos = gap_pos.as_opt_usize();

                let fields = children
                    .iter()
                    .enumerate()
                    .map(|(i, child)| {
                        let field = if let Some(gap_pos) = gap_pos {
                            if i >= gap_pos {
                                let back_offset = children.len() - i;
                                (tup_size - back_offset) as u32
                            } else {
                                i as u32
                            }
                        } else {
                            i as u32
                        };

                        FieldPattern {
                            pattern: self.pattern(child),
                            field,
                        }
                    })
                    .collect();

                if adt_info.is_enum() {
                    PatternKind::Enum {
                        fields,
                        variant_index,
                    }
                } else {
                    PatternKind::Struct { fields }
                }
            }
            hir::PatKind::Struct(struct_path, fields, _) => {
                let adt = if let TypeKind::Adt(adt) = ty.kind() {
                    adt
                } else {
                    panic!("struct pattern is not a struct?");
                };
                let adt_info = adt.item.adt_info();

                let fields = fields
                    .iter()
                    .map(|field| {
                        let id = self.types.field_index(field.hir_id).as_u32();
                        FieldPattern {
                            field: id,
                            pattern: self.pattern(field.pat),
                        }
                    })
                    .collect();

                if adt_info.is_enum() {
                    let res = self.types.qpath_res(&struct_path, pat.hir_id);
                    let variant_index = Self::def_variant(&res, rs_ty);

                    PatternKind::Enum {
                        fields,
                        variant_index,
                    }
                } else {
                    PatternKind::Struct { fields }
                }
            }
            hir::PatKind::Path(path) => {
                if let TypeKind::Adt(adt) = ty.kind() {
                    let adt_info = adt.item.adt_info();

                    if adt_info.is_enum() {
                        let res = self.types.qpath_res(&path, pat.hir_id);
                        let def = res.def_id();
                        let ctor_item = self.ctx.vm.types.def_from_rustc(def, &[], &self.ctx).item;

                        let (_, variant_index) = ctor_item.ctor_info().unwrap();

                        PatternKind::Enum {
                            fields: vec![],
                            variant_index,
                        }
                    } else {
                        // nothing to test
                        PatternKind::Hole
                    }
                } else {
                    PatternKind::Error(format!("WARNING path pattern = {:?}", path))
                }
            }
            hir::PatKind::Ref(sub_pattern, _) => {
                let sub_pattern = self.pattern(sub_pattern);
                PatternKind::DeRef { sub_pattern }
            }
            hir::PatKind::Or(options) => {
                let options = options.iter().map(|p| self.pattern(p)).collect();
                PatternKind::Or { options }
            }
            hir::PatKind::Wild => PatternKind::Hole,
            hir::PatKind::Slice(..) => PatternKind::Error("todo slice pattern".to_owned()),
            hir::PatKind::Range(start, end, end_kind) => {
                let start = start.map(|start| self.expr(start));
                let end = end.map(|end| self.expr(end));
                let end_is_inclusive = end_kind == rustc_hir::RangeEnd::Included;

                PatternKind::Range {
                    start,
                    end,
                    end_is_inclusive,
                }
            }
            _ => panic!("pat {:?}", pat.kind),
        };

        let mut res_pat = self.builder.add_pattern(Pattern {
            kind: pattern_kind,
            ty,
        });

        let all_adjust = self.types.pat_adjustments();
        if let Some(adjust) = all_adjust.get(pat.hir_id) {
            for adjust_ty in adjust.iter().rev() {
                let adjust_ty = self.ctx.type_from_rustc(*adjust_ty);
                res_pat = self.builder.add_pattern(Pattern {
                    kind: PatternKind::DeRef {
                        sub_pattern: res_pat,
                    },
                    ty: adjust_ty,
                });
            }
        }

        res_pat
    }

    fn def_variant(res: &hir::def::Res, rs_ty: rustc_middle::ty::Ty) -> u32 {
        use hir::def::{DefKind, Res};
        match res {
            // never refer to a variant
            Res::Def(DefKind::TyAlias, _) | Res::SelfTyAlias { .. } => 0,

            Res::Def(_, def_id) => {
                let rustc_middle::ty::TyKind::Adt(adt_def,_) = rs_ty.kind() else {
                    panic!("attempt to convert struct without adt");
                };
                adt_def.variant_index_with_id(*def_id).as_u32()
            }
            _ => {
                panic!("struct from {:?}", res)
            }
        }
    }
}

/// Iterates through all expressions and replaces captured exprs with UpVar exprs.
fn replace_captures(ir: &mut IRFunction, captures: &[&rustc_middle::ty::CapturedPlace]) {
    for expr_id in ir.iter_expr_ids() {
        for (capture_index, capture) in captures.iter().enumerate() {
            if let Some(upvar) = match_capture(ir, expr_id, capture, capture_index as u32, 0) {
                ir.expr_mut(expr_id).kind = ExprKind::UpVar(upvar);
                // we shouldn't need to consider any other candidates for this expr
                break;
            }
        }
    }
}

fn match_capture(
    ir: &IRFunction,
    expr_id: ExprId,
    capture: &rustc_middle::ty::CapturedPlace,
    capture_index: u32,
    proj_index: usize,
) -> Option<UpVar> {
    let expr = &ir.expr(expr_id).kind;

    if let Some(proj) = capture.place.projections.get(proj_index) {
        use rustc_middle::hir::place::ProjectionKind;
        match proj.kind {
            ProjectionKind::Field(cap_field, cap_variant) => {
                if let ExprKind::Field {
                    lhs,
                    variant,
                    field,
                } = expr
                {
                    if *variant == cap_variant.as_u32() && *field == cap_field.as_u32() {
                        match_capture(ir, *lhs, capture, capture_index, proj_index + 1)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            ProjectionKind::Deref => {
                if let ExprKind::DeRef(arg) = expr {
                    match_capture(ir, *arg, capture, capture_index, proj_index + 1)
                } else {
                    None
                }
            }
            _ => panic!("todo projection {:?}", proj),
        }
    } else {
        assert!(proj_index == capture.place.projections.len());

        if let rustc_middle::hir::place::PlaceBase::Upvar(upvar) = capture.place.base {
            let local_id = upvar.var_path.hir_id.local_id.as_u32();

            if let ExprKind::VarRef(id) = expr {
                if *id == local_id {
                    Some(UpVar {
                        index: capture_index,
                        is_ref: is_capture_by_ref(capture),
                    })
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            panic!("cannot handle capture base {:?}", capture.place.base);
        }
    }
}

fn is_capture_by_ref(capture: &rustc_middle::ty::CapturedPlace) -> bool {
    use rustc_middle::ty::UpvarCapture;
    match capture.info.capture_kind {
        UpvarCapture::ByValue => false,
        UpvarCapture::ByRef(_) => true,
    }
}
