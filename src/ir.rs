/// This module contains a re-implementation of the THIR, compatible with our own interned types and suitable for serialization.
use std::str::FromStr;

use rustc_hir::def_id::LocalDefId;
use rustc_middle::thir;
use rustc_hir as hir;
use rustc_middle::ty::TypeckResults;

use crate::items::FunctionSig;
use crate::rustc_worker::RustCContext;
use crate::types::{ItemWithSubs, Type, TypeKind, FloatWidth};

pub struct IRFunction<'vm> {
    pub sig: FunctionSig<'vm>,
    pub is_constant: bool,
    pub root_expr: ExprId,
    pub params: Vec<PatternId>,
    exprs: Vec<Expr<'vm>>,
    patterns: Vec<Pattern<'vm>>,

    //stmts: Vec<Stmt<'vm>>,
    //blocks: Vec<Block>,
    //arms: Vec<MatchArm<'vm>>,
}

#[derive(Debug, Clone, Copy)]
pub struct ExprId(u32);
#[derive(Debug, Clone, Copy)]
pub struct PatternId(u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LoopId(u32);

#[derive(Debug, Clone, Copy)]
pub struct ArmId(u32);

pub struct Expr<'vm> {
    pub kind: ExprKind<'vm>,
    pub ty: Type<'vm>,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprId),
    Let {
        pattern: PatternId,
        init: Option<ExprId>,
        else_block: Option<Block>,
    },
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub result: Option<ExprId>,
}

#[derive(Debug)]
pub struct MatchArm<'vm> {
    pub pattern: Pattern<'vm>,
    pub expr: ExprId,
    pub has_guard: bool
}

#[derive(Debug)]
pub struct Pattern<'vm> {
    pub kind: PatternKind,
    pub ty: Type<'vm>,
}

#[derive(Debug)]
pub enum ExprKind<'vm> {
    /// Used to replace some intermediate nodes which aren't super useful to us.
    Dummy(ExprId),

    /// Used for unsupported expressions, allowing us to serialize all IR
    Error,

    /// Primitive casts
    Cast(ExprId),

    /// Pointer casts
    PointerCast(ExprId, PointerCast),

    Block(Block),

    /// Variable reference
    VarRef(u32),

    /// Small literals: integer, float, char, or bool types
    LiteralValue(i128),
    /// Zero-sized type literals
    LiteralVoid,
    /// Literal used for strings. Can't lower to a pointer at this stage because it would break serialization.
    LiteralBytes(&'vm [u8]),

    Unary(UnaryOp, ExprId),
    Binary(BinaryOp, ExprId, ExprId),
    AssignOp(BinaryOp, ExprId, ExprId),
    Assign(ExprId, ExprId),
    LogicOp(LogicOp, ExprId, ExprId),

    If {
        cond: ExprId,
        then: ExprId,
        else_opt: Option<ExprId>,
    },
    Loop(Block,LoopId),
    Break {
        loop_id: LoopId,
        value: Option<ExprId>,
    },
    Continue {
        loop_id: LoopId,
    },
    Return(Option<ExprId>),

    Call {
        func: ExprId,
        args: Vec<ExprId>,
    },
    Tuple(Vec<ExprId>),
    Adt {
        variant: u32,
        fields: Vec<(u32, ExprId)>,
    },
    Array(Vec<ExprId>),

    NamedConst(ItemWithSubs<'vm>),
    //Function(ItemWithSubs<'vm>),

    Ref(ExprId),
    DeRef(ExprId),

    Field {
        lhs: ExprId,
        variant: u32,
        field: u32,
    },
    Index {
        lhs: ExprId,
        index: ExprId,
    },

    Let {
        pattern: PatternId,
        init: ExprId,
    },
    Match {
        arg: ExprId,
        arms: Vec<ArmId>,
    },
}

#[derive(Debug)]
pub enum PatternKind {
    LocalBinding {
        local_id: u32,
        mode: BindingMode,
        sub_pattern: Option<PatternId>,
    },
    Struct {
        fields: Vec<FieldPattern>,
    },
    Enum {
        fields: Vec<FieldPattern>,
        variant_index: u32,
    },
    Or {
        options: Vec<PatternId>,
    },
    DeRef {
        sub_pattern: PatternId,
    },
    /// Small literals: integer, float, char, or bool types (same as the ExprKind)
    LiteralValue(i128),

    Hole,

    /// Error, for unsupported patterns
    Error
}

#[derive(Debug)]
pub enum BindingMode {
    Value,
    Ref,
}

#[derive(Debug)]
pub struct FieldPattern {
    pub field: u32,
    pub pattern: PatternId,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    BitAnd,
    BitOr,
    BitXor,
    ShiftR,
    ShiftL,

    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
}

#[derive(Debug)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Debug)]
pub enum PointerCast {
    ReifyFnPointer,
    UnsafeFnPointer,
    ClosureFnPointer,
    MutToConstPointer,
    ArrayToPointer,
    UnSize,
}

/// Converts rust IR to skitter IR.
/// 
/// Now uses TypeckResults instead of THIR because THIR can get randomly stolen when querying other THIR.
pub struct IRFunctionBuilder<'vm, 'tcx> {
    ctx: RustCContext<'vm, 'tcx>,
    func_id: LocalDefId,
    types: &'tcx TypeckResults<'tcx>,
    loops: Vec<(hir::HirId,LoopId)>,
    // parts of our result function
    exprs: Vec<Expr<'vm>>,
    patterns: Vec<Pattern<'vm>>,
}

impl<'vm, 'tcx, 'a> IRFunctionBuilder<'vm, 'tcx> {
    pub fn build(
        ctx: RustCContext<'vm, 'tcx>,
        func_id: LocalDefId,
        body: &rustc_hir::Body,
        types: &'tcx TypeckResults<'tcx>,
        is_constant: bool
    ) -> IRFunction<'vm> {

        assert!(body.generator_kind.is_none());
        
        let mut builder = Self {
            ctx,
            func_id,
            types,
            loops: vec!(),
            exprs: vec!(),
            patterns: vec!(),
        };
        
        let params: Vec<_> = body.params.iter().map(|param| builder.pattern(param.pat)).collect();

        let root_expr = builder.expr(body.value);

        let output = builder.exprs[root_expr.0 as usize].ty;
        let inputs = params.iter().map(|p| {
            builder.patterns[p.0 as usize].ty
        }).collect();

        let sig = FunctionSig {
            inputs,
            output
        };

        IRFunction {
            sig,
            is_constant,
            params,
            root_expr,
            exprs: builder.exprs,
            patterns: builder.patterns
        }
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
            _ => panic!()
        }
    }

    fn expr(&mut self, expr: &hir::Expr) -> ExprId {
        let ty = self.types.expr_ty(expr);
        let ty = self.ctx.type_from_rustc(ty);

        let expr_kind = match expr.kind {
            hir::ExprKind::Lit(lit) => {
                use rustc_ast::ast::LitKind;
                match lit.node {
                    LitKind::Int(n, _) => {
                        let n = n as i128;
                        ExprKind::LiteralValue(n)
                    }
                    LitKind::Float(sym, _) => {
                        let TypeKind::Float(float_width) = ty.kind() else {
                            panic!("non-float float literal");
                        };
                        match float_width {
                            FloatWidth::F32 => {
                                let n = f32::from_str(sym.as_str()).unwrap();
                                let x: i32 = unsafe { std::mem::transmute(n) };
                                ExprKind::LiteralValue(x as i128)
                            }
                            FloatWidth::F64 => {
                                let n = f64::from_str(sym.as_str()).unwrap();
                                let x: i64 = unsafe { std::mem::transmute(n) };
                                ExprKind::LiteralValue(x as i128)
                            }
                        }
                    }
                    LitKind::Bool(b) => ExprKind::LiteralValue(if b { 1 } else { 0 }),
                    LitKind::Char(c) => ExprKind::LiteralValue(c as i128),
                    LitKind::Byte(b) => {
                        ExprKind::LiteralValue(b as i128)
                    }
                    LitKind::Str(sym, _) => {
                        let bytes = sym.as_str().as_bytes().to_owned();
                        ExprKind::LiteralBytes(self.ctx.vm.alloc_constant(bytes))
                    }
                    LitKind::ByteStr(ref bytes,_) => {
                        let bytes = bytes.iter().copied().collect();
                        ExprKind::LiteralBytes(self.ctx.vm.alloc_constant(bytes))
                    }
                    _ => panic!("lit other {:?}", lit.node),
                }
            }
            hir::ExprKind::Cast(arg,_) => {
                ExprKind::Cast(self.expr(arg))
            }
            hir::ExprKind::AddrOf(_,_,arg) => {
                ExprKind::Ref(self.expr(arg))
            }
            hir::ExprKind::Unary(op,arg) => {
                if let hir::UnOp::Deref = op {
                    ExprKind::DeRef(self.expr(arg))
                } else {
                    let op = match op {
                        hir::UnOp::Not => UnaryOp::Not,
                        hir::UnOp::Neg => UnaryOp::Neg,
                        _ => panic!() 
                    };

                    ExprKind::Unary(op, self.expr(arg))
                }
            }
            hir::ExprKind::Binary(op,lhs,rhs) => {
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                match op.node {
                    hir::BinOpKind::And => {
                        ExprKind::LogicOp(LogicOp::And, lhs, rhs)
                    },
                    hir::BinOpKind::Or => {
                        ExprKind::LogicOp(LogicOp::Or, lhs, rhs)
                    },
                    _ => {
                        if let Some(func_did) = self.types.type_dependent_def_id(expr.hir_id) {
                            let subs = self.types.node_substs(expr.hir_id);
                            let func_item = self.ctx.vm.types.def_from_rustc(func_did, subs, &self.ctx);
                            let func_ty = self.ctx.vm.ty_func_def(func_item);

                            let func = self.add_expr(Expr{
                                kind: ExprKind::LiteralVoid,
                                ty: func_ty
                            });

                            ExprKind::Call{ func, args: vec!(lhs,rhs) }
                        } else {
                            let op = self.bin_op(op.node);

                            ExprKind::Binary(op, lhs, rhs)
                        }
                    }
                }
            }
            hir::ExprKind::AssignOp(op,lhs,rhs) => {
                let op = self.bin_op(op.node);
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                ExprKind::AssignOp(op, lhs, rhs)
            }
            hir::ExprKind::Assign(lhs,rhs,_) => {
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                ExprKind::Assign(lhs, rhs)
            }
            hir::ExprKind::Call(func,args) => {
                if let Some(func_did) = self.types.type_dependent_def_id(expr.hir_id) {
                    panic!("{:?}",func_did)
                } else {
                    let func = self.expr(func);
    
                    let args = args.iter().map(|arg| {
                        self.expr(arg)
                    }).collect();
    
                    ExprKind::Call{ func, args }
                }
            }
            hir::ExprKind::MethodCall(_,lhs,args,_) => {
                // We need to pull the actual method def from typeck results.
                let method_did = self.types.type_dependent_def_id(expr.hir_id).unwrap();
                let method_subs = self.types.node_substs(expr.hir_id);
                let method_item = self.ctx.vm.types.def_from_rustc(method_did, method_subs, &self.ctx);
                let method_ty = self.ctx.vm.ty_func_def(method_item);

                let lhs = self.expr(lhs);
                let args = args.iter().map(|arg| self.expr(arg));

                let full_args = std::iter::once(lhs).chain(args).collect();

                let func = self.add_expr(Expr{
                    kind: ExprKind::LiteralVoid,
                    ty: method_ty
                });

                ExprKind::Call{ func, args: full_args }
            }
            hir::ExprKind::If(cond,then,else_opt) => {
                let cond = self.expr(cond);
                let then = self.expr(then);
                let else_opt = else_opt.map(|e| self.expr(e));
                ExprKind::If{ cond, then, else_opt }
            }
            hir::ExprKind::Loop(body,..) => {
                let loop_id = LoopId(self.loops.len() as u32);
                self.loops.push((expr.hir_id,loop_id));

                let body = self.block(body);
                ExprKind::Loop(body,loop_id)
            }
            hir::ExprKind::Break(dest,value) => {
                let target = dest.target_id.unwrap();
                let (_,loop_id) = self.loops.iter().find(|(h,_)| h == &target).copied().unwrap();

                let value = value.map(|e| self.expr(e));

                ExprKind::Break { loop_id, value }
            }
            hir::ExprKind::Continue(dest) => {
                let target = dest.target_id.unwrap();
                let (_,loop_id) = self.loops.iter().find(|(h,_)| h == &target).copied().unwrap();

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
            hir::ExprKind::Struct(path,fields,rest) => {
                assert!(rest.is_none());
                let res = self.types.qpath_res(&path,expr.hir_id);

                match res {
                    hir::def::Res::Def(hir::def::DefKind::Struct,_) |
                    hir::def::Res::Def(hir::def::DefKind::TyAlias,_) |
                    hir::def::Res::SelfTyAlias{..} => {
                        // structs are okay!
                    }
                    _ => panic!("struct? {:?}",res)
                }

                let variant = 0;

                let fields = fields.iter().map(|field| {
                    let id = self.types.field_index(field.hir_id).as_u32();
                    let expr = self.expr(field.expr);
                    (id,expr)
                }).collect();

                ExprKind::Adt{
                    variant,
                    fields
                }
            }
            hir::ExprKind::Array(args) => {
                let args = args.iter().map(|a| self.expr(a)).collect();
                ExprKind::Array(args)
            }
            hir::ExprKind::Index(lhs,index) => {
                let lhs = self.expr(lhs);
                let index = self.expr(index);
                ExprKind::Index{ lhs, index }
            }
            hir::ExprKind::Field(lhs,field) => {
                let index = self.types.field_index(expr.hir_id);
                let lhs = self.expr(lhs);
                ExprKind::Field{ lhs, variant: 0, field: index.as_u32() }
            }
            hir::ExprKind::Path(path) => {
                let res = self.types.qpath_res(&path,expr.hir_id);
                
                match res {
                    hir::def::Res::Def(def_kind,did) => {
                        match def_kind {
                            hir::def::DefKind::Fn |
                            hir::def::DefKind::AssocFn |
                            hir::def::DefKind::Ctor(..) => {
                                // return void expression, any information needed
                                // can be pulled from the type
                                ExprKind::LiteralVoid
                            }
                            hir::def::DefKind::Const |
                            hir::def::DefKind::AssocConst => {
                                let subs = self.types.node_substs(expr.hir_id);
                                let const_item = self.ctx.vm.types.def_from_rustc(did, subs, &self.ctx);
                                ExprKind::NamedConst(const_item)
                            }
                            _ => panic!("def = {:?}",def_kind)
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
                    _ => panic!("path = {:?}",res)
                }
            }
            hir::ExprKind::Let(let_expr) => {
                ExprKind::Let {
                    pattern: self.pattern(let_expr.pat),
                    init: self.expr(let_expr.init),
                }
            }
            hir::ExprKind::Block(block,_) => {
                let block = self.block(block);
                ExprKind::Block(block)
            }
            hir::ExprKind::DropTemps(e) => {
                // TODO TODO TODO
                // this may pose an issue when implementing destructors / drop
                return self.expr(e);
            }
            _ => panic!("todo expr kind {:?}",expr.kind)
        };

        let mut expr_id = self.add_expr(Expr{
            kind: expr_kind,
            ty
        });

        let adjust_list = self.types.expr_adjustments(expr);
        for adjust in adjust_list {
            let adjust_ty = self.ctx.type_from_rustc(adjust.target);

            use rustc_middle::ty::adjustment::Adjust;
            match adjust.kind {
                Adjust::NeverToAny => {
                    expr_id = self.add_expr(Expr{
                        kind: ExprKind::Dummy(expr_id),
                        ty: adjust_ty
                    });
                }
                Adjust::Deref(overloaded) => {
                    assert!(overloaded.is_none());
                    expr_id = self.add_expr(Expr{
                        kind: ExprKind::DeRef(expr_id),
                        ty: adjust_ty
                    });
                }
                Adjust::Borrow(_) => {
                    expr_id = self.add_expr(Expr{
                        kind: ExprKind::Ref(expr_id),
                        ty: adjust_ty
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

                    expr_id = self.add_expr(Expr{
                        kind: ExprKind::PointerCast(expr_id,ptr_cast),
                        ty: adjust_ty
                    });
                }
                _ => panic!("todo adjust {:?}",adjust)
            }
        }

        expr_id
    }

    fn add_expr(&mut self, e: Expr<'vm>) -> ExprId {
        let expr_id = ExprId(self.exprs.len() as u32);
        self.exprs.push(e);
        expr_id
    }

    fn add_pattern(&mut self, p: Pattern<'vm>) -> PatternId {
        let pat_id = PatternId(self.patterns.len() as u32);
        self.patterns.push(p);
        pat_id
    }

    fn block(&mut self, block: &hir::Block) -> Block {
        let mut stmts = vec!();

        for stmt in block.stmts {
            if let Some(stmt) = self.stmt(&stmt) {
                stmts.push(stmt);
            }
        }

        let result = block.expr.map(|expr| self.expr(expr));

        Block{
            stmts,
            result
        }
    }

    fn stmt(&mut self, stmt: &hir::Stmt) -> Option<Stmt> {
        let res = match stmt.kind {
            hir::StmtKind::Expr(expr) |
            hir::StmtKind::Semi(expr) => {
                Stmt::Expr(self.expr(&expr))
            }
            hir::StmtKind::Local(local) => {

                let pattern = self.pattern(local.pat);
                let init = local.init.map(|init| self.expr(init));
                let else_block = local.els.map(|else_block| self.block(else_block));

                Stmt::Let{ pattern, init, else_block }
            }
            hir::StmtKind::Item(_) => return None,
        };

        Some(res)
    }

    fn pattern(&mut self, pat: &hir::Pat) -> PatternId {
        let ty = self.types.pat_ty(pat);
        let ty = self.ctx.type_from_rustc(ty);

        let pattern_kind = match pat.kind {
            hir::PatKind::Binding(_,hir_id,_,sub_pattern) => {
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

                PatternKind::LocalBinding{ local_id, mode, sub_pattern }
            }
            hir::PatKind::Lit(lit) => {
                if let hir::ExprKind::Lit(lit) = lit.kind {
                    use rustc_ast::ast::LitKind;
                    match lit.node {
                        LitKind::Int(n, _) => {
                            let n = n as i128;
                            PatternKind::LiteralValue(n)
                        }
                        _ => panic!("pat lit kind {:?}",lit.node)
                    }
                } else {
                    panic!("pat lit {:?}",lit);
                }
            }
            hir::PatKind::Tuple(children,gap_pos) => {
                let tup_size = if let TypeKind::Tuple(children) = ty.kind() {
                    children.len()
                } else {
                    panic!("tuple pattern is not a tuple?");
                };

                let gap_pos = gap_pos.as_opt_usize();

                let fields = children.iter().enumerate().map(|(i,child)| {

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

                    FieldPattern{
                        pattern: self.pattern(child),
                        field
                    }
                }).collect();

                PatternKind::Struct { fields }
            }
            hir::PatKind::Ref(sub_pattern,_) => {
                let sub_pattern = self.pattern(sub_pattern);
                PatternKind::DeRef{ sub_pattern }
            }
            hir::PatKind::Or(options) => {
                let options = options.iter().map(|p| self.pattern(p)).collect();
                PatternKind::Or{ options }
            }
            hir::PatKind::Wild => {
                PatternKind::Hole
            }
            _ => panic!("pat {:?}",pat.kind)
        };

        let mut res_pat = self.add_pattern(Pattern{
            kind: pattern_kind,
            ty
        });

        let all_adjust = self.types.pat_adjustments();
        if let Some(adjust) = all_adjust.get(pat.hir_id) {
            for adjust_ty in adjust.iter().rev() {
                let adjust_ty = self.ctx.type_from_rustc(*adjust_ty);
                res_pat = self.add_pattern(Pattern{
                    kind: PatternKind::DeRef{
                        sub_pattern: res_pat
                    },
                    ty: adjust_ty
                });
            }
        }

        res_pat
    }

    fn pattern_old(&self, old: &thir::Pat<'tcx>) -> Pattern<'vm> {
        panic!();
        /*let ty = self.ctx.type_from_rustc(old.ty);

        let kind = match old.kind {
            thir::PatKind::AscribeUserType { ref subpattern, .. } => {
                return self.pattern_old(subpattern);
            }
            thir::PatKind::Binding {
                ref subpattern,
                ref var,
                mode,
                ..
            } => {
                let mode = match mode {
                    rustc_middle::thir::BindingMode::ByValue => BindingMode::Value,
                    rustc_middle::thir::BindingMode::ByRef(_) => BindingMode::Ref,
                };
                let hir_id = var.0;
                // should hold true, even for closures? when are we ever going
                // to bind a variable that doesn't belong to our current function?
                assert_eq!(hir_id.owner.def_id, self.func_id);

                let local_id = hir_id.local_id.as_u32();
                PatternKind::LocalBinding {
                    local_id,
                    mode,
                    sub_pattern: subpattern.as_ref().map(|pat| Box::new(self.pattern_old(&pat))),
                }
            }
            thir::PatKind::Leaf { ref subpatterns } => {
                let fields: Vec<_> = subpatterns
                    .iter()
                    .map(|child| FieldPattern {
                        field: child.field.as_u32(),
                        pattern: self.pattern_old(&child.pattern),
                    })
                    .collect();
                PatternKind::Struct { fields }
            }
            thir::PatKind::Variant {
                ref subpatterns,
                variant_index,
                ..
            } => {
                let fields: Vec<_> = subpatterns
                    .iter()
                    .map(|child| FieldPattern {
                        field: child.field.as_u32(),
                        pattern: self.pattern_old(&child.pattern),
                    })
                    .collect();
                PatternKind::Enum {
                    fields,
                    variant_index: variant_index.as_u32(),
                }
            }
            thir::PatKind::Or { ref pats } => {
                let options = pats.iter().map(|child| self.pattern_old(child)).collect();
                PatternKind::Or { options }
            }
            thir::PatKind::Deref { ref subpattern } => PatternKind::DeRef {
                sub_pattern: Box::new(self.pattern_old(subpattern)),
            },
            thir::PatKind::Constant { ref value } => match ty.kind() {
                TypeKind::Int(..) | TypeKind::Char | TypeKind::Bool => {
                    let size = rustc_abi::Size::from_bytes(ty.layout().assert_size());
                    let bits = value.try_to_bits(size).expect("failed to get const bits");
                    PatternKind::LiteralValue(bits as i128)
                }
                _ => panic!("const kind"),
            },
            thir::PatKind::Slice{..} => PatternKind::Error,
            thir::PatKind::Range{..} => PatternKind::Error,
            thir::PatKind::Wild => PatternKind::Hole,
            _ => panic!("pattern kind {:?}", old.kind),
        };

        Pattern {
            kind,
            ty: self.ctx.type_from_rustc(old.ty),
        }*/
    }
}

impl<'vm> IRFunction<'vm> {
    pub fn expr(&self, id: ExprId) -> &Expr<'vm> {
        &self.exprs[id.0 as usize]
    }

    pub fn pattern(&self, id: PatternId) -> &Pattern<'vm> {
        &self.patterns[id.0 as usize]
    }
}

// used to generate glue code for Fn* traits on regular functions
pub fn glue_ir_for_fn_trait<'vm>(
    func_ty: Type<'vm>,
    self_ty: Type<'vm>,
    args_ty: Type<'vm>,
    res_ty: Type<'vm>,
) -> IRFunction<'vm> {
    panic!("fixme");
    /*assert!(func_ty.is_concrete());
    assert!(args_ty.is_concrete());

    let sig = FunctionSig {
        inputs: vec![self_ty, args_ty],
        output: res_ty,
    };

    let params = sig
        .inputs
        .iter()
        .enumerate()
        .map(|(i, ty)| Pattern {
            kind: PatternKind::LocalBinding {
                local_id: i as u32,
                mode: BindingMode::Value,
                sub_pattern: None,
            },
            ty: *ty,
        })
        .collect();

    let TypeKind::Tuple(inner_arg_tys) = args_ty.kind() else {
        panic!("fn trait args must be a tuple");
    };

    // build the actual ir
    let mut exprs = Vec::new();

    let func_expr = ExprId(0);
    exprs.push(Expr {
        kind: ExprKind::LiteralVoid,
        ty: func_ty,
    });

    let tuple_expr = ExprId(1);
    exprs.push(Expr {
        kind: ExprKind::VarRef(1),
        ty: args_ty,
    });

    let mut call_args = Vec::new();
    for (i, arg_ty) in inner_arg_tys.iter().enumerate() {
        call_args.push(ExprId(2 + i as u32));
        exprs.push(Expr {
            kind: ExprKind::Field {
                lhs: tuple_expr,
                variant: 0,
                field: i as u32,
            },
            ty: *arg_ty,
        });
    }

    let root_expr = ExprId(exprs.len() as u32);
    exprs.push(Expr {
        kind: ExprKind::Call {
            func: func_expr,
            args: call_args,
        },
        ty: res_ty,
    });

    IRFunction {
        sig,
        is_constant: false,
        params,
        exprs,
        root_expr,

        arms: vec![],
        blocks: vec![],
        stmts: vec![],
    }*/
}
