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
    pub params: Vec<Pattern<'vm>>,
    exprs: Vec<Expr<'vm>>,
    stmts: Vec<Stmt<'vm>>,
    blocks: Vec<Block>,
    arms: Vec<MatchArm<'vm>>,
}

#[derive(Debug, Clone, Copy)]
pub struct BlockId(u32);
#[derive(Debug, Clone, Copy)]
pub struct StmtId(u32);
#[derive(Debug, Clone, Copy)]
pub struct ExprId(u32);

#[derive(Debug, Clone, Copy)]
pub struct ArmId(u32);

pub struct Expr<'vm> {
    pub kind: ExprKind<'vm>,
    pub ty: Type<'vm>,
}

#[derive(Debug)]
pub enum Stmt<'vm> {
    Expr(ExprId),
    Let {
        pattern: Pattern<'vm>,
        init: Option<ExprId>,
        else_block: Option<BlockId>,
    },
}

pub struct Block {
    pub stmts: Vec<StmtId>,
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
    pub kind: PatternKind<'vm>,
    pub ty: Type<'vm>,
}

#[derive(Debug)]
pub enum ExprKind<'vm> {
    /// Used to replace some intermediate nodes which aren't super useful to us.
    /// Stores an optional scope ID which is used to resolve breaks
    Dummy(ExprId, Option<u32>),

    /// Used for unsupported expressions, allowing us to serialize all IR
    Error,

    /// Primitive casts
    Cast(ExprId),

    /// Pointer casts
    PointerCast(ExprId, PointerCast),

    Block(BlockId),

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
    Loop(BlockId),
    Break {
        loop_id: ExprId,
        value: Option<ExprId>,
    },
    Continue {
        loop_id: ExprId,
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
        pattern: Pattern<'vm>,
        init: ExprId,
    },
    Match {
        arg: ExprId,
        arms: Vec<ArmId>,
    },
}

#[derive(Debug)]
pub enum PatternKind<'vm> {
    LocalBinding {
        local_id: u32,
        mode: BindingMode,
        sub_pattern: Option<Box<Pattern<'vm>>>,
    },
    Struct {
        fields: Vec<FieldPattern<'vm>>,
    },
    Enum {
        fields: Vec<FieldPattern<'vm>>,
        variant_index: u32,
    },
    Or {
        options: Vec<Pattern<'vm>>,
    },
    DeRef {
        sub_pattern: Box<Pattern<'vm>>,
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
pub struct FieldPattern<'vm> {
    pub field: u32,
    pub pattern: Pattern<'vm>,
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
    // parts of our result function
    exprs: Vec<Expr<'vm>>,
    stmts: Vec<Stmt<'vm>>,
    blocks: Vec<Block>,
}

impl<'vm, 'tcx, 'a> IRFunctionBuilder<'vm, 'tcx> {
    pub fn build(
        ctx: RustCContext<'vm, 'tcx>,
        func_id: LocalDefId,
        body: &rustc_hir::Body,
        types: &'tcx TypeckResults<'tcx>,
    ) -> IRFunction<'vm> {

        assert!(body.params.len() == 0);
        assert!(body.generator_kind.is_none());

        let mut builder = Self {
            ctx,
            func_id,
            types,
            exprs: vec!(),
            stmts: vec!(),
            blocks: vec!()
        };

        let root_expr = builder.expr(body.value);

        // TODO
        let is_constant = false;
        let res_ty = builder.exprs[root_expr.0 as usize].ty;
        let sig = FunctionSig {
            inputs: vec!(),
            output: res_ty
        };

        IRFunction {
            sig,
            is_constant,
            params: vec!(),
            root_expr,
            exprs: builder.exprs,
            stmts: builder.stmts,
            blocks: builder.blocks,
            arms: vec!(),
        }

        /*let root_expr = builder.expr_id(root);

        let arms = thir
            .arms
            .iter()
            .map(|arm| {
                let pattern = builder.pattern(&arm.pattern);
                let expr = builder.expr_id(arm.body);
                let has_guard = arm.guard.is_some();
                MatchArm { pattern, expr, has_guard }
            })
            .collect();

        let exprs = thir.exprs.iter().map(|old| builder.expr(old)).collect();
        let stmts = thir.stmts.iter().map(|old| builder.stmt(old)).collect();
        let blocks = thir
            .blocks
            .iter()
            .map(|old| {
                let stmts = old.stmts.iter().map(|x| builder.stmt_id(*x)).collect();
                let result = old.expr.map(|x| builder.expr_id(x));
                Block { stmts, result }
            })
            .collect();

        let params = thir
            .params
            .iter()
            .map(|param| builder.pattern(param.pat.as_ref().unwrap()))
            .collect();

        let (sig, is_constant) = match thir.body_type {
            thir::BodyTy::Fn(sig) => (FunctionSig::from_rustc(&sig, &builder.ctx), false),
            thir::BodyTy::Const(ty) => {
                let output = builder.ctx.type_from_rustc(ty);
                (
                    FunctionSig {
                        inputs: vec![],
                        output,
                    },
                    true,
                )
            }
        };

        IRFunction {
            sig,
            is_constant,
            params,
            root_expr,
            exprs,
            stmts,
            blocks,
            arms,
        }*/
    }

    fn expr_id(&self, id: thir::ExprId) -> ExprId {
        ExprId(id.as_u32())
    }

    fn stmt_id(&self, id: thir::StmtId) -> StmtId {
        StmtId(id.as_u32())
    }

    fn block_id(&self, id: thir::BlockId) -> BlockId {
        BlockId(id.as_u32())
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
            hir::ExprKind::Unary(op,arg) => {
                let op = match op {
                    hir::UnOp::Not => UnaryOp::Not,
                    hir::UnOp::Neg => UnaryOp::Neg,
                    hir::UnOp::Deref => panic!("deref")
                };
                ExprKind::Unary(op, self.expr(arg))
            }
            hir::ExprKind::Binary(op,lhs,rhs) => {
                let op = self.bin_op(op.node);
                let lhs = self.expr(lhs);
                let rhs = self.expr(rhs);

                ExprKind::Binary(op, lhs, rhs)
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
                let func = self.expr(func);

                let args = args.iter().map(|arg| {
                    self.expr(arg)
                }).collect();

                ExprKind::Call{ func, args }
            }
            hir::ExprKind::If(cond,then,else_opt) => {
                let cond = self.expr(cond);
                let then = self.expr(then);
                let else_opt = else_opt.map(|e| self.expr(e));
                ExprKind::If{ cond, then, else_opt }
            }
            hir::ExprKind::Loop(body,..) => {
                let body = self.block(body);
                ExprKind::Loop(body)
            }
            hir::ExprKind::Path(path) => {
                let res = self.types.qpath_res(&path,expr.hir_id);
                
                if let hir::def::Res::Def(def_kind,did) = res {
                    //let subs = self.types.node_substs(expr.hir_id);
                    //let item_with_subs = self.ctx.vm.types.def_from_rustc(did, subs, &self.ctx);

                    match def_kind {
                        hir::def::DefKind::Fn => {
                            // pull information from the type instead
                            ExprKind::LiteralVoid
                            //ExprKind::Function(item_with_subs)
                        }
                        _ => panic!("def = {:?}",def_kind)
                    }
                } else if let hir::def::Res::Local(hir_id) = res {

                    assert_eq!(hir_id.owner.def_id, self.func_id);
                    let local_id = hir_id.local_id.as_u32();

                    ExprKind::VarRef(local_id)
                } else {
                    panic!("path = {:?}",res);
                }
            }
            hir::ExprKind::Block(block,_) => {
                let block_id = self.block(block);
                ExprKind::Block(block_id)
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
            panic!("todo adjustment");
        }

        expr_id
    }

    fn add_expr(&mut self, e: Expr<'vm>) -> ExprId {
        let expr_id = ExprId(self.exprs.len() as u32);
        self.exprs.push(e);
        expr_id
    }

    fn block(&mut self, block: &hir::Block) -> BlockId {
        let mut stmts = vec!();

        for stmt in block.stmts {
            stmts.push(self.stmt(&stmt));
        }

        let result = block.expr.map(|expr| self.expr(expr));

        let block = Block{
            stmts,
            result
        };

        let block_id = BlockId(self.blocks.len() as u32);
        self.blocks.push(block);
        block_id
    }

    fn stmt(&mut self, stmt: &hir::Stmt) -> StmtId {
        let stmt_kind = match stmt.kind {
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
            _ => panic!("stmt {:?}",stmt)
        };

        let stmt_id = StmtId(self.stmts.len() as u32);
        self.stmts.push(stmt_kind);
        stmt_id
    }

    fn pattern(&self, pat: &hir::Pat) -> Pattern<'vm> {
        let ty = self.types.pat_ty(pat);
        let ty = self.ctx.type_from_rustc(ty);

        let all_adjust = self.types.pat_adjustments();
        if let Some(adjust) = all_adjust.get(pat.hir_id) {
            assert!(adjust.len() == 0);
        }

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

                let sub_pattern = sub_pattern.map(|sub_pat| Box::new(self.pattern(sub_pat)));

                PatternKind::LocalBinding{ local_id, mode, sub_pattern }
            }
            _ => panic!("pat {:?}",pat)
        };

        Pattern{
            kind: pattern_kind,
            ty
        }
    }

    fn expr_old(&self, old: &thir::Expr<'tcx>) -> Expr<'vm> {
        let kind = match old.kind {
            thir::ExprKind::Use { source }
            | thir::ExprKind::NeverToAny { source }
            | thir::ExprKind::ValueTypeAscription { source, .. } => {
                ExprKind::Dummy(self.expr_id(source), None)
            }
            thir::ExprKind::Scope {
                value,
                region_scope,
                ..
            } => ExprKind::Dummy(self.expr_id(value), Some(region_scope.id.as_u32())),
            thir::ExprKind::Cast { source } => ExprKind::Cast(self.expr_id(source)),
            thir::ExprKind::Pointer { cast, source } => {
                use rustc_middle::ty::adjustment::PointerCast as PC;
                let cast = match cast {
                    PC::ReifyFnPointer => PointerCast::ReifyFnPointer,
                    PC::UnsafeFnPointer => PointerCast::UnsafeFnPointer,
                    PC::ClosureFnPointer(_) => PointerCast::ClosureFnPointer,
                    PC::MutToConstPointer => PointerCast::MutToConstPointer,
                    PC::ArrayToPointer => PointerCast::ArrayToPointer,
                    PC::Unsize => PointerCast::UnSize,
                };
                ExprKind::PointerCast(self.expr_id(source), cast)
            }

            thir::ExprKind::Block { block } => ExprKind::Block(self.block_id(block)),

            thir::ExprKind::Literal { lit, neg } => {
                use rustc_ast::ast::LitKind;
                match lit.node {
                    LitKind::Int(n, _) => {
                        let mut n = n as i128;
                        if neg {
                            n = -n;
                        }
                        ExprKind::LiteralValue(n)
                    }
                    LitKind::Float(sym, _) => {
                        let rustc_middle::ty::TyKind::Float(float_ty) = old.ty.kind() else {
                            panic!("non-float float literal");
                        };
                        match float_ty {
                            rustc_middle::ty::FloatTy::F32 => {
                                let mut n = f32::from_str(sym.as_str()).unwrap();
                                if neg {
                                    n = -n;
                                }
                                let x: i32 = unsafe { std::mem::transmute(n) };
                                ExprKind::LiteralValue(x as i128)
                            }
                            rustc_middle::ty::FloatTy::F64 => {
                                let mut n = f64::from_str(sym.as_str()).unwrap();
                                if neg {
                                    n = -n;
                                }
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
            thir::ExprKind::ZstLiteral { .. } => ExprKind::LiteralVoid,
            thir::ExprKind::VarRef { id } => {
                let hir_id = id.0;
                // will probably not hold true for closures
                assert_eq!(hir_id.owner.def_id, self.func_id);

                let local_id = hir_id.local_id.as_u32();
                ExprKind::VarRef(local_id)
            }
            thir::ExprKind::Unary { op, arg } => {
                use rustc_middle::mir::UnOp;
                let op = match op {
                    UnOp::Not => UnaryOp::Not,
                    UnOp::Neg => UnaryOp::Neg,
                };
                ExprKind::Unary(op, self.expr_id(arg))
            }
            thir::ExprKind::LogicalOp { op, lhs, rhs } => {
                use rustc_middle::thir::LogicalOp;
                let op = match op {
                    LogicalOp::And => LogicOp::And,
                    LogicalOp::Or => LogicOp::Or,
                };
                ExprKind::LogicOp(op, self.expr_id(lhs), self.expr_id(rhs))
            }
            /*thir::ExprKind::Binary { op, lhs, rhs } => {
                ExprKind::Binary(self.bin_op(op), self.expr_id(lhs), self.expr_id(rhs))
            }
            thir::ExprKind::AssignOp { op, lhs, rhs } => {
                ExprKind::AssignOp(self.bin_op(op), self.expr_id(lhs), self.expr_id(rhs))
            }*/
            thir::ExprKind::Assign { lhs, rhs } => {
                ExprKind::Assign(self.expr_id(lhs), self.expr_id(rhs))
            }

            thir::ExprKind::Borrow { arg, .. } | thir::ExprKind::AddressOf { arg, .. } => {
                ExprKind::Ref(self.expr_id(arg))
            }
            thir::ExprKind::Deref { arg } => ExprKind::DeRef(self.expr_id(arg)),

            thir::ExprKind::If {
                cond,
                then,
                else_opt,
                ..
            } => ExprKind::If {
                cond: self.expr_id(cond),
                then: self.expr_id(then),
                else_opt: else_opt.map(|e| self.expr_id(e)),
            },
            /*thir::ExprKind::Break { label, value } => ExprKind::Break {
                scope_id: label.id.as_u32(),
                value: value.map(|e| self.expr_id(e)),
            },
            thir::ExprKind::Continue { label } => ExprKind::Continue {
                scope_id: label.id.as_u32(),
            },*/
            thir::ExprKind::Return { value } => {
                let value = value.map(|e| self.expr_id(e));
                ExprKind::Return(value)
            }
            //thir::ExprKind::Loop { body } => ExprKind::Loop(self.expr_id(body)),
            thir::ExprKind::Match {
                scrutinee,
                ref arms,
            } => {
                let arms = arms.iter().map(|arm_id| ArmId(arm_id.as_u32())).collect();

                ExprKind::Match {
                    arg: self.expr_id(scrutinee),
                    arms,
                }
            }

            thir::ExprKind::Call {
                ty, fun, ref args, ..
            } => panic!(),/*ExprKind::Call {
                func_ty: self.ctx.type_from_rustc(ty),
                func_expr: self.expr_id(fun),
                args: args.iter().map(|arg| self.expr_id(*arg)).collect(),
            },*/
            thir::ExprKind::Tuple { ref fields } => {
                let fields = fields.iter().map(|f| self.expr_id(*f)).collect();
                ExprKind::Tuple(fields)
            }
            thir::ExprKind::Adt(ref adt) => {
                let fields = adt
                    .fields
                    .iter()
                    .map(|f| (f.name.as_u32(), self.expr_id(f.expr)))
                    .collect();

                ExprKind::Adt {
                    variant: adt.variant_index.as_u32(),
                    fields,
                }
            }
            thir::ExprKind::Array { ref fields } => {
                let fields = fields.iter().map(|f| self.expr_id(*f)).collect();
                ExprKind::Array(fields)
            }

            // NYI:
            thir::ExprKind::Repeat{..} => {
                ExprKind::Error
            }
            thir::ExprKind::Closure{..} => {
                ExprKind::Error
            }
            thir::ExprKind::StaticRef{..} => {
                ExprKind::Error
            }
            // needs substs -- is it just syntactic sugar for an inline constant?
            thir::ExprKind::ConstBlock{..} => {
                ExprKind::Error
            }

            thir::ExprKind::Field {
                lhs,
                variant_index,
                name,
            } => ExprKind::Field {
                lhs: self.expr_id(lhs),
                variant: variant_index.as_u32(),
                field: name.as_u32(),
            },
            thir::ExprKind::Index { lhs, index } => ExprKind::Index {
                lhs: self.expr_id(lhs),
                index: self.expr_id(index),
            },
            thir::ExprKind::Let { expr, ref pat } => ExprKind::Let {
                pattern: self.pattern_old(pat),
                init: self.expr_id(expr),
            },
            thir::ExprKind::NamedConst { def_id, substs, .. } => {
                let item_ref = self.ctx.vm.types.def_from_rustc(def_id, substs, &self.ctx);
                ExprKind::NamedConst(item_ref)
            }

            _ => panic!("convert {:?}", old.kind),
        };

        Expr {
            kind,
            ty: self.ctx.type_from_rustc(old.ty),
        }
    }

    fn pattern_old(&self, old: &thir::Pat<'tcx>) -> Pattern<'vm> {
        let ty = self.ctx.type_from_rustc(old.ty);

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
        }
    }

    fn stmt_old(&self, old: &thir::Stmt<'tcx>) -> Stmt<'vm> {
        use rustc_middle::thir::StmtKind;
        match old.kind {
            StmtKind::Expr { expr, .. } => Stmt::Expr(self.expr_id(expr)),
            StmtKind::Let {
                ref pattern,
                initializer,
                else_block,
                ..
            } => Stmt::Let {
                pattern: self.pattern_old(pattern),
                init: initializer.map(|x| self.expr_id(x)),
                else_block: else_block.map(|x| self.block_id(x)),
            },
        }
    }
}

impl<'vm> IRFunction<'vm> {
    pub fn expr(&self, id: ExprId) -> &Expr<'vm> {
        &self.exprs[id.0 as usize]
    }

    pub fn stmt(&self, id: StmtId) -> &Stmt<'vm> {
        &self.stmts[id.0 as usize]
    }

    pub fn block(&self, id: BlockId) -> &Block {
        &self.blocks[id.0 as usize]
    }

    pub fn arm(&self, id: ArmId) -> &MatchArm<'vm> {
        &self.arms[id.0 as usize]
    }
}

// used to generate glue code for Fn* traits on regular functions
pub fn glue_ir_for_fn_trait<'vm>(
    func_ty: Type<'vm>,
    self_ty: Type<'vm>,
    args_ty: Type<'vm>,
    res_ty: Type<'vm>,
) -> IRFunction<'vm> {
    assert!(func_ty.is_concrete());
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
    }
}
