/// This module contains a re-implementation of the THIR, compatible with our own interned types and suitable for serialization.

use std::str::FromStr;

use rustc_middle::thir;
use rustc_middle::thir::Thir;
use rustc_hir::def_id::LocalDefId;

use crate::rustc_worker::RustCContext;
use crate::types::{Type};

pub struct IRFunction<'vm> {
    pub root_expr: ExprId,
    pub params: Vec<Pattern<'vm>>,
    pub return_ty: Type<'vm>,
    exprs: Vec<Expr<'vm>>,
    stmts: Vec<Stmt<'vm>>,
    blocks: Vec<Block>
}

#[derive(Debug,Clone,Copy)]
pub struct BlockId(u32);
#[derive(Debug,Clone,Copy)]
pub struct StmtId(u32);
#[derive(Debug,Clone,Copy)]
pub struct ExprId(u32);

pub struct Expr<'vm> {
    pub kind: ExprKind<'vm>,
    pub ty: Type<'vm>
}

pub enum Stmt<'vm> {
    Expr(ExprId),
    Let{
        pattern: Pattern<'vm>,
        init: Option<ExprId>,
        else_block: Option<BlockId>
    }
}

pub struct Block {
    pub stmts: Vec<StmtId>,
    pub result: Option<ExprId>,
}

#[derive(Debug)]
pub struct Pattern<'vm> {
    pub kind: PatternKind<'vm>,
    pub ty: Type<'vm>
}

#[derive(Debug)]
pub enum ExprKind<'vm> {
    /// Used to replace some intermediate nodes which aren't super useful to us.
    /// Stores an optional scope ID which is used to resolve breaks
    Dummy(ExprId,Option<u32>),

    /// Primitive casts
    Cast(ExprId),

    /// Pointer casts
    PointerCast(ExprId,PointerCast),

    Block(BlockId),

    /// Variable reference
    VarRef(u32),

    /// Small literals: integer, float, char, or bool types
    LiteralValue(i128),
    /// Zero-sized type literals
    LiteralVoid,

    Unary(UnaryOp,ExprId),
    Binary(BinaryOp,ExprId,ExprId),
    AssignOp(BinaryOp,ExprId,ExprId),
    Assign(ExprId,ExprId),
    LogicOp(LogicOp,ExprId,ExprId),

    If{cond: ExprId, then: ExprId, else_opt: Option<ExprId>},
    Loop(ExprId),
    Break{scope_id: u32, value: Option<ExprId>},
    Continue{scope_id: u32},
    Return(Option<ExprId>),

    Call{func_ty: Type<'vm>, func_expr: ExprId, args: Vec<ExprId>},
    Tuple(Vec<ExprId>),
    Adt{variant: u32, fields: Vec<(u32,ExprId)> },
    Array(Vec<ExprId>),

    Ref(ExprId),
    DeRef(ExprId),

    Field{ lhs: ExprId, variant: u32, field: u32 },
    Index{ lhs: ExprId, index: ExprId },
}

#[derive(Debug)]
pub enum PatternKind<'vm> {
    LocalBinding(u32,BindingMode),
    Struct{
        fields: Vec<FieldPattern<'vm>>
    },
    Hole
}

#[derive(Debug)]
pub enum BindingMode {
    Value,
    Ref
}

#[derive(Debug)]
pub struct FieldPattern<'vm> {
    pub field: u32,
    pub pattern: Pattern<'vm>
}

#[derive(Debug,Clone,Copy)]
pub enum UnaryOp {
    Neg,
    Not
}

#[derive(Debug,Clone,Copy)]
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
    GtEq
}

#[derive(Debug)]
pub enum LogicOp {
    And,
    Or
}

#[derive(Debug)]
pub enum PointerCast{
    ReifyFnPointer,
    UnsafeFnPointer,
    ClosureFnPointer,
    MutToConstPointer,
    ArrayToPointer,
    UnSize,
}

pub struct IRFunctionBuilder<'vm,'tcx> {
    ctx: RustCContext<'vm,'tcx>,
    func_id: LocalDefId
}

impl<'vm,'tcx,'a> IRFunctionBuilder<'vm,'tcx> {
    pub fn build(ctx: RustCContext<'vm,'tcx>, func_id: LocalDefId, root: thir::ExprId, thir: &Thir<'tcx>) -> IRFunction<'vm> {
        if ctx.vm.is_verbose {
            println!("converting ir for {:?}",func_id);
        }

        let builder = Self{
            ctx,
            func_id
        };

        let root_expr = builder.expr_id(root);

        let exprs = thir.exprs.iter().map(|old| builder.expr(old)).collect();
        let stmts = thir.stmts.iter().map(|old| builder.stmt(old)).collect();
        let blocks = thir.blocks.iter().map(|old| {
            let stmts = old.stmts.iter().map(|x| builder.stmt_id(*x)).collect();
            let result = old.expr.map(|x| builder.expr_id(x));
            Block {
                stmts,
                result
            }
        }).collect();

        let params = thir.params.iter().map(|param| {
            builder.pattern(param.pat.as_ref().unwrap())
        }).collect();

        let return_ty = match thir.body_type {
            thir::BodyTy::Fn(sig) => {
                sig.output()
            }
            _ => panic!("const not supported")
        };

        let return_ty = builder.ctx.type_from_rustc(return_ty);

        IRFunction{
            params,
            return_ty,
            root_expr,
            exprs,
            stmts,
            blocks
        }
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

    fn bin_op(&self, op: rustc_middle::mir::BinOp) -> BinaryOp {
        use rustc_middle::mir::BinOp;
        match op {
            BinOp::Add => BinaryOp::Add,
            BinOp::Sub => BinaryOp::Sub,
            BinOp::Mul => BinaryOp::Mul,
            BinOp::Div => BinaryOp::Div,
            BinOp::Rem => BinaryOp::Rem,

            BinOp::BitAnd => BinaryOp::BitAnd,
            BinOp::BitOr => BinaryOp::BitOr,
            BinOp::BitXor => BinaryOp::BitXor,
            BinOp::Shr => BinaryOp::ShiftR,
            BinOp::Shl => BinaryOp::ShiftL,

            BinOp::Eq => BinaryOp::Eq,
            BinOp::Ne => BinaryOp::NotEq,
            BinOp::Lt => BinaryOp::Lt,
            BinOp::Gt => BinaryOp::Gt,
            BinOp::Le => BinaryOp::LtEq,
            BinOp::Ge => BinaryOp::GtEq,

            BinOp::Offset => panic!("todo offset")
        }
    }

    fn expr(&self, old: &thir::Expr<'tcx>) -> Expr<'vm> {
        let kind = match old.kind {
            thir::ExprKind::Use{source} |
            thir::ExprKind::NeverToAny{source} |
            thir::ExprKind::ValueTypeAscription{source,..} => {
                ExprKind::Dummy(self.expr_id(source),None)
            }
            thir::ExprKind::Scope{value,region_scope,..} => {
                ExprKind::Dummy(self.expr_id(value),Some(region_scope.id.as_u32()))
            }
            thir::ExprKind::Cast{source} => {
                ExprKind::Cast(self.expr_id(source))
            }
            thir::ExprKind::Pointer{cast,source} => {
                use rustc_middle::ty::adjustment::PointerCast as PC;
                let cast = match cast {
                    PC::ReifyFnPointer => PointerCast::ReifyFnPointer,
                    PC::UnsafeFnPointer => PointerCast::UnsafeFnPointer,
                    PC::ClosureFnPointer(_) => PointerCast::ClosureFnPointer,
                    PC::MutToConstPointer => PointerCast::MutToConstPointer,
                    PC::ArrayToPointer => PointerCast::ArrayToPointer,
                    PC::Unsize => PointerCast::UnSize,
                };
                ExprKind::PointerCast(self.expr_id(source),cast)
            }

            thir::ExprKind::Block{block} => {
                ExprKind::Block(self.block_id(block))
            }

            thir::ExprKind::Literal{lit,neg} => {
                use rustc_ast::ast::LitKind;
                match lit.node {
                    LitKind::Int(n,_) => {
                        let mut n = n as i128;
                        if neg {
                            n = -n;
                        }
                        ExprKind::LiteralValue(n)
                    }
                    LitKind::Float(sym,_) => {
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
                    LitKind::Bool(b) => {
                        ExprKind::LiteralValue(if b { 1 } else { 0 })
                    }
                    LitKind::Char(c) => {
                        ExprKind::LiteralValue(c as i128)
                    }
                    _ => panic!("lit other {:?}",lit.node)
                }
            }
            thir::ExprKind::ZstLiteral{..} => {
                ExprKind::LiteralVoid
            }
            thir::ExprKind::VarRef{id} => {
                let hir_id = id.0;
                // will probably not hold true for closures
                assert_eq!(hir_id.owner.def_id,self.func_id);

                let local_id = hir_id.local_id.as_u32();
                ExprKind::VarRef(local_id)
            }
            thir::ExprKind::Unary{op,arg} => {
                use rustc_middle::mir::UnOp;
                let op = match op {
                    UnOp::Not => UnaryOp::Not,
                    UnOp::Neg => UnaryOp::Neg,
                };
                ExprKind::Unary(op, self.expr_id(arg))
            }
            thir::ExprKind::LogicalOp{op,lhs,rhs} => {
                use rustc_middle::thir::LogicalOp;
                let op = match op {
                    LogicalOp::And => LogicOp::And,
                    LogicalOp::Or => LogicOp::Or,
                };
                ExprKind::LogicOp(op, self.expr_id(lhs), self.expr_id(rhs))
            }
            thir::ExprKind::Binary{op,lhs,rhs} => {
                ExprKind::Binary(self.bin_op(op), self.expr_id(lhs), self.expr_id(rhs))
            }
            thir::ExprKind::AssignOp{op,lhs,rhs} => {
                ExprKind::AssignOp(self.bin_op(op), self.expr_id(lhs), self.expr_id(rhs))
            }
            thir::ExprKind::Assign{lhs,rhs} => {
                ExprKind::Assign(self.expr_id(lhs), self.expr_id(rhs))
            }

            thir::ExprKind::Borrow{arg,..} |
            thir::ExprKind::AddressOf{arg,..} => {
                ExprKind::Ref(self.expr_id(arg))
            }
            thir::ExprKind::Deref{arg} => {
                ExprKind::DeRef(self.expr_id(arg))
            }

            thir::ExprKind::If{cond,then,else_opt,..} => {
                ExprKind::If{
                    cond: self.expr_id(cond),
                    then: self.expr_id(then),
                    else_opt: else_opt.map(|e| self.expr_id(e))
                }
            }
            thir::ExprKind::Break{label,value} => {
                ExprKind::Break{
                    scope_id: label.id.as_u32(),
                    value: value.map(|e| self.expr_id(e))
                }
            }
            thir::ExprKind::Continue{label} => {
                ExprKind::Continue{
                    scope_id: label.id.as_u32()
                }
            }
            thir::ExprKind::Return{value} => {
                let value = value.map(|e| self.expr_id(e));
                ExprKind::Return(value)
            }
            thir::ExprKind::Loop{body} => {
                ExprKind::Loop(self.expr_id(body))
            }

            thir::ExprKind::Call{ty,fun,ref args,..} => {
                ExprKind::Call{
                    func_ty: self.ctx.type_from_rustc(ty),
                    func_expr: self.expr_id(fun),
                    args: args.iter().map(|arg| self.expr_id(*arg)).collect()
                }
            }
            thir::ExprKind::Tuple{ref fields} => {
                let fields = fields.iter().map(|f| self.expr_id(*f)).collect();
                ExprKind::Tuple(fields)
            }
            thir::ExprKind::Adt(ref adt) => {
                let fields = adt.fields.iter().map(|f| {
                    (
                        f.name.as_u32(),
                        self.expr_id(f.expr)
                    )
                }).collect();

                ExprKind::Adt{
                    variant: adt.variant_index.as_u32(),
                    fields
                }
            }
            thir::ExprKind::Array{ref fields} => {
                let fields = fields.iter().map(|f| self.expr_id(*f)).collect();
                ExprKind::Array(fields)
            }

            thir::ExprKind::Field{lhs,variant_index,name} => {

                ExprKind::Field{
                    lhs: self.expr_id(lhs),
                    variant: variant_index.as_u32(),
                    field: name.as_u32()
                }
            }
            thir::ExprKind::Index{lhs,index} => {
                ExprKind::Index{
                    lhs: self.expr_id(lhs),
                    index: self.expr_id(index),
                }
            }

            _ => panic!("convert {:?}",old.kind)
        };

        Expr{
            kind,
            ty: self.ctx.type_from_rustc(old.ty)
        }
    }

    fn pattern(&self, old: &thir::Pat<'tcx>) -> Pattern<'vm> {
        let kind = match old.kind {
            thir::PatKind::AscribeUserType{ref subpattern,..} => {
                return self.pattern(subpattern);
            }
            thir::PatKind::Binding{ref subpattern, ref var, mode,..} => {
                assert!(subpattern.is_none());
                let mode = match mode {
                    rustc_middle::thir::BindingMode::ByValue => BindingMode::Value,
                    rustc_middle::thir::BindingMode::ByRef(_) => BindingMode::Ref,
                };
                let hir_id = var.0;
                // will probably not hold true for closures
                assert_eq!(hir_id.owner.def_id,self.func_id);

                let local_id = hir_id.local_id.as_u32();
                PatternKind::LocalBinding(local_id,mode)
            }
            thir::PatKind::Leaf{ref subpatterns} => {
                let fields: Vec<_> = subpatterns.iter().map(|child| {
                    FieldPattern{
                        field: child.field.as_u32(),
                        pattern: self.pattern(&child.pattern)
                    }
                }).collect();
                PatternKind::Struct { fields }
            }
            thir::PatKind::Wild => PatternKind::Hole,
            _ => panic!("pattern kind {:?}",old.kind)
        };

        Pattern {
            kind,
            ty: self.ctx.type_from_rustc(old.ty)
        }
    }

    fn stmt(&self, old: &thir::Stmt<'tcx>) -> Stmt<'vm> {
        use rustc_middle::thir::StmtKind;
        match old.kind {
            StmtKind::Expr{expr,..} => Stmt::Expr(self.expr_id(expr)),
            StmtKind::Let{ref pattern,initializer,else_block,..} => {
                Stmt::Let{
                    pattern: self.pattern(pattern),
                    init: initializer.map(|x| self.expr_id(x)),
                    else_block: else_block.map(|x| self.block_id(x)),
                }
            }
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
}
