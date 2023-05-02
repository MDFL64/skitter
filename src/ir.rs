/// This module contains a re-implementation of the THIR, compatible with our own interned types and suitable for serialization.

use std::str::FromStr;

use rustc_middle::ty::Ty;
use rustc_middle::thir;
use rustc_middle::thir::Thir;
use rustc_middle::middle::region::Scope;
use rustc_hir::def_id::LocalDefId;

pub struct IRFunction<'vm> {
    pub root_expr: ExprId,
    exprs: Vec<Expr<'vm>>,
    stmts: Vec<Stmt<'vm>>,
    blocks: Vec<Block>
}

#[derive(Debug)]
pub struct BlockId(u32);
#[derive(Debug)]
pub struct StmtId(u32);
#[derive(Debug)]
pub struct ExprId(u32);

struct Expr<'vm> {
    kind: ExprKind<'vm>,
    ty: Ty<'vm>
}

enum Stmt<'vm> {
    Expr(ExprId),
    Let{
        pattern: Box<Pattern<'vm>>,
        init: Option<ExprId>,
        else_block: Option<BlockId>
    }
}

struct Block {
    stmts: Vec<StmtId>,
    result: Option<ExprId>,
}

struct Pattern<'vm> {
    kind: PatternKind,
    ty: Ty<'vm>
}

#[derive(Debug)]
enum ExprKind<'vm> {
    /// Used to replace some intermediate nodes which aren't super useful to us.
    /// Stores an optional scope ID which is used to resolve breaks
    Dummy(ExprId,Option<u32>),

    /// Primitive casts
    Cast(ExprId),

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

    Call{func_ty: Ty<'vm>, func: ExprId, args: Vec<ExprId>},
    Tuple(Vec<ExprId>),

    Field{ lhs: ExprId, variant: u32, field: u32 }
}

enum PatternKind {
    LocalBinding(u32)
}

#[derive(Debug)]
enum UnaryOp {
    Neg,
    Not
}

#[derive(Debug)]
enum BinaryOp {
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
enum LogicOp {
    And,
    Or
}

pub struct IRFunctionBuilder {
    func_id: LocalDefId
}

impl IRFunctionBuilder {
    pub fn build<'vm>(func_id: LocalDefId, root: thir::ExprId, thir: &Thir<'vm>) -> IRFunction<'vm> {
        let builder = Self{
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

        IRFunction{
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

    fn expr<'vm>(&self, old: &thir::Expr<'vm>) -> Expr<'vm> {
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
            thir::ExprKind::Loop{body} => {
                ExprKind::Loop(self.expr_id(body))
            }

            thir::ExprKind::Call{ty,fun,ref args,..} => {
                ExprKind::Call{
                    func_ty: ty,
                    func: self.expr_id(fun),
                    args: args.iter().map(|arg| self.expr_id(*arg)).collect()
                }
            }
            thir::ExprKind::Tuple{ref fields} => {
                let fields = fields.iter().map(|f| self.expr_id(*f)).collect();
                ExprKind::Tuple(fields)
            }

            thir::ExprKind::Field{lhs,variant_index,name} => {

                ExprKind::Field{
                    lhs: self.expr_id(lhs),
                    variant: variant_index.as_u32(),
                    field: name.as_u32()
                }
            }

            _ => panic!("convert {:?}",old.kind)
        };

        Expr{
            kind,
            ty: old.ty
        }
    }

    fn pattern<'vm>(&self, old: &thir::Pat<'vm>) -> Box<Pattern<'vm>> {
        let kind = match old.kind {
            thir::PatKind::AscribeUserType{ref subpattern,..} => {
                return self.pattern(subpattern);
            }
            thir::PatKind::Binding{ref subpattern, ref var, mode,..} => {
                assert!(subpattern.is_none());
                assert!(mode == rustc_middle::thir::BindingMode::ByValue);

                let hir_id = var.0;
                // will probably not hold true for closures
                assert_eq!(hir_id.owner.def_id,self.func_id);

                let local_id = hir_id.local_id.as_u32();
                PatternKind::LocalBinding(local_id)
            }
            _ => panic!("pattern kind {:?}",old.kind)
        };

        Box::new(Pattern {
            kind,
            ty: old.ty
        })
    }

    fn stmt<'vm>(&self, old: &thir::Stmt<'vm>) -> Stmt<'vm> {
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
