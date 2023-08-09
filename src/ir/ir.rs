use crate::{
    items::FunctionSig,
    types::{ItemWithSubs, Mutability, Type}, persist::{Persist, PersistReadContext, PersistWriteContext},
};

#[derive(Default)]
pub struct IRFunctionBuilder<'vm> {
    exprs: Vec<Expr<'vm>>,
    patterns: Vec<Pattern<'vm>>,
}

impl<'vm> IRFunctionBuilder<'vm> {
    pub fn add_expr(&mut self, e: Expr<'vm>) -> ExprId {
        let expr_id = ExprId(self.exprs.len() as u32);
        self.exprs.push(e);
        expr_id
    }

    pub fn add_pattern(&mut self, p: Pattern<'vm>) -> PatternId {
        let pat_id = PatternId(self.patterns.len() as u32);
        self.patterns.push(p);
        pat_id
    }

    pub fn expr(&self, id: ExprId) -> &Expr<'vm> {
        &self.exprs[id.0 as usize]
    }

    pub fn pattern(&self, id: PatternId) -> &Pattern<'vm> {
        &self.patterns[id.0 as usize]
    }

    pub fn finish(
        self,
        root_expr: ExprId,
        is_constant: bool,
        params: Vec<PatternId>,
    ) -> IRFunction<'vm> {
        let output = self.exprs[root_expr.0 as usize].ty;
        let inputs = params
            .iter()
            .map(|p| self.patterns[p.0 as usize].ty)
            .collect();

        let sig = FunctionSig { inputs, output };

        IRFunction {
            sig,
            is_constant,
            params,
            root_expr,
            exprs: self.exprs,
            patterns: self.patterns,
        }
    }
}

// IR structures

pub struct IRFunction<'vm> {
    pub sig: FunctionSig<'vm>,
    pub is_constant: bool,
    pub root_expr: ExprId,
    pub params: Vec<PatternId>,
    exprs: Vec<Expr<'vm>>,
    patterns: Vec<Pattern<'vm>>,
}

impl<'vm> IRFunction<'vm> {
    pub fn expr(&self, id: ExprId) -> &Expr<'vm> {
        &self.exprs[id.0 as usize]
    }

    pub fn pattern(&self, id: PatternId) -> &Pattern<'vm> {
        &self.patterns[id.0 as usize]
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExprId(u32);
#[derive(Debug, Clone, Copy)]
pub struct PatternId(u32);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LoopId(u32);

impl LoopId {
    pub fn new(x: u32) -> Self {
        Self(x)
    }
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub result: Option<ExprId>,
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

pub struct Expr<'vm> {
    pub kind: ExprKind<'vm>,
    pub ty: Type<'vm>,
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
    Loop(Block, LoopId),
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
    Ref(ExprId, Mutability),
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
        arms: Vec<MatchArm>,
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
    Error,
}

#[derive(Debug,Clone,Copy)]
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

#[derive(Debug,Clone,Copy)]
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

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: PatternId,
    pub body: ExprId,
    pub has_guard: bool,
}

// Persistence!

impl<'vm> Persist<'vm> for IRFunction<'vm> {
    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        panic!();
    }

    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.sig.inputs.persist_write(write_ctx);
        self.sig.output.persist_write(write_ctx);

        self.is_constant.persist_write(write_ctx);

        self.root_expr.persist_write(write_ctx);
        self.params.persist_write(write_ctx);

        self.exprs.persist_write(write_ctx);
        self.patterns.persist_write(write_ctx);
    }
}

impl<'vm> Persist<'vm> for ExprId {
    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        Self(u32::persist_read(read_ctx))
    }

    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.0.persist_write(write_ctx)
    }
}

impl<'vm> Persist<'vm> for PatternId {
    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        Self(u32::persist_read(read_ctx))
    }

    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.0.persist_write(write_ctx)
    }
}

impl<'vm> Persist<'vm> for Block {
    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.stmts.persist_write(write_ctx);
        self.result.persist_write(write_ctx);
    }
}

impl<'vm> Persist<'vm> for Stmt {
    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        match self {
            Stmt::Expr(e) => {
                write_ctx.write_byte(b'E');
                e.persist_write(write_ctx);
            }
            Stmt::Let{
                pattern,
                init,
                else_block
            } => {
                write_ctx.write_byte(b'L');
                pattern.persist_write(write_ctx);
                init.persist_write(write_ctx);
                else_block.persist_write(write_ctx);
            }
        }
    }
}

impl<'vm> Persist<'vm> for Expr<'vm> {
    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.ty.persist_write(write_ctx);
        match self.kind {
            ExprKind::LiteralVoid => {
                write_ctx.write_byte(b'0');
            }
            ExprKind::AssignOp(op,lhs,rhs) => {
                write_ctx.write_byte(b'a');
                op.persist_write(write_ctx);
                lhs.persist_write(write_ctx);
                rhs.persist_write(write_ctx);
            }
            ExprKind::Binary(op,lhs,rhs) => {
                write_ctx.write_byte(b'b');
                op.persist_write(write_ctx);
                lhs.persist_write(write_ctx);
                rhs.persist_write(write_ctx);
            }
            ExprKind::Call{ func, ref args } => {
                write_ctx.write_byte(b'c');
                func.persist_write(write_ctx);
                args.persist_write(write_ctx);
            }
            ExprKind::LiteralValue(x) => {
                write_ctx.write_byte(b'n');
                x.persist_write(write_ctx);
            }
            ExprKind::Unary(op,e) => {
                write_ctx.write_byte(b'u');
                match op {
                    UnaryOp::Not => write_ctx.write_byte(b'!'),
                    UnaryOp::Neg => write_ctx.write_byte(b'-'),
                }
                e.persist_write(write_ctx);
            }
            ExprKind::VarRef(n) => {
                write_ctx.write_byte(b'v');
                n.persist_write(write_ctx);
            }
            ExprKind::Break{loop_id,value} => {
                write_ctx.write_byte(b'B');
                loop_id.0.persist_write(write_ctx);
                value.persist_write(write_ctx);
            }
            ExprKind::LogicOp(op,lhs,rhs) => {
                write_ctx.write_byte(b'G');
                match op {
                    LogicOp::And => write_ctx.write_byte(b'&'),
                    LogicOp::Or => write_ctx.write_byte(b'|'),
                }
                lhs.persist_write(write_ctx);
                rhs.persist_write(write_ctx);
            }
            ExprKind::Loop(ref block,loop_id) => {
                write_ctx.write_byte(b'L');
                block.persist_write(write_ctx);
                loop_id.0.persist_write(write_ctx);
            }
            ExprKind::Return(value) => {
                write_ctx.write_byte(b'R');
                value.persist_write(write_ctx);
            }
            ExprKind::Assign(lhs,rhs) => {
                write_ctx.write_byte(b'=');
                lhs.persist_write(write_ctx);
                rhs.persist_write(write_ctx);
            }
            ExprKind::If{cond,then,else_opt} => {
                write_ctx.write_byte(b'?');
                cond.persist_write(write_ctx);
                then.persist_write(write_ctx);
                else_opt.persist_write(write_ctx);
            }
            ExprKind::Cast(e) => {
                write_ctx.write_byte(b'@');
                e.persist_write(write_ctx);
            }
            ExprKind::Dummy(e) => {
                write_ctx.write_byte(b',');
                e.persist_write(write_ctx);
            }
            ExprKind::DeRef(e) => {
                write_ctx.write_byte(b'*');
                e.persist_write(write_ctx);
            }
            ExprKind::Ref(e,mutability) => {
                write_ctx.write_byte(b'&');
                match mutability {
                    Mutability::Const => write_ctx.write_byte(b'c'),
                    Mutability::Mut =>   write_ctx.write_byte(b'm'),
                }
                e.persist_write(write_ctx);
            }
            ExprKind::Block(ref block) => {
                write_ctx.write_byte(b'{');
                block.persist_write(write_ctx);
            }
            ExprKind::Tuple(ref args) => {
                write_ctx.write_byte(b'(');
                args.persist_write(write_ctx);
            }
            _ => panic!("todo persist expr {:?}",self.kind)
        }
    }
}

impl<'vm> Persist<'vm> for Pattern<'vm> {
    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.ty.persist_write(write_ctx);
        match self.kind {
            PatternKind::LocalBinding { local_id, mode, sub_pattern } => {
                write_ctx.write_byte(b'L');
                local_id.persist_write(write_ctx);
                match mode {
                    BindingMode::Value => write_ctx.write_byte(b'V'),
                    BindingMode::Ref => write_ctx.write_byte(b'R'),
                }
                sub_pattern.persist_write(write_ctx);
            }
            _ => panic!("todo persist pattern {:?}",self.kind)
        }
    }
}

impl<'vm> Persist<'vm> for BinaryOp {
    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        match self {
            BinaryOp::Add => write_ctx.write_byte(b'+'),
            BinaryOp::Sub => write_ctx.write_byte(b'-'),
            BinaryOp::Mul => write_ctx.write_byte(b'*'),
            BinaryOp::Div => write_ctx.write_byte(b'/'),
            BinaryOp::Rem => write_ctx.write_byte(b'%'),

            BinaryOp::BitAnd => write_ctx.write_byte(b'&'),
            BinaryOp::BitOr => write_ctx.write_byte(b'|'),
            BinaryOp::BitXor => write_ctx.write_byte(b'^'),

            BinaryOp::ShiftL => write_ctx.write_byte(b'U'),
            BinaryOp::ShiftR => write_ctx.write_byte(b'D'),

            BinaryOp::Eq => write_ctx.write_byte(b'='),
            BinaryOp::NotEq => write_ctx.write_byte(b'!'),

            BinaryOp::Lt => write_ctx.write_byte(b'<'),
            BinaryOp::Gt => write_ctx.write_byte(b'>'),
            BinaryOp::LtEq => write_ctx.write_byte(b'L'),
            BinaryOp::GtEq => write_ctx.write_byte(b'G')
        }
    }
}
