use crate::{
    items::FunctionSig,
    types::{ItemWithSubs, Mutability, Type}, persist::{Persist, PersistReader, PersistWriter},
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
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let sig_inputs = Persist::persist_read(reader);
        let sig_output = Persist::persist_read(reader);

        let sig = FunctionSig{
            inputs: sig_inputs,
            output: sig_output
        };
        panic!()
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.sig.inputs.persist_write(writer);
        self.sig.output.persist_write(writer);

        self.is_constant.persist_write(writer);

        self.root_expr.persist_write(writer);
        self.params.persist_write(writer);

        self.exprs.persist_write(writer);
        self.patterns.persist_write(writer);
    }
}

impl<'vm> Persist<'vm> for ExprId {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        Self(u32::persist_read(reader))
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.0.persist_write(writer)
    }
}

impl<'vm> Persist<'vm> for PatternId {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        Self(u32::persist_read(reader))
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.0.persist_write(writer)
    }
}

impl<'vm> Persist<'vm> for Block {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.stmts.persist_write(writer);
        self.result.persist_write(writer);
    }
}

impl<'vm> Persist<'vm> for Stmt {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        match self {
            Stmt::Expr(e) => {
                writer.write_byte(b'E');
                e.persist_write(writer);
            }
            Stmt::Let{
                pattern,
                init,
                else_block
            } => {
                writer.write_byte(b'L');
                pattern.persist_write(writer);
                init.persist_write(writer);
                else_block.persist_write(writer);
            }
        }
    }
}

impl<'vm> Persist<'vm> for Expr<'vm> {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.ty.persist_write(writer);
        match self.kind {
            ExprKind::LiteralVoid => {
                writer.write_byte(b'0');
            }
            ExprKind::AssignOp(op,lhs,rhs) => {
                writer.write_byte(b'a');
                op.persist_write(writer);
                lhs.persist_write(writer);
                rhs.persist_write(writer);
            }
            ExprKind::Binary(op,lhs,rhs) => {
                writer.write_byte(b'b');
                op.persist_write(writer);
                lhs.persist_write(writer);
                rhs.persist_write(writer);
            }
            ExprKind::Call{ func, ref args } => {
                writer.write_byte(b'c');
                func.persist_write(writer);
                args.persist_write(writer);
            }
            ExprKind::LiteralValue(x) => {
                writer.write_byte(b'n');
                x.persist_write(writer);
            }
            ExprKind::Unary(op,e) => {
                writer.write_byte(b'u');
                match op {
                    UnaryOp::Not => writer.write_byte(b'!'),
                    UnaryOp::Neg => writer.write_byte(b'-'),
                }
                e.persist_write(writer);
            }
            ExprKind::VarRef(n) => {
                writer.write_byte(b'v');
                n.persist_write(writer);
            }
            ExprKind::Break{loop_id,value} => {
                writer.write_byte(b'B');
                loop_id.0.persist_write(writer);
                value.persist_write(writer);
            }
            ExprKind::LogicOp(op,lhs,rhs) => {
                writer.write_byte(b'G');
                match op {
                    LogicOp::And => writer.write_byte(b'&'),
                    LogicOp::Or => writer.write_byte(b'|'),
                }
                lhs.persist_write(writer);
                rhs.persist_write(writer);
            }
            ExprKind::Loop(ref block,loop_id) => {
                writer.write_byte(b'L');
                block.persist_write(writer);
                loop_id.0.persist_write(writer);
            }
            ExprKind::Return(value) => {
                writer.write_byte(b'R');
                value.persist_write(writer);
            }
            ExprKind::Assign(lhs,rhs) => {
                writer.write_byte(b'=');
                lhs.persist_write(writer);
                rhs.persist_write(writer);
            }
            ExprKind::If{cond,then,else_opt} => {
                writer.write_byte(b'?');
                cond.persist_write(writer);
                then.persist_write(writer);
                else_opt.persist_write(writer);
            }
            ExprKind::Cast(e) => {
                writer.write_byte(b'@');
                e.persist_write(writer);
            }
            ExprKind::Dummy(e) => {
                writer.write_byte(b',');
                e.persist_write(writer);
            }
            ExprKind::DeRef(e) => {
                writer.write_byte(b'*');
                e.persist_write(writer);
            }
            ExprKind::Ref(e,mutability) => {
                writer.write_byte(b'&');
                match mutability {
                    Mutability::Const => writer.write_byte(b'c'),
                    Mutability::Mut =>   writer.write_byte(b'm'),
                }
                e.persist_write(writer);
            }
            ExprKind::Block(ref block) => {
                writer.write_byte(b'{');
                block.persist_write(writer);
            }
            ExprKind::Tuple(ref args) => {
                writer.write_byte(b'(');
                args.persist_write(writer);
            }
            _ => panic!("todo persist expr {:?}",self.kind)
        }
    }
}

impl<'vm> Persist<'vm> for Pattern<'vm> {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.ty.persist_write(writer);
        match self.kind {
            PatternKind::LocalBinding { local_id, mode, sub_pattern } => {
                writer.write_byte(b'L');
                local_id.persist_write(writer);
                match mode {
                    BindingMode::Value => writer.write_byte(b'V'),
                    BindingMode::Ref => writer.write_byte(b'R'),
                }
                sub_pattern.persist_write(writer);
            }
            _ => panic!("todo persist pattern {:?}",self.kind)
        }
    }
}

impl<'vm> Persist<'vm> for BinaryOp {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        panic!()
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        match self {
            BinaryOp::Add => writer.write_byte(b'+'),
            BinaryOp::Sub => writer.write_byte(b'-'),
            BinaryOp::Mul => writer.write_byte(b'*'),
            BinaryOp::Div => writer.write_byte(b'/'),
            BinaryOp::Rem => writer.write_byte(b'%'),

            BinaryOp::BitAnd => writer.write_byte(b'&'),
            BinaryOp::BitOr => writer.write_byte(b'|'),
            BinaryOp::BitXor => writer.write_byte(b'^'),

            BinaryOp::ShiftL => writer.write_byte(b'U'),
            BinaryOp::ShiftR => writer.write_byte(b'D'),

            BinaryOp::Eq => writer.write_byte(b'='),
            BinaryOp::NotEq => writer.write_byte(b'!'),

            BinaryOp::Lt => writer.write_byte(b'<'),
            BinaryOp::Gt => writer.write_byte(b'>'),
            BinaryOp::LtEq => writer.write_byte(b'L'),
            BinaryOp::GtEq => writer.write_byte(b'G')
        }
    }
}
