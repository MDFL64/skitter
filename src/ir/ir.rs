use crate::{
    items::FunctionSig,
    types::{ItemWithSubs, Mutability, Type},
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

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: PatternId,
    pub body: ExprId,
    pub has_guard: bool,
}
