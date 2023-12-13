use std::{fmt::Debug, sync::Arc};

use skitter_macro::Persist;

use crate::{
    closure::FnTrait,
    items::FunctionSig,
    types::{ConstGeneric, ItemWithSubs, Mutability, Type, SubList},
    variants::VariantIndex, vm::VM, bytecode_compiler::BytecodeCompiler,
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
        ir_kind: IRKind,
        params: Vec<PatternId>,
        opaque_types: Vec<OpaqueTypeMapping<'vm>>,
    ) -> IRFunction<'vm> {
        let output = self.exprs[root_expr.0 as usize].ty;
        let inputs = params
            .iter()
            .map(|p| self.patterns[p.0 as usize].ty)
            .collect();

        let sig = FunctionSig { inputs, output };

        IRFunction {
            sig,
            ir_kind,
            closure_kind: None,
            params,
            root_expr,
            exprs: self.exprs,
            patterns: self.patterns,
            opaque_types,
        }
    }
}

// IR structures
#[derive(Persist)]
pub struct IRFunction<'vm> {
    pub sig: FunctionSig<'vm>,
    pub ir_kind: IRKind,
    pub closure_kind: Option<FnTrait>,
    pub root_expr: ExprId,
    pub params: Vec<PatternId>,
    pub opaque_types: Vec<OpaqueTypeMapping<'vm>>,
    exprs: Vec<Expr<'vm>>,
    patterns: Vec<Pattern<'vm>>,
}

#[derive(Copy, Clone, Eq, PartialEq, Persist)]
pub enum IRKind {
    Function,
    Constant,
    Static,
}

impl<'vm> Debug for IRFunction<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("IRFunction")
    }
}

impl<'vm> IRFunction<'vm> {
    pub fn expr(&self, id: ExprId) -> &Expr<'vm> {
        &self.exprs[id.0 as usize]
    }

    pub fn pattern(&self, id: PatternId) -> &Pattern<'vm> {
        &self.patterns[id.0 as usize]
    }

    // deliberately given this name to avoid accidental clones
    pub fn clone_ir(&self) -> Self {
        Self {
            sig: self.sig.clone(),
            ir_kind: self.ir_kind,
            closure_kind: self.closure_kind,
            root_expr: self.root_expr,
            params: self.params.clone(),
            exprs: self.exprs.clone(),
            patterns: self.patterns.clone(),
            opaque_types: self.opaque_types.clone(),
        }
    }

    pub fn insert_pattern(&mut self, kind: PatternKind<'vm>, ty: Type<'vm>) -> PatternId {
        let index = self.patterns.len();
        self.patterns.push(Pattern { kind, ty });
        PatternId(index as u32)
    }

    pub fn iter_expr_ids(&self) -> impl Iterator<Item = ExprId> {
        (0..self.exprs.len()).map(|index| ExprId(index as u32))
    }

    pub fn expr_mut(&mut self, id: ExprId) -> &mut Expr<'vm> {
        &mut self.exprs[id.0 as usize]
    }

    pub fn const_eval(&self, vm: &'vm VM<'vm>, subs: &SubList<'vm>) -> (Vec<u8>,Type<'vm>) {
        let bc = BytecodeCompiler::compile(
            vm,
            self,
            subs,
            "<const block>",
            subs,
        );

        let eval_thread = vm.make_thread();
        eval_thread.run_bytecode(&bc, 0);

        let ty = self.sig.output; // todo sub?

        let bytes = eval_thread.copy_result(0, ty.layout().assert_size() as usize);

        (bytes,ty)
    }
}

#[derive(Debug, Clone, Copy, Persist)]
pub struct ExprId(u32);
#[derive(Debug, Clone, Copy, Persist)]
pub struct PatternId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Persist)]
pub struct LoopId(u32);

impl LoopId {
    pub fn new(x: u32) -> Self {
        Self(x)
    }
}

#[derive(Debug, Clone, Persist)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub result: Option<ExprId>,
}

#[derive(Debug, Clone, Persist)]
pub enum Stmt {
    Expr(ExprId),
    Let {
        pattern: PatternId,
        init: Option<ExprId>,
        else_block: Option<Block>,
    },
}

#[derive(Clone, Debug, Persist)]
pub struct Expr<'vm> {
    pub kind: ExprKind<'vm>,
    pub ty: Type<'vm>,
}

#[derive(Debug, Clone, Persist)]
pub struct Pattern<'vm> {
    pub kind: PatternKind<'vm>,
    pub ty: Type<'vm>,
}

#[derive(Debug, Clone, Persist)]
pub struct UpVar {
    pub index: u32,
    pub is_ref: bool,
}

#[derive(Debug, Clone, Persist)]
pub enum ExprKind<'vm> {
    /// Used to replace some intermediate nodes which aren't super useful to us.
    Dummy(ExprId),

    /// Primitive casts
    Cast(ExprId),

    /// Pointer casts
    PointerCast(ExprId, PointerCast),

    Block(Block),
    ConstBlock(Arc<IRFunction<'vm>>),

    /// Variable reference
    VarRef(u32),

    /// Reference to capture.
    /// Inserted right after building closure IR, and handled by the bytecode compiler.
    UpVar(UpVar),

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
        variant: VariantIndex,
        fields: Vec<(u32, ExprId)>,
        rest: Option<ExprId>,
    },
    Array(Vec<ExprId>),
    /// Check the type to determine the count.
    ArrayRepeat(ExprId, ConstGeneric),

    NamedConst(ItemWithSubs<'vm>),
    /// Subs are included for consistency, they should be empty?
    Static(ItemWithSubs<'vm>),

    //Function(ItemWithSubs<'vm>),
    Ref(ExprId, Mutability),
    DeRef(ExprId),

    Field {
        lhs: ExprId,
        variant: VariantIndex,
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

    ConstParam(u32),

    /// Error, for unsupported exprs
    Error(String),
}

#[derive(Debug, Clone, Persist)]
pub enum PatternKind<'vm> {
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
        variant_index: VariantIndex,
    },
    Or {
        options: Vec<PatternId>,
    },
    DeRef {
        sub_pattern: PatternId,
    },
    /// Small literals: integer, float, char, or bool types (same as the ExprKind)
    LiteralValue(i128),
    LiteralBytes(&'vm [u8]),

    Range {
        start: Option<ExprId>,
        end: Option<ExprId>,
        end_is_inclusive: bool,
    },
    Slice {
        start: Vec<PatternId>,
        mid: Option<PatternId>,
        end: Vec<PatternId>,
    },

    NamedConst(ItemWithSubs<'vm>),

    Hole,

    /// Error, for unsupported patterns
    Error(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Persist)]
pub enum BindingMode {
    Value,
    Ref,
}

#[derive(Debug, Clone, Persist)]
pub struct FieldPattern {
    pub field: u32,
    pub pattern: PatternId,
}

#[derive(Debug, Clone, Copy, Persist)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, Persist)]
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

#[derive(Debug, Clone, Copy, Persist)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone, Copy, Persist)]
pub enum PointerCast {
    ReifyFnPointer,
    UnsafeFnPointer,
    ClosureFnPointer,
    MutToConstPointer,
    ArrayToPointer,
    UnSize,
}

#[derive(Debug, Clone, Persist)]
pub struct MatchArm {
    pub pattern: PatternId,
    pub body: ExprId,
    pub guard: MatchGuard,
}

#[derive(Debug, Clone, Persist)]
pub enum MatchGuard {
    If(ExprId),
    IfLet,
    None,
}

#[derive(Clone, Debug, Persist)]
pub struct OpaqueTypeMapping<'vm> {
    pub source_item: ItemWithSubs<'vm>,
    pub source_full_path: &'vm str,
    pub destination_ty: Type<'vm>,
}
