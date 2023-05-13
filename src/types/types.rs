use crate::items::Item;

use super::Type;

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum TypeKind<'vm> {
    Int(IntWidth,IntSign),
    Float(FloatWidth),
    Char,
    Bool,
    Never,

    Ref(Type<'vm>),
    Ptr(Type<'vm>),

    Tuple(Vec<Type<'vm>>),
    Array(Type<'vm>,ArraySize),
    Slice(Type<'vm>),
    Str,

    FunctionDef(ItemWithSubs<'vm>),
    Adt(ItemWithSubs<'vm>),
    Alias, // todo
    Foreign,
    Dynamic,
    FunctionPointer,

    Param(u32)
}

#[derive(Debug,Copy,Clone,Hash,PartialEq,Eq)]
pub enum IntSign {
    Signed,
    Unsigned
}

#[derive(Debug,Copy,Clone,Hash,PartialEq,Eq)]
pub enum IntWidth {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize
}

#[derive(Debug,Copy,Clone,Hash,PartialEq,Eq)]
pub enum FloatWidth {
    F32,
    F64
}

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum Sub<'vm> {
    Type(Type<'vm>),
    Lifetime,
    Const
}

impl<'vm> Sub<'vm> {
    pub fn sub(&self, subs: &[Sub<'vm>]) -> Self {
        match self {
            Sub::Type(ty) => Sub::Type(ty.sub(subs)),
            _ => self.clone()
        }
    }
}

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub struct ItemWithSubs<'vm> {
    pub item: &'vm Item<'vm>,
    pub subs: Vec<Sub<'vm>>
}

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum ArraySize {
    Static(u32),
    ConstParam(u32)
}

impl ArraySize {
    pub fn assert_static(&self) -> u32 {
        if let ArraySize::Static(n) = self {
            *n
        } else {
            panic!("expected static sized array, got const param");
        }
    }
}
