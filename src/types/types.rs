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
    Array(Type<'vm>,u32),
    Slice(Type<'vm>),

    FunctionDef(TypeDef<'vm>),
    Adt(TypeDef<'vm>),

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
pub struct TypeDef<'vm> {
    pub item: Item<'vm>,
    pub subs: Vec<Sub<'vm>>
}

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum Sub<'vm> {
    Type(Type<'vm>)
}

impl<'vm> Sub<'vm> {
    pub fn sub(&self, subs: &[Sub<'vm>]) -> Self {
        match self {
            Sub::Type(ty) => Sub::Type(ty.sub(subs)),
            _ => self.clone()
        }
    }
}
