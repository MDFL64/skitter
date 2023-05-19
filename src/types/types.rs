use std::fmt::{Display, Write};

use crate::items::Item;

use super::Type;

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum TypeKind<'vm> {
    Int(IntWidth,IntSign),
    Float(FloatWidth),
    Char,
    Bool,
    Never,

    Ref(Type<'vm>,Mutability),
    Ptr(Type<'vm>,Mutability),

    Tuple(Vec<Type<'vm>>),
    Array(Type<'vm>,ArraySize),
    Slice(Type<'vm>),
    Str,

    FunctionDef(ItemWithSubs<'vm>),
    Adt(ItemWithSubs<'vm>),
    AssociatedType(ItemWithSubs<'vm>),
    Foreign,
    Dynamic,
    FunctionPointer,

    Param(u32),
    Error
}

impl<'vm> TypeKind<'vm> {
    pub fn is_dummy(&self) -> bool {
        match self {
            TypeKind::Dynamic => true,
            _ => false
        }
    }
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

#[derive(Debug,Copy,Clone,Hash,PartialEq,Eq)]
pub enum Mutability {
    Mut,
    Const
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

// pretty printing for types
impl<'vm> Display for Sub<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sub::Type(ty) => write!(f,"{}",ty),
            Sub::Lifetime => write!(f,"'_"),
            Sub::Const => write!(f,"<const>")
        }
    }
}

pub fn sub_list_to_string<'vm>(subs: &[Sub<'vm>]) -> String {
    let mut res = "<".to_owned();
    for (i,sub) in subs.iter().enumerate() {
        if i>0 {
            res.push(',');
        }
        write!(res,"{}",sub).ok();
    }
    res.push('>');
    res
}

impl<'vm> Display for Type<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            TypeKind::Adt(item_with_subs) => {
                write!(f,"{}",item_with_subs.item.path.as_string())?;
                if item_with_subs.subs.len() > 0 {
                    write!(f,"{}",sub_list_to_string(&item_with_subs.subs))?;
                }
                Ok(())
            }
            TypeKind::Ref(ty,mutability) => {
                match mutability {
                    Mutability::Const => write!(f,"&{}",ty),
                    Mutability::Mut => write!(f,"&mut {}",ty),
                }
            }
            _ => {
                write!(f,"{:?}",self)
            }
        }
    }
}
