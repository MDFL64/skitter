use std::fmt::Display;

use skitter_macro::Persist;

use crate::{items::GenericCounts, vm::VM};

use super::{Type, TypeKind};

#[derive(Debug, Hash, PartialEq, Eq, Clone, Persist)]
pub enum Sub<'vm> {
    Type(Type<'vm>),
    Lifetime,
    Const(Type<'vm>, ConstGeneric),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Persist)]
pub enum ConstGeneric {
    Value(i128),
    Param(u32),
    Error,
    Unknown,
}

impl<'vm> Sub<'vm> {
    pub fn sub(&self, subs: &SubList<'vm>) -> Self {
        match self {
            Sub::Type(ty) => Sub::Type(ty.sub(subs)),
            Sub::Const(ty, c) => Sub::Const(*ty, c.sub(subs, *ty)),
            _ => self.clone(),
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Sub::Type(ty) => ty.is_concrete(),
            Sub::Lifetime => true,
            Sub::Const(ty, c) => ty.is_concrete() && c.is_concrete(),
        }
    }

    pub fn assert_ty(&self) -> Type<'vm> {
        match self {
            Sub::Type(ty) => *ty,
            _ => panic!("not a type"),
        }
    }

    pub fn assert_const(&self, expect_ty: Type<'vm>) -> &ConstGeneric {
        match self {
            Sub::Const(ty, c) => {
                assert!(*ty == expect_ty);
                c
            }
            _ => panic!("not a const generic"),
        }
    }
}

// pretty printing for types
impl<'vm> Display for Sub<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sub::Type(ty) => write!(f, "{}", ty),
            Sub::Lifetime => write!(f, "'_"),
            Sub::Const(ty, c) => write!(f, "<const {} {:?}>", ty, c),
        }
    }
}

impl ConstGeneric {
    pub fn get_value(&self) -> i128 {
        match self {
            ConstGeneric::Value(n) => *n,
            _ => panic!("cannot get value from: {:?}", self),
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            ConstGeneric::Value(_) => true,
            _ => false,
        }
    }

    pub fn sub<'vm>(&self, subs: &SubList<'vm>, expect_ty: Type<'vm>) -> Self {
        match self {
            ConstGeneric::Param(sub_n) => subs
                .list
                .get(*sub_n as usize)
                .expect("const index out of bounds")
                .assert_const(expect_ty)
                .clone(),
            _ => self.clone(),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Persist)]
pub struct SubList<'vm> {
    pub list: Vec<Sub<'vm>>,
}

impl<'vm> SubList<'vm> {
    pub fn empty() -> Self {
        Self { list: vec![] }
    }

    pub fn from_summary(summary: &GenericCounts, vm: &'vm VM<'vm>) -> Self {
        let total = summary.total();
        let mut list = Vec::with_capacity(total as usize);

        let unk_ty = vm.common_types().unknown;

        for _ in 0..summary.lifetimes {
            list.push(Sub::Lifetime);
        }
        for _ in 0..summary.types {
            list.push(Sub::Type(unk_ty));
        }
        for _ in 0..summary.consts {
            list.push(Sub::Const(unk_ty, ConstGeneric::Unknown));
        }

        SubList { list }
    }

    pub fn sub(&self, subs: &SubList<'vm>) -> Self {
        let new_subs: Vec<_> = self.list.iter().map(|field| field.sub(subs)).collect();
        SubList { list: new_subs }
    }

    pub fn is_concrete(&self) -> bool {
        self.list.iter().all(|sub| sub.is_concrete())
    }

    pub fn is_identity(&self) -> bool {
        // in practice this is more of an "assert_is_identity" due to the amount of stuff that will blow it up

        for (i, sub) in self.list.iter().enumerate() {
            match sub {
                Sub::Type(ty) => {
                    if let TypeKind::Param(p) = ty.kind() {
                        if *p != i as u32 {
                            return false;
                        }
                    } else {
                        panic!("is_identity? ty: {}", ty);
                    }
                }
                Sub::Const(_, kind) => {
                    if let ConstGeneric::Param(p) = kind {
                        if *p != i as u32 {
                            return false;
                        }
                    } else {
                        panic!("is_identity? const: {:?}", kind);
                    }
                }
                Sub::Lifetime => {
                    // don't care
                }
            }
        }

        true
    }
}

impl<'vm> Display for SubList<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<")?;
        for (i, sub) in self.list.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            write!(f, "{}", sub)?;
        }
        write!(f, ">")
    }
}
