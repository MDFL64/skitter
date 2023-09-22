use std::fmt::Display;

use crate::{items::GenericCounts, vm::VM};

use super::{Type, TypeKind};

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Sub<'vm> {
    Type(Type<'vm>),
    Lifetime,
    Const,
}

impl<'vm> Sub<'vm> {
    pub fn sub(&self, subs: &SubList<'vm>) -> Self {
        match self {
            Sub::Type(ty) => Sub::Type(ty.sub(subs)),
            _ => self.clone(),
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Sub::Type(ty) => ty.is_concrete(),
            Sub::Lifetime => true,
            Sub::Const => todo!(),
        }
    }

    pub fn assert_ty(&self) -> Type<'vm> {
        match self {
            Sub::Type(ty) => *ty,
            _ => panic!("not a type"),
        }
    }
}

// pretty printing for types
impl<'vm> Display for Sub<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sub::Type(ty) => write!(f, "{}", ty),
            Sub::Lifetime => write!(f, "'_"),
            Sub::Const => write!(f, "<const>"),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct SubList<'vm> {
    pub list: Vec<Sub<'vm>>,
}

impl<'vm> SubList<'vm> {
    pub fn from_summary(summary: &GenericCounts, vm: &'vm VM<'vm>) -> Self {
        let total = summary.lifetimes + summary.types + summary.consts;
        let mut list = Vec::with_capacity(total as usize);

        for _ in 0..summary.lifetimes {
            list.push(Sub::Lifetime);
        }
        for _ in 0..summary.types {
            let ty = vm.common_types().unknown;

            list.push(Sub::Type(ty));
        }
        for _ in 0..summary.consts {
            list.push(Sub::Const);
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

        for (i,sub) in self.list.iter().enumerate() {
            match sub {
                Sub::Type(ty) => {
                    if let TypeKind::Param(p) = ty.kind() {
                        if *p != i as u32 {
                            return false;
                        }
                    } else {
                        panic!("is_identity? {}",ty);
                    }
                }
                _ => panic!("is_identity? {:?}",sub)
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
