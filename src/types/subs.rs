use std::fmt::Display;

use super::Type;

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum Sub<'vm> {
    Type(Type<'vm>),
    Lifetime,
    Const
}

impl<'vm> Sub<'vm> {
    pub fn sub(&self, subs: &SubList<'vm>) -> Self {
        match self {
            Sub::Type(ty) => Sub::Type(ty.sub(subs)),
            _ => self.clone()
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

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub struct SubList<'vm> {
    pub list: Vec<Sub<'vm>>
}

impl<'vm> SubList<'vm> {
    pub fn sub(&self, subs: &SubList<'vm>) -> Self {
        let new_subs: Vec<_> = self.list.iter().map(|field| {
            field.sub(subs)
        }).collect();
        SubList{ list: new_subs }
    }
}

impl<'vm> Display for SubList<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"<")?;
        for (i,sub) in self.list.iter().enumerate() {
            if i>0 {
                write!(f,",")?;
            }
            write!(f,"{}",sub)?;
        }
        write!(f,">")
    }
}
