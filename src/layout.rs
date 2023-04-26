use rustc_middle::ty::{Ty, TyKind, IntTy, UintTy, FloatTy};

use crate::abi::POINTER_SIZE;

#[derive(Debug)]
pub struct Layout {
    pub size: u32,
    pub align: u32,
    pub kind: LayoutKind
}

impl Layout {
    pub fn sign(&self) -> IntSign {
        match self.kind {
            LayoutKind::Int(sign) => sign,
            _ => IntSign::Unsigned
        }
    }
}

#[derive(Debug)]
pub enum LayoutKind {
    Int(IntSign),
    Float,
    Bool,
    Void
}

#[derive(Debug,Copy,Clone)]
pub enum IntSign {
    Signed,
    Unsigned
}

impl Layout {
    pub fn from(ty: Ty) -> Self {
        let kind = ty.kind();
        match kind {
            TyKind::Int(IntTy::I8) => Layout::simple(1, LayoutKind::Int(IntSign::Signed)),
            TyKind::Int(IntTy::I16) => Layout::simple(2, LayoutKind::Int(IntSign::Signed)),
            TyKind::Int(IntTy::I32) => Layout::simple(4, LayoutKind::Int(IntSign::Signed)),
            TyKind::Int(IntTy::I64) => Layout::simple(8, LayoutKind::Int(IntSign::Signed)),
            TyKind::Int(IntTy::I128) => Layout::simple(16, LayoutKind::Int(IntSign::Signed)),

            TyKind::Uint(UintTy::U8) => Layout::simple(1, LayoutKind::Int(IntSign::Unsigned)),
            TyKind::Uint(UintTy::U16) => Layout::simple(2, LayoutKind::Int(IntSign::Unsigned)),
            TyKind::Uint(UintTy::U32) => Layout::simple(4, LayoutKind::Int(IntSign::Unsigned)),
            TyKind::Uint(UintTy::U64) => Layout::simple(8, LayoutKind::Int(IntSign::Unsigned)),
            TyKind::Uint(UintTy::U128) => Layout::simple(16, LayoutKind::Int(IntSign::Unsigned)),

            TyKind::Int(IntTy::Isize) => Layout::simple(POINTER_SIZE.bytes(), LayoutKind::Int(IntSign::Signed)),
            TyKind::Uint(UintTy::Usize) => Layout::simple(POINTER_SIZE.bytes(), LayoutKind::Int(IntSign::Unsigned)),

            TyKind::Float(FloatTy::F32) => Layout::simple(4, LayoutKind::Float),
            TyKind::Float(FloatTy::F64) => Layout::simple(8, LayoutKind::Float),

            TyKind::Bool => Layout::simple(1, LayoutKind::Bool),
            TyKind::Char => Layout::simple(4, LayoutKind::Int(IntSign::Unsigned)),

            TyKind::Tuple(list) => {
                if list.len() == 0 {
                    Layout::zero()
                } else {
                    println!("? {:?}",list);
                    panic!("non-trivial tuple");
                }
            }
            _ => panic!("can't layout: {:?}",kind)
        }
    }

    fn simple(size: u32, kind: LayoutKind) -> Self {
        Self {
            size,
            align: size,
            kind
        }
    }

    fn zero() -> Self {
        Self {
            size: 0,
            align: 1,
            kind: LayoutKind::Void
        }
    }
}
