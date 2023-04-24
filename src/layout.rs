use rustc_middle::ty::Ty;
use rustc_middle::ty::TyKind;
use rustc_middle::ty::IntTy;
use rustc_middle::ty::UintTy;

use crate::abi::POINTER_SIZE;

#[derive(Debug)]
pub struct Layout {
    pub size: usize,
    pub align: usize
}

impl Layout {
    pub fn from(ty: Ty) -> Self {
        let kind = ty.kind();
        match kind {
            TyKind::Int(IntTy::I8) => Layout::simple(1),
            TyKind::Int(IntTy::I16) => Layout::simple(2),
            TyKind::Int(IntTy::I32) => Layout::simple(4),
            TyKind::Int(IntTy::I64) => Layout::simple(8),
            TyKind::Int(IntTy::I128) => Layout::simple(16),

            TyKind::Uint(UintTy::U8) => Layout::simple(1),
            TyKind::Uint(UintTy::U16) => Layout::simple(2),
            TyKind::Uint(UintTy::U32) => Layout::simple(4),
            TyKind::Uint(UintTy::U64) => Layout::simple(8),
            TyKind::Uint(UintTy::U128) => Layout::simple(16),

            TyKind::Int(IntTy::Isize) => Layout::simple(POINTER_SIZE.bytes()),
            TyKind::Uint(UintTy::Usize) => Layout::simple(POINTER_SIZE.bytes()),

            TyKind::Bool => Layout::simple(1),
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

    fn simple(size: usize) -> Self {
        Self {
            size,
            align: size
        }
    }

    fn zero() -> Self {
        Self {
            size: 0,
            align: 1
        }
    }
}
