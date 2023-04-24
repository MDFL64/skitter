use rustc_middle::ty::Ty;
use rustc_middle::ty::TyKind;
use rustc_middle::ty::IntTy;
use rustc_middle::ty::UintTy;

#[derive(Debug)]
pub struct Layout {
    pub size: usize,
    pub align: usize
}

impl Layout {
    pub fn from(ty: Ty) -> Self {
        let kind = ty.kind();
        match kind {
            //TyKind::
            TyKind::Int(IntTy::I32) => Layout::simple(4),
            TyKind::Int(IntTy::I128) => Layout::simple(16),

            TyKind::Uint(UintTy::U32) => Layout::simple(4),
            TyKind::Uint(UintTy::U128) => Layout::simple(16),

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
