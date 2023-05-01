use rustc_middle::ty::{Ty, TyKind, IntTy, UintTy, FloatTy, AdtKind};
use rustc_abi::VariantIdx;

use crate::{abi::POINTER_SIZE, vm::VM};

#[derive(Debug)]
pub struct Layout {
    pub size: u32,
    pub align: u32,
    pub kind: LayoutKind,
    pub field_offsets: Vec<u32>
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

    Ref,
    Ptr,
    Tuple,
    Struct,

    Never
}

#[derive(Debug,Copy,Clone)]
pub enum IntSign {
    Signed,
    Unsigned
}

impl Layout {
    pub fn from<'tcx>(ty: Ty<'tcx>, vm: &VM<'tcx>) -> Self {
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

            TyKind::Never => Layout::simple(0,LayoutKind::Never),

            TyKind::Ref(..)  => Layout::simple(POINTER_SIZE.bytes(), LayoutKind::Ref),
            TyKind::RawPtr(..) => Layout::simple(POINTER_SIZE.bytes(), LayoutKind::Ptr),

            TyKind::Tuple(list) => {
                // should be quick if this is ()
                Layout::compound(list.iter(),LayoutKind::Tuple,vm)
            }
            TyKind::Adt(def,subs) => {
                match def.adt_kind() {
                    AdtKind::Struct => {
                        let variant = def.variant(VariantIdx::from_u32(0));
                        let fields = variant.fields.iter().map(|f| f.ty(vm.tcx,subs));
                        Layout::compound(fields,LayoutKind::Struct,vm)
                    }
                    AdtKind::Enum => panic!("enum"),
                    AdtKind::Union => panic!("union"),
                }
            }
            _ => panic!("can't layout: {:?}",kind)
        }
    }

    pub fn align(x: u32, align: u32) -> u32 {
        let mask = align - 1;
        (x + mask) & !mask
    }

    fn compound<'tcx>(fields: impl Iterator<Item=Ty<'tcx>>, kind: LayoutKind, vm: &VM<'tcx>) -> Self {
        let mut size = 0;
        let mut align = 1;

        let field_offsets = fields.map(|ty| {
            let layout = Layout::from(ty,vm);

            size = Self::align(size, align);

            let offset = size;

            size += layout.size;
            align = align.max(layout.align);

            offset
        }).collect();

        // dumb
        while (size % align) != 0 {
            size += 1;
        }

        Layout {
            size,
            align,
            kind,
            field_offsets
        }
    }

    fn simple(size: u32, kind: LayoutKind) -> Self {
        Self {
            size,
            align: size.max(1),
            kind,
            field_offsets: Vec::new()
        }
    }
}
