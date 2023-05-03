use rustc_middle::ty::{Ty, TyKind, IntTy, UintTy, FloatTy, AdtKind};
use rustc_abi::VariantIdx;

use crate::{abi::POINTER_SIZE, vm::VM};

#[derive(Debug)]
pub struct Layout {
    pub maybe_size: Option<u32>,
    pub align: u32,
    pub kind: LayoutKind,
    pub field_offsets: Vec<u32>
}

impl Layout {
    pub fn assert_size(&self) -> u32 {
        self.maybe_size.expect("unsized type not permitted")
    }

    pub fn is_sized(&self) -> bool {
        self.maybe_size.is_some()
    } 

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
    Array{elem_size: u32, elem_count: u32},
    Slice{elem_size: u32},

    Never
}

#[derive(Debug,Copy,Clone)]
pub enum IntSign {
    Signed,
    Unsigned
}

impl Layout {
    pub fn from<'tcx>(ty: Ty<'tcx>, vm: &VM<'_,'tcx>) -> Self {
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

            TyKind::Ref(_,ref_ty,_) => Self::pointer(*ref_ty, LayoutKind::Ref, vm),
            TyKind::RawPtr(ptr) => Self::pointer(ptr.ty, LayoutKind::Ptr, vm),

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
            TyKind::Array(elem_ty,count) => {
                let param_env = rustc_middle::ty::ParamEnv::reveal_all();
                let count = count.eval_target_usize(vm.tcx, param_env) as u32;

                let elem_layout = Layout::from(*elem_ty,vm);
                let elem_size = elem_layout.assert_size();

                Layout {
                    maybe_size: Some(elem_size * count),
                    align: elem_layout.align,
                    kind: LayoutKind::Array{
                        elem_size,
                        elem_count: count
                    },
                    field_offsets: Vec::new()
                }
            }
            TyKind::Slice(elem_ty) => {
                let elem_layout = Layout::from(*elem_ty,vm);
                let elem_size = elem_layout.assert_size();

                Layout {
                    maybe_size: None,
                    align: elem_layout.align,
                    kind: LayoutKind::Slice{ elem_size },
                    field_offsets: Vec::new()
                }
            }
            _ => panic!("can't layout: {:?}",kind)
        }
    }

    pub fn align(x: u32, align: u32) -> u32 {
        let mask = align - 1;
        (x + mask) & !mask
    }

    fn compound<'tcx>(fields: impl Iterator<Item=Ty<'tcx>>, kind: LayoutKind, vm: &VM<'_,'tcx>) -> Self {
        let mut size = 0;
        let mut align = 1;

        let field_offsets = fields.map(|ty| {
            let layout = Layout::from(ty,vm);

            size = Self::align(size, layout.align);

            let offset = size;

            // todo unsized structs
            size += layout.assert_size();
            align = align.max(layout.align);

            offset
        }).collect();

        size = Self::align(size, align);

        Layout {
            maybe_size: Some(size),
            align,
            kind,
            field_offsets
        }
    }

    fn pointer<'tcx>(ref_ty: Ty<'tcx>, kind: LayoutKind, vm: &VM<'_,'tcx>) -> Self {
        let ptr_size = POINTER_SIZE.bytes();

        let ref_layout = Layout::from(ref_ty,vm);
        if ref_layout.is_sized() {
            Layout::simple(ptr_size, kind)
        } else {
            Self {
                maybe_size: Some(ptr_size * 2),
                align: ptr_size,
                kind,
                field_offsets: Vec::new()
            }
        }
    }

    fn simple(size: u32, kind: LayoutKind) -> Self {
        Self {
            maybe_size: Some(size),
            align: size.max(1),
            kind,
            field_offsets: Vec::new()
        }
    }
}
