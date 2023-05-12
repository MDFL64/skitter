use crate::{abi::POINTER_SIZE, types::{Type, TypeKind, IntWidth}};

use super::FloatWidth;

#[derive(Debug)]
pub struct Layout {
    pub maybe_size: Option<u32>,
    pub align: u32,
    pub field_offsets: Vec<u32>
}

impl Layout {
    pub fn assert_size(&self) -> u32 {
        self.maybe_size.expect("unsized type not permitted")
    }

    pub fn is_sized(&self) -> bool {
        self.maybe_size.is_some()
    }
}

impl Layout {
    pub fn from<'vm>(ty: Type<'vm>) -> Self {
        let kind = ty.kind();
        match kind {
            TypeKind::Int(width,_) => {
                let size = match width {
                    IntWidth::I8 => 1,
                    IntWidth::I16 => 2,
                    IntWidth::I32 => 4,
                    IntWidth::I64 => 8,
                    IntWidth::I128 => 16,
                    IntWidth::ISize => POINTER_SIZE.bytes()
                };
                Layout::simple(size)
            }
            TypeKind::Float(FloatWidth::F32) => Layout::simple(4),
            TypeKind::Float(FloatWidth::F64) => Layout::simple(8),
            TypeKind::Bool => Layout::simple(1),
            TypeKind::Char => Layout::simple(4),
            TypeKind::Tuple(fields) => {
                Layout::compound(fields.iter().copied())
            }
            TypeKind::Array(elem_ty,count) => {
                let elem_layout = elem_ty.layout();
                let elem_size = elem_layout.assert_size();

                Layout {
                    maybe_size: Some(elem_size * count),
                    align: elem_layout.align,
                    field_offsets: Vec::new()
                }
            }
            TypeKind::Slice(elem_ty) => {
                let elem_layout = elem_ty.layout();

                Layout {
                    maybe_size: None,
                    align: elem_layout.align,
                    field_offsets: Vec::new()
                }
            }

            TypeKind::Ref(ref_ty) |
            TypeKind::Ptr(ref_ty) => {
                let ptr_size = POINTER_SIZE.bytes();

                let ref_layout = Layout::from(*ref_ty);
                if ref_layout.is_sized() {
                    Layout::simple(ptr_size)
                } else {
                    Self {
                        maybe_size: Some(ptr_size * 2),
                        align: ptr_size,
                        field_offsets: Vec::new()
                    }
                }
            }
            TypeKind::Adt() => {
                /*let fields = def.item.get_adt_fields();

                let fixed_fields = fields.iter().map(|field| {
                    field.sub(&def.subs)
                });

                Layout::compound(fixed_fields)*/
                panic!("todo fix adt")
            }
            _ => panic!("can't layout: {:?}",kind)
        }
    }

    fn compound<'vm>(fields: impl Iterator<Item=Type<'vm>>) -> Self {
        let mut size = 0;
        let mut align = 1;

        let field_offsets = fields.map(|ty| {
            let layout = Layout::from(ty);

            size = crate::abi::align(size, layout.align);

            let offset = size;

            // todo unsized structs
            size += layout.assert_size();
            align = align.max(layout.align);

            offset
        }).collect();

        size = crate::abi::align(size, align);

        Layout {
            maybe_size: Some(size),
            align,
            field_offsets
        }
    }

    fn simple(size: u32) -> Self {
        Self {
            maybe_size: Some(size),
            align: size.max(1),
            field_offsets: Vec::new()
        }
    }
}
