use crate::{
    abi::POINTER_SIZE,
    types::{IntWidth, Type, TypeKind},
};

use super::{FloatWidth, ItemWithSubs};

#[derive(Debug)]
pub struct Layout {
    pub maybe_size: Option<u32>,
    pub align: u32,
    pub field_offsets: Vec<Vec<u32>>,
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
            TypeKind::Int(width, _) => {
                let size = match width {
                    IntWidth::I8 => 1,
                    IntWidth::I16 => 2,
                    IntWidth::I32 => 4,
                    IntWidth::I64 => 8,
                    IntWidth::I128 => 16,
                    IntWidth::ISize => POINTER_SIZE.bytes(),
                };
                Layout::simple(size)
            }
            TypeKind::Float(FloatWidth::F32) => Layout::simple(4),
            TypeKind::Float(FloatWidth::F64) => Layout::simple(8),
            TypeKind::Bool => Layout::simple(1),
            TypeKind::Char => Layout::simple(4),
            TypeKind::Tuple(fields) => {
                Layout::compound(std::iter::once(fields.iter().copied()), None)
            }
            TypeKind::Array(elem_ty, count) => {
                let elem_layout = elem_ty.layout();
                let elem_size = elem_layout.assert_size();

                let count = count.assert_static();

                Layout {
                    maybe_size: Some(elem_size * count),
                    align: elem_layout.align,
                    field_offsets: Vec::new(),
                }
            }
            TypeKind::Slice(elem_ty) => {
                let elem_layout = elem_ty.layout();

                Layout {
                    maybe_size: None,
                    align: elem_layout.align,
                    field_offsets: Vec::new(),
                }
            }
            TypeKind::StringSlice => Layout {
                maybe_size: None,
                align: 1,
                field_offsets: Vec::new(),
            },

            TypeKind::Ref(ref_ty, _) | TypeKind::Ptr(ref_ty, _) => {
                let ptr_size = POINTER_SIZE.bytes();

                if ref_ty.is_sized() {
                    Layout::simple(ptr_size)
                } else {
                    Self {
                        maybe_size: Some(ptr_size * 2),
                        align: ptr_size,
                        field_offsets: Vec::new(),
                    }
                }
            }
            TypeKind::Adt(ItemWithSubs { item, subs }) => {
                let info = item.adt_info();

                let fixed_fields = info
                    .variant_fields
                    .iter()
                    .map(|fields| fields.iter().map(|field| field.sub(subs)));

                Layout::compound(fixed_fields, info.discriminant_ty())
            }
            TypeKind::FunctionDef(_) => Layout::simple(0),
            TypeKind::Never => Layout::simple(0),
            _ => panic!("can't layout: {:?}", kind),
        }
    }

    fn compound<'vm>(
        variants: impl Iterator<Item = impl Iterator<Item = Type<'vm>>>,
        discriminator_ty: Option<Type<'vm>>,
    ) -> Self {
        let mut full_size = 0;
        let mut align = 1;

        let base_size = if let Some(discriminator_ty) = discriminator_ty {
            let dl = discriminator_ty.layout();
            align = dl.align;
            dl.assert_size()
        } else {
            0
        };

        let field_offsets = variants
            .map(|fields| {
                let mut size = base_size;
                let res = fields
                    .map(|ty| {
                        let layout = Layout::from(ty);

                        size = crate::abi::align(size, layout.align);

                        let offset = size;

                        // todo unsized structs
                        size += layout.assert_size();
                        align = align.max(layout.align);

                        offset
                    })
                    .collect();
                full_size = full_size.max(size);
                res
            })
            .collect();

        full_size = crate::abi::align(full_size, align);

        Layout {
            maybe_size: Some(full_size),
            align,
            field_offsets,
        }
    }

    fn simple(size: u32) -> Self {
        Self {
            maybe_size: Some(size),
            align: size.max(1),
            field_offsets: Vec::new(),
        }
    }
}
