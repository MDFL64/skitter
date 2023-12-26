use crate::{
    abi::POINTER_SIZE,
    types::{IntWidth, Type, TypeKind},
    variants::Variants,
};

use super::{FloatWidth, ItemWithSubs};

#[derive(Debug, Clone)]
pub struct Layout {
    pub maybe_size: Option<u32>,
    pub align: u32,
    pub field_offsets: Variants<Vec<u32>>,
}

impl Layout {
    pub fn assert_size(&self) -> u32 {
        self.maybe_size.expect("unsized type not permitted")
    }

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

                let count = count.get_value() as u32;

                Layout {
                    maybe_size: Some(elem_size * count),
                    align: elem_layout.align,
                    field_offsets: Variants::empty(),
                }
            }
            TypeKind::Slice(elem_ty) => {
                let elem_layout = elem_ty.layout();

                Layout {
                    maybe_size: None,
                    align: elem_layout.align,
                    field_offsets: Variants::empty(),
                }
            }
            TypeKind::StringSlice => Layout {
                maybe_size: None,
                align: 1,
                field_offsets: Variants::empty(),
            },

            TypeKind::Ref(ref_ty, _) | TypeKind::Ptr(ref_ty, _) => {
                let ptr_size = POINTER_SIZE.bytes();

                if ref_ty.is_sized() {
                    Layout::simple(ptr_size)
                } else {
                    Self {
                        maybe_size: Some(ptr_size * 2),
                        align: ptr_size,
                        field_offsets: Variants::empty(),
                    }
                }
            }
            TypeKind::FunctionPointer(_) => {
                let ptr_size = POINTER_SIZE.bytes();
                Layout::simple(ptr_size)
            }

            TypeKind::Adt(ItemWithSubs { item, subs }) => {
                let info = item.adt_info();

                if info.is_union() {
                    let mut align = 1;
                    let mut size = 0;

                    for ty in info.variant_fields.assert_single().iter() {
                        let layout = ty.sub(subs).layout();

                        align = align.max(layout.align);
                        size = size.max(layout.assert_size());
                    }

                    size = crate::abi::align(size, align);

                    let field_count = info.variant_fields.assert_single().len();

                    let field_offsets = Variants::new(vec![vec![0; field_count]]);

                    Layout {
                        maybe_size: Some(size),
                        align,
                        field_offsets,
                    }
                } else {
                    let fixed_fields = info
                        .variant_fields
                        .iter()
                        .map(|(_, fields)| fields.iter().map(|field| field.sub(subs)));

                    let discriminant_ty = info.enum_info().map(|e| e.discriminant_internal);

                    Layout::compound(fixed_fields, discriminant_ty)
                }
            }
            TypeKind::FunctionDef(_) => Layout::simple(0),
            TypeKind::Never => Layout::simple(0),
            TypeKind::Closure(closure, subs) => {
                let env_ty = closure.env(subs);

                assert!(env_ty.is_concrete());
                env_ty.layout().clone()
            }
            _ => panic!("can't layout: {:?}", kind),
        }
    }

    fn compound<'vm>(
        variants: impl Iterator<Item = impl Iterator<Item = Type<'vm>>>,
        discriminant_ty: Option<Type<'vm>>,
    ) -> Self {
        let mut full_size = 0;
        let mut align = 1;

        let base_size = if let Some(discriminant_ty) = discriminant_ty {
            let dl = discriminant_ty.layout();
            align = dl.align;
            dl.assert_size()
        } else {
            0
        };

        let mut is_unsized = false;

        let field_offsets = variants
            .map(|fields| {
                let mut size = base_size;
                let res = fields
                    .map(|ty| {
                        let layout = ty.layout();

                        size = crate::abi::align(size, layout.align);

                        let offset = size;

                        if let Some(field_size) = layout.maybe_size {
                            size += field_size;
                        } else {
                            is_unsized = true;
                        }
                        align = align.max(layout.align);

                        offset
                    })
                    .collect();
                full_size = full_size.max(size);
                res
            })
            .collect();

        let field_offsets = Variants::new(field_offsets);

        let maybe_size = if is_unsized {
            None
        } else {
            Some(crate::abi::align(full_size, align))
        };

        Layout {
            maybe_size,
            align,
            field_offsets,
        }
    }

    fn simple(size: u32) -> Self {
        Self {
            maybe_size: Some(size),
            align: size.max(1),
            field_offsets: Variants::empty(),
        }
    }
}
