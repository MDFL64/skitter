use crate::{
    crate_provider::TraitImplResult,
    items::{AssocValue, Item, ItemPath},
    types::{Sub, SubList, Type},
    variants::VariantIndex,
    vm::Function,
};

use super::TypeKind;

// TODO, this is just a function. This function:
// 1. Calls drop, if applicable.
// 2. Drops all fields, if applicable.
#[derive(Clone)]
struct DropGlue<'vm>(&'vm Function<'vm>);

// Jargon:
// Drop "glue" refers to all the code required to drop a type, which may include a Drop impl and code to drop fields.
//
// A drop "leaf" is a type that must be either fully live or fully dead, either because it implements Drop or for some other reason.
//
// A drop "branch" is a type that contains fields that are Drop, but is not Drop itself. Some of its fields may be dead, and some may be live.

/// Contains some metadata about drops, which is used both to compile functions, and to
/// generate drop "glue". Glue is stored separately in the type.
#[derive(Clone)]
pub enum DropInfo<'vm> {
    None,
    Leaf(DropGlue<'vm>),
    Branch {
        glue: DropGlue<'vm>,
        fields: Vec<DropField<'vm>>,
    },
}

#[derive(Clone, Debug)]
pub struct DropField<'vm> {
    variant: VariantIndex,
    field: u32,
    ty: Type<'vm>,
}

impl<'vm> DropInfo<'vm> {
    fn new(
        drop_fn: Option<&'vm Item<'vm>>,
        variant_fields: &[Vec<Type<'vm>>],
        subs: &SubList<'vm>,
    ) -> Self {
        let mut fields = Vec::new();

        for (variant, field_tys) in variant_fields.iter().enumerate() {
            let variant = VariantIndex::new(variant as u32);

            for (field_index, ty) in field_tys.iter().enumerate() {
                let ty = ty.sub(subs);
                if ty.drop_info().is_drop() {
                    fields.push(DropField {
                        variant,
                        field: field_index as u32,
                        ty,
                    })
                }
            }
        }

        if fields.len() > 0 || drop_fn.is_some() {
            let glue = DropGlue::new(drop_fn, &fields);

            if drop_fn.is_some() {
                DropInfo::Leaf(glue)
            } else {
                DropInfo::Branch { glue, fields }
            }
        } else {
            DropInfo::None
        }
    }

    /// Returns true if ANY drop-related actions are required.
    pub fn is_drop(&self) -> bool {
        !self.is_none()
    }

    pub fn is_none(&self) -> bool {
        match self {
            Self::None => true,
            _ => false,
        }
    }
}

impl<'vm> Type<'vm> {
    pub fn drop_info(&self) -> &DropInfo<'vm> {
        self.0.drop_info.get_or_init(|| {
            // gather fields, bail for types we can't drop
            match self.kind() {
                TypeKind::Bool
                | TypeKind::Char
                | TypeKind::Int(..)
                | TypeKind::Float(_)
                | TypeKind::Ref(..)
                | TypeKind::Ptr(..)
                | TypeKind::FunctionPointer(..)
                | TypeKind::FunctionDef(..)
                | TypeKind::Never => DropInfo::None,

                TypeKind::Array(child, _) => {
                    if child.drop_info().is_drop() {
                        panic!("array drop");
                    } else {
                        DropInfo::None
                    }
                }

                TypeKind::Tuple(fields) => {
                    DropInfo::new(None, std::slice::from_ref(fields), &SubList::empty())
                }
                TypeKind::Closure(closure, subs) => {
                    let env = closure.env(subs);
                    env.drop_info().clone()
                }
                TypeKind::Adt(info) => {
                    let adt_info = info.item.adt_info();
                    let drop_fn = self.1.find_drop(*self);

                    DropInfo::new(drop_fn, adt_info.variant_fields.as_slice(), &info.subs)
                }
                _ => panic!("drop info: {:?}", self),
            }
        })
    }
}

impl<'vm> DropGlue<'vm> {
    pub fn new(drop_fn: Option<&'vm Item<'vm>>, fields: &[DropField<'vm>]) -> Self {
        println!("{:?} {:?}", drop_fn, fields);
        panic!("new drop glue");
    }
}
