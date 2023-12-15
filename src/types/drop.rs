use crate::{
    crate_provider::TraitImplResult,
    items::{AssocValue, Item, ItemPath},
    types::{Sub, SubList, Type}, variants::VariantIndex,
};

use super::TypeKind;

// TODO, this is just a function. This function:
// 1. Calls drop, if applicable.
// 2. Drops all fields, if applicable.
struct DropGlue;

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
    Leaf(DropGlue),
    Branch {
        glue: DropGlue,
        fields: Vec<DropField>
    }
}

#[derive(Clone)]
pub struct DropField<'vm> {
    variant: VariantIndex,
    field: u32,
    ty: Type<'vm>,
}

impl<'vm> DropInfo<'vm> {
    fn new(drop_impl: Option<&'vm Item<'vm>>, fields: Vec<DropField<'vm>>) -> Self {
        Self {
            is_special_leaf: false,
            drop_impl,
            fields,
        }
    }

    fn for_tuple(fields: &[Type<'vm>]) -> Self {
        let fields = fields
            .iter()
            .copied()
            .enumerate()
            .filter_map(|(i, ty)| {
                if ty.drop_info().is_drop() {
                    Some(DropField {
                        variant: 0,
                        field: i as u32,
                        ty,
                    })
                } else {
                    None
                }
            })
            .collect();

        DropInfo::new(None, fields)
    }

    fn empty() -> Self {
        Self {
            is_special_leaf: false,
            drop_impl: None,
            fields: vec![],
        }
    }

    fn special_leaf() -> Self {
        Self {
            is_special_leaf: true,
            drop_impl: None,
            fields: vec![],
        }
    }

    /// Returns true if ANY drop-related actions are required.
    pub fn is_drop(&self) -> bool {
        self.is_special_leaf || self.drop_impl.is_some() || self.fields.len() > 0
    }
}

fn get_drop_impl<'vm>(ty: Type<'vm>) -> Option<&'vm Item<'vm>> {
    let vm = ty.1;

    let core_id = *vm.core_crate.get().expect("no core crate");
    let core_provider = vm.crate_provider(core_id);
    let drop_trait = core_provider
        .item_by_path(&ItemPath::for_type("::ops::drop::Drop"))
        .expect("no drop trait");

    let drop_impl = drop_trait.find_trait_impl(&SubList {
        list: vec![Sub::Type(ty)],
    });

    match drop_impl {
        TraitImplResult::Dynamic => {
            panic!("dyn drop, is this even valid?");
        }
        TraitImplResult::Static(drop_impl) => {
            let val = drop_impl.assoc_values[0].as_ref().unwrap();
            let AssocValue::Item(item_id) = val else {
                panic!("drop impl is not an item");
            };

            let provider = vm.crate_provider(drop_impl.crate_id);
            let item = provider.item_by_id(*item_id);
            Some(item)
        }
        TraitImplResult::None => None,
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
                | TypeKind::Never => DropInfo::empty(),

                TypeKind::Array(child, _) => {
                    if child.drop_info().is_drop() {
                        // drop array of T
                        DropInfo::special_leaf()
                    } else {
                        DropInfo::empty()
                    }
                }

                TypeKind::Tuple(fields) => DropInfo::for_tuple(fields),
                TypeKind::Closure(closure, subs) => {
                    let env = closure.env(subs);
                    env.drop_info().clone()
                }
                TypeKind::Adt(..) => {
                    get_drop_impl(*self);
                    // todo check fields
                    DropInfo::empty()
                }
                _ => panic!("drop info: {:?}", self),
            }
        })
    }
}
