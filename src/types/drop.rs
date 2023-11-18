use crate::{
    crate_provider::TraitImplResult,
    items::{AssocValue, Item, ItemPath},
    types::{Sub, SubList, Type},
};

use super::TypeKind;

// Jargon:
// Drop "glue" refers to all the code required to drop a type, which may include a Drop impl and code to drop fields.
//
// A drop "leaf" is a type that must be either fully live or fully dead, either because it implements Drop or for some other reason.
//
// A drop "branch" is a type that contains fields that are Drop, but is not Drop itself. Some of its fields may be dead, and some may be live.

/// Contains some metadata about drops, which is used both to compile functions, and to
/// generate drop "glue". Glue is stored separately in the type.
pub struct DropInfo<'vm> {
    /// Marks this type as a drop "leaf" even if it lacks an impl or fields.
    /// Used for:
    /// - Arrays
    is_special_leaf: bool,

    drop_impl: Option<i32>,
    fields: Vec<DropField<Type<'vm>>>,
}

pub struct DropField<T> {
    variant: u32,
    field: u32,
    ty: T,
}

impl<'vm> DropInfo<'vm> {
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

    // kill me
    pub fn is_none(&self) -> bool {
        true
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
                | TypeKind::Never => return DropInfo::empty(),

                TypeKind::Array(child, _) => {
                    return if !child.drop_info().is_none() {
                        // drop array of T
                        DropInfo::special_leaf()
                    } else {
                        DropInfo::empty()
                    };
                }

                TypeKind::Tuple(..) => {
                    // todo check fields
                    DropInfo::empty()
                }
                TypeKind::Closure(closure, subs) => {
                    // todo use captures
                    DropInfo::empty()
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
