use crate::{
    crate_provider::TraitImplResult,
    items::ItemPath,
    types::{Sub, SubList, Type},
};

use super::TypeKind;

pub enum DropInfo<'vm> {
    Leaf,
    Branch(Vec<DropField<Type<'vm>>>),
    Array(Type<'vm>),
    None,
}

impl<'vm> DropInfo<'vm> {
    pub fn is_none(&self) -> bool {
        match self {
            DropInfo::None => true,
            _ => false,
        }
    }
}

pub struct DropField<T> {
    variant: u32,
    field: u32,
    ty: T,
}

impl<'vm> Type<'vm> {
    pub fn drop_info(&self) -> &DropInfo<'vm> {
        self.0.drop_info.get_or_init(|| {
            // PRE CHECK: bail for types we definitely can't drop
            match self.kind() {
                TypeKind::Bool
                | TypeKind::Char
                | TypeKind::Int(..)
                | TypeKind::Float(_)
                | TypeKind::Ref(..)
                | TypeKind::Ptr(..)
                | TypeKind::FunctionPointer(..) => return DropInfo::None,
                TypeKind::Array(child, _) => {
                    return if !child.drop_info().is_none() {
                        DropInfo::Array(*child)
                    } else {
                        DropInfo::None
                    }
                }
                TypeKind::Tuple(..) => (), // CHECK FIELDS / NO TRAIT?
                TypeKind::Adt(..) => (), // CHECK FIELDS AND TRAIT
                _ => panic!("drop info: {}", self),
            }

            let vm = self.1;

            // TODO cache a ref to drop trait somewhere
            let core_id = *vm.core_crate.get().expect("no core crate");
            let core_provider = vm.crate_provider(core_id);
            let drop_trait = core_provider
                .item_by_path(&ItemPath::for_type("::ops::drop::Drop"))
                .expect("no drop trait");

            let drop_impl = drop_trait.find_trait_impl(&SubList {
                list: vec![Sub::Type(*self)],
            });

            match drop_impl {
                TraitImplResult::Dynamic => panic!("dyn drop"),
                TraitImplResult::Static(_) => {
                    // TODO
                    DropInfo::Leaf
                }
                TraitImplResult::None => DropInfo::None,
            }
        })
    }
}
