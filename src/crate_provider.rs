use std::sync::Arc;

use crate::{
    ir::IRFunction,
    items::{AdtInfo, Item, ItemId, ItemPath, AssocValue},
    types::{Type, SubList},
};

pub trait CrateProvider<'vm>: Send + Sync + 'vm {
    /// Find an item by id. Generally used to retrieve local items.
    fn item_by_id(&self, id: ItemId) -> &'vm Item<'vm>;

    /// Find an item by path. Generally used for external items.
    fn item_by_path(&self, path: &ItemPath<'vm>) -> Option<&'vm Item<'vm>>;

    /// Retrieve the IR for a function OR constant.
    fn build_ir(&self, id: ItemId) -> Arc<IRFunction<'vm>>;

    fn build_adt(&self, id: ItemId) -> AdtInfo<'vm>;

    /// Populate inherent impls of a type.
    /// The type must be declared in this crate, with the exception
    /// of primitive types, which have impls in core.
    fn fill_inherent_impls(&self, ty: Type<'vm>);

    fn trait_impl(&self, trait_item: &Item, for_tys: &SubList<'vm>) -> Option<&[Option<AssocValue<'vm>>]>;
}
