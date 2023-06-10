use std::sync::Arc;

use crate::{
    ir::IRFunction,
    items::{Item, ItemId, ItemPath},
    types::Type,
};

pub trait CrateProvider<'vm>: Send + Sync + 'vm {
    /// Find an item by id. Generally used to retrieve local items.
    fn item_by_id(&self, id: ItemId) -> &'vm Item<'vm>;

    /// Find an item by path. Generally used for external items.
    fn item_by_path(&self, path: &ItemPath) -> Option<&'vm Item<'vm>>;

    /// Retrieve the IR for a function OR constant.
    fn build_ir(&self, id: ItemId) -> Arc<IRFunction<'vm>>;

    /// Populate inherent impls of a type.
    /// The type must be declared in this crate, with the exception
    /// of primitive types, which have impls in core.
    fn fill_inherent_impls(&self, ty: Type<'vm>);
}
