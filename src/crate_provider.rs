use std::sync::Arc;

use crate::{
    ir::IRFunction,
    items::{Item, ItemId, ItemPath},
};

pub trait CrateProvider<'vm>: Send + Sync + 'vm {
    fn item_by_id(&self, id: ItemId) -> &'vm Item<'vm>;

    fn item_by_path(&self, path: &ItemPath) -> Option<&'vm Item<'vm>>;

    fn build_ir(&self, id: ItemId) -> Arc<IRFunction<'vm>>;
}
