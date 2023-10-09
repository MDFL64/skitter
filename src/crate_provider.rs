use std::sync::Arc;

use crate::{
    ir::IRFunction,
    items::{AdtInfo, AssocValue, CrateId, Item, ItemId, ItemPath},
    types::{SubList, Type},
};

pub trait CrateProvider<'vm>: Send + Sync + 'vm {
    /// Find an item by id. Generally used to retrieve local items.
    fn item_by_id(&self, id: ItemId) -> &'vm Item<'vm>;

    /// Find an item by path. Generally used for external items.
    fn item_by_path(&self, path: &ItemPath<'vm>) -> Option<&'vm Item<'vm>>;

    /// Retrieve the IR for a function OR constant.
    fn build_ir(&self, id: ItemId) -> Arc<IRFunction<'vm>>;

    fn build_adt(&self, id: ItemId) -> AdtInfo<'vm>;

    fn trait_impl(&self, trait_item: &Item<'vm>, for_tys: &SubList<'vm>) -> Option<TraitImpl<'vm>>;

    fn inherent_impl(&self, full_key: &str, ty: Type<'vm>) -> Option<AssocValue<'vm>>;
}

pub struct TraitImpl<'vm> {
    pub crate_id: CrateId,
    pub assoc_values: Arc<[Option<AssocValue<'vm>>]>,
    pub impl_subs: SubList<'vm>,
}

pub enum TraitImplResult<'vm> {
    Static(TraitImpl<'vm>),
    Dynamic,
    None,
}
