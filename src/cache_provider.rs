use std::{
    error::Error,
    sync::{Arc, OnceLock},
};

use ahash::AHashMap;

use crate::{
    crate_provider::{CrateProvider, TraitImpl},
    impls::{ImplTable, ImplTableLazy},
    ir::IRFunction,
    items::{read_item_ir, AdtInfo, AssocValue, CrateId, Item, ItemId, ItemPath},
    lazy_collections::{LazyArray, LazyTable},
    persist::{Persist, PersistReadContext, PersistReader},
    persist_header::{persist_header_read, PersistCrateHeader},
    rustc_worker::RustCWorkerConfig,
    types::{SubList, Type},
    vm::VM,
};

pub struct CacheProvider<'vm> {
    read_context: Arc<PersistReadContext<'vm>>,
    impls: ImplTableLazy<'vm>,
}

impl<'vm> CacheProvider<'vm> {
    pub fn new(
        config: &RustCWorkerConfig,
        vm: &'vm VM<'vm>,
        this_crate: CrateId,
    ) -> Result<Self, Box<dyn Error>> {
        let bytes = std::fs::read(config.crate_path.cache_path())?;
        let bytes = vm.alloc_constant(bytes);

        let read_context = Arc::new(PersistReadContext {
            this_crate,
            vm,
            types: OnceLock::new(),
            items: OnceLock::new(),
            crate_id_map: OnceLock::new(),
        });

        let mut reader = PersistReader::new(bytes, read_context.clone());
        persist_header_read(&mut reader)?;

        let crate_header = PersistCrateHeader::persist_read(&mut reader);
        crate_header.validate()?;

        if let Some(root_file) = crate_header.files.get(0) {
            if root_file.path != config.crate_path.source_path() {
                return Err("cache file refers to incorrect source file".into());
            }
        }

        // crate id mapping
        let mut crate_id_map = AHashMap::new();
        {
            let this_crate_src = u32::persist_read(&mut reader);
            crate_id_map.insert(this_crate_src, this_crate);
            let dep_crates = Vec::<(&str, u32)>::persist_read(&mut reader);
            for (name, src_id) in dep_crates {
                if let Some(dst_id) = config.find_extern(name) {
                    crate_id_map.insert(src_id, dst_id);
                } else {
                    panic!("crate '{}' was not found.", name);
                }
            }
        }
        read_context
            .crate_id_map
            .set(crate_id_map)
            .map_err(|_| "double-assign to crate ids")?;

        let items = LazyTable::read(&mut reader);
        read_context
            .items
            .set(items)
            .map_err(|_| "double-assign to items")?;

        let impls = ImplTableLazy::new(&mut reader);

        let types = LazyArray::read(&mut reader);
        read_context
            .types
            .set(types)
            .map_err(|_| "double-assign to types")?;

        Ok(Self {
            read_context,
            impls,
        })
    }
}

impl<'vm> CrateProvider<'vm> for CacheProvider<'vm> {
    fn item_by_id(&self, id: ItemId) -> &'vm Item<'vm> {
        let items = self.read_context.items.get().unwrap();
        items.array.get(id.index())
    }

    fn item_by_path(&self, path: &ItemPath<'vm>) -> Option<&'vm Item<'vm>> {
        let items = self.read_context.items.get().unwrap();
        items.get(path).copied()
    }

    fn build_ir(&self, id: ItemId) -> Arc<IRFunction<'vm>> {
        let items = self.read_context.items.get().unwrap();
        let item = items.array.get(id.index());

        let saved_data = item.saved_data.expect("item is missing ir block");
        let mut reader = PersistReader::new(saved_data, self.read_context.clone());
        let ir = read_item_ir(&mut reader, item);

        if let Some(ir) = ir {
            ir
        } else {
            panic!("no ir available for {:?}", item);
        }
    }

    fn build_adt(&self, id: ItemId) -> AdtInfo<'vm> {
        let items = self.read_context.items.get().unwrap();
        let item = items.array.get(id.index());

        if let Some(saved_info) = item.saved_data {
            let mut reader = PersistReader::new(saved_info, self.read_context.clone());
            AdtInfo::persist_read(&mut reader)
        } else {
            panic!("no adt info available for {:?}", id);
        }
    }

    fn trait_impl(&self, trait_item: &Item<'vm>, for_tys: &SubList<'vm>) -> Option<TraitImpl<'vm>> {
        self.impls.find_trait(trait_item, for_tys)
    }

    fn inherent_impl(&self, full_key: &str, ty: Type<'vm>) -> Option<AssocValue<'vm>> {
        self.impls.find_inherent(full_key, ty)
    }
}
