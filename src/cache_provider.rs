use std::{error::Error, sync::Arc, rc::Rc};

use crate::{items::{CrateId, ItemId, Item, ItemPath}, vm::VM, crate_provider::CrateProvider, types::Type, ir::IRFunction, persist::{PersistReader, Persist, PersistReadContext}, persist_header::{persist_header_read, PersistCrateHeader}, lazy_collections::LazyTable};


pub struct CacheProvider<'vm> {
    //crate_id: CrateId
    items: LazyTable<'vm,&'vm Item<'vm>>
}

impl<'vm> CacheProvider<'vm> {
    pub fn new(path: &str, vm: &'vm VM<'vm>, this_crate: CrateId) -> Result<Self,Box<dyn Error>> {
        let bytes = std::fs::read(path)?;
        let bytes = vm.alloc_constant(bytes);

        let read_context = Arc::new(PersistReadContext{
            this_crate,
            vm
        });

        let mut reader = PersistReader::new(bytes, read_context);
        persist_header_read(&mut reader)?;

        let crate_header = PersistCrateHeader::persist_read(&mut reader);
        crate_header.validate()?;

        let items = LazyTable::read(&mut reader);

        Ok(Self{
            items
        })
    }
}

impl<'vm> CrateProvider<'vm> for CacheProvider<'vm> {
    fn item_by_id(&self, id: ItemId) -> &'vm Item<'vm> {
        panic!("item_by_id")
    }

    fn item_by_path(&self, path: &ItemPath<'vm>) -> Option<&'vm Item<'vm>> {
        Some(self.items.get(path))
    }

    fn build_ir(&self, id: ItemId) -> Arc<IRFunction<'vm>> {
        panic!("build_ir")
    }

    fn fill_inherent_impls(&self, ty: Type<'vm>) {
        panic!("fill_inherent_impls")
    }
}
