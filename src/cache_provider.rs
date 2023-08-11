use std::{error::Error, sync::{Arc, OnceLock}, rc::Rc};

use crate::{items::{CrateId, ItemId, Item, ItemPath}, vm::VM, crate_provider::CrateProvider, types::Type, ir::IRFunction, persist::{PersistReader, Persist, PersistReadContext}, persist_header::{persist_header_read, PersistCrateHeader}, lazy_collections::{LazyTable, LazyArray}};


pub struct CacheProvider<'vm> {
    read_context: Arc<PersistReadContext<'vm>>
}

impl<'vm> CacheProvider<'vm> {
    pub fn new(path: &str, vm: &'vm VM<'vm>, this_crate: CrateId) -> Result<Self,Box<dyn Error>> {
        let bytes = std::fs::read(path)?;
        let bytes = vm.alloc_constant(bytes);

        let read_context = Arc::new(PersistReadContext{
            this_crate,
            vm,
            types: OnceLock::new(),
            items: OnceLock::new()
        });

        let mut reader = PersistReader::new(bytes, read_context.clone());
        persist_header_read(&mut reader)?;

        let crate_header = PersistCrateHeader::persist_read(&mut reader);
        crate_header.validate()?;

        let items = LazyTable::read(&mut reader);
        assert!(read_context.items.set(items).is_ok());

        let types = LazyArray::read(&mut reader);
        assert!(read_context.types.set(types).is_ok());

        Ok(Self{
            read_context
        })
    }
}

impl<'vm> CrateProvider<'vm> for CacheProvider<'vm> {
    fn item_by_id(&self, id: ItemId) -> &'vm Item<'vm> {
        panic!("item_by_id")
    }

    fn item_by_path(&self, path: &ItemPath<'vm>) -> Option<&'vm Item<'vm>> {
        let items = self.read_context.items.get().unwrap();
        Some(items.get(path))
    }

    fn build_ir(&self, id: ItemId) -> Arc<IRFunction<'vm>> {
        let items = self.read_context.items.get().unwrap();
        let item = items.array.get(id.index());

        if let Some(saved_ir) = item.saved_ir {
            let mut reader = PersistReader::new(saved_ir, self.read_context.clone());
            let ir = IRFunction::persist_read(&mut reader);
            Arc::new(ir)
        } else {
            panic!("no ir available for {:?}",id);
        }
    }

    fn fill_inherent_impls(&self, ty: Type<'vm>) {
        panic!("fill_inherent_impls")
    }
}
