use std::{sync::Mutex, collections::HashMap};

use colosseum::sync::Arena;

use crate::vm::VM;

#[derive(Copy,Clone)]
pub struct Item<'vm>(&'vm InternedItem<'vm>);

pub struct ItemContext<'vm>{
    table: Mutex<HashMap<(CrateId,String),Item<'vm>>>,
    arena: Arena<InternedItem<'vm>>
}

#[derive(Hash,Eq,PartialEq,Copy,Clone)]
pub struct CrateId(u32);

impl CrateId {
    pub fn new(n: u32) -> Self {
        Self(n)
    }
}

impl<'vm> ItemContext<'vm> {
    pub fn new() -> Self {
        ItemContext{
            table: Default::default(),
            arena: Arena::new()
        }
    }

    pub fn get_item(&'vm self, crate_id: CrateId, path: String) -> Item<'vm> {
        let mut table = self.table.lock().unwrap();
        let entry = table.entry((crate_id,path));

        entry.or_insert_with(|| {
            let intern_ref = self.arena.alloc(Default::default());
            Item(intern_ref)
        }).clone()
    }
}

#[derive(Default)]
struct InternedItem<'vm> {
    vm: Option<&'vm VM<'vm>>
}
