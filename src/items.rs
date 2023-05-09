use std::{sync::{Mutex, RwLock, Arc, OnceLock, MutexGuard}, collections::HashMap, hash::Hash};

use colosseum::sync::Arena;

use crate::{vm::{VM, Function}, ir::IRFunction, types::Sub};

#[derive(Copy,Clone)]
pub struct Item<'vm>(&'vm InternedItem<'vm>, &'vm VM<'vm>);

impl<'vm> Item<'vm> {
    pub fn path(&self) -> &str {
        &self.0.path
    }
}

impl<'vm> std::fmt::Debug for Item<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Item").finish()
    }
}

impl<'vm> PartialEq for Item<'vm> {
    fn eq(&self, other: &Self) -> bool {
        let a = self.0 as *const _;
        let b = other.0 as *const _;
        a == b
    }
}

impl<'vm> Eq for Item<'vm> {}

impl<'vm> Hash for Item<'vm> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const _ as usize);
    }
}

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

    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl<'vm> ItemContext<'vm> {
    pub fn new() -> Self {
        ItemContext{
            table: Default::default(),
            arena: Arena::new()
        }
    }

    pub fn get_item(&'vm self, crate_id: CrateId, path: &str, vm: &'vm VM<'vm>) -> Item<'vm> {
        let mut table = self.table.lock().unwrap();
        let entry = table.entry((crate_id,path.to_owned()));

        entry.or_insert_with(|| {
            let intern_ref = self.arena.alloc(InternedItem::new(crate_id, path.to_owned()));
            Item(intern_ref,vm)
        }).clone()
    }
}

struct InternedItem<'vm>{
    kind: OnceLock<ItemKind<'vm>>,
    crate_id: CrateId,
    path: String
}

impl<'vm> InternedItem<'vm> {
    pub fn new(crate_id: CrateId, path: String) -> Self {
        Self{
            kind: OnceLock::new(),
            crate_id,
            path
        }
    }
}

enum ItemKind<'vm> {
    Function{
        ir: Mutex<Option<Arc<IRFunction<'vm>>>>,
        mono_instances: Mutex<HashMap<Vec<Sub<'vm>>,&'vm Function<'vm>>>
    }
}

impl<'vm> Item<'vm> {
    pub fn vm(&self) -> &'vm VM<'vm> {
        self.1
    }

    pub fn get_function(&self, subs: &[Sub<'vm>]) -> &'vm Function<'vm> {

        self.0.kind.get_or_init(|| {
            ItemKind::Function{
                ir: Mutex::new(None),
                mono_instances: Mutex::new(HashMap::new()),
            }
        });

        let Some(ItemKind::Function{mono_instances,..}) = self.0.kind.get() else {
            panic!("item kind mismatch");
        };

        let mut mono_instances = mono_instances.lock().unwrap();
        mono_instances.entry(subs.to_owned()).or_insert_with(|| {
            self.1.alloc_function(*self, subs.to_owned())
        })
    }

    pub fn get_ir(&self) -> Arc<IRFunction<'vm>> {
        
        let Some(ItemKind::Function{ir,..}) = self.0.kind.get() else {
            panic!("item kind mismatch");
        };

        loop {
            {
                let ir = ir.lock().unwrap();
                if let Some(ir) = ir.as_ref() {
                    return ir.clone();
                }
            }

            self.1.build_function_ir(self.0.crate_id,self.0.path.clone());
        }
    }

    pub fn set_ir(&self, new_ir: IRFunction<'vm>) {
        let Some(ItemKind::Function{ir,..}) = self.0.kind.get() else {
            panic!("item kind mismatch");
        };

        let mut dest = ir.lock().unwrap();

        *dest = Some(Arc::new(new_ir));
    }
}
