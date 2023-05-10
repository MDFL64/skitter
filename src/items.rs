use std::{sync::{Mutex, Arc, OnceLock}, collections::HashMap, hash::Hash};

use colosseum::sync::Arena;

use crate::{vm::{VM, Function}, ir::IRFunction, types::{Sub, Type}};

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

    /*pub fn init_item(&'vm self, vm: &'vm VM<'vm>, crate_id: CrateId, path: &str, kind: ItemKind<'vm>) -> Item<'vm> {
        /*let interned = self.arena.alloc(InternedItem{
            kind,
            crate_id,
            path: path.to_owned()
        });

        let item = Item(interned,vm);

        let mut table = self.table.write().unwrap();
        let old = table.insert((crate_id,path.to_owned()),item);
        assert!(old.is_none());

        item*/
    }*/

    pub fn get(&'vm self, crate_id: CrateId, path: &str, vm: &'vm VM<'vm>) -> Item<'vm> {
        let mut table = self.table.lock().unwrap();

        *table.entry((crate_id,path.to_owned())).or_insert_with(|| {
            let interned = self.arena.alloc(InternedItem{
                kind: OnceLock::new(),
                crate_id,
                path: path.to_owned()
            });

            Item(interned,vm)
        })
    }
}

struct InternedItem<'vm>{
    kind: OnceLock<ItemKind<'vm>>,
    crate_id: CrateId,
    path: String
}

pub enum ItemKind<'vm> {
    Function{
        ir: Mutex<Option<Arc<IRFunction<'vm>>>>,
        mono_instances: Mutex<HashMap<Vec<Sub<'vm>>,&'vm Function<'vm>>>,
        parent_trait: Option<Item<'vm>>
    },
    Adt{
        fields: Vec<Type<'vm>>
    }
}

impl<'vm> ItemKind<'vm> {
    pub fn new_function(parent_trait: Option<Item<'vm>>) -> Self {
        Self::Function {
            ir: Default::default(),
            mono_instances: Default::default(),
            parent_trait
        }
    }
}

impl<'vm> Item<'vm> {
    pub fn vm(&self) -> &'vm VM<'vm> {
        self.1
    }

    fn kind(&self) -> &'vm ItemKind<'vm> {
        self.0.kind.get().expect("item not initialized")
    }

    pub fn init(&self, kind: ItemKind<'vm>) {
        self.0.kind.set(kind).ok().expect("item already initialized");
    }

    pub fn get_function(&self, subs: &[Sub<'vm>]) -> &'vm Function<'vm> {

        let ItemKind::Function{mono_instances,..} = self.kind() else {
            panic!("item kind mismatch");
        };

        let mut mono_instances = mono_instances.lock().unwrap();
        mono_instances.entry(subs.to_owned()).or_insert_with(|| {
            self.vm().alloc_function(*self, subs.to_owned())
        })
    }

    /*pub fn set_trait_base_method(&self, parent_trait: Item<'vm>) {
        let res = self.0.kind.set(ItemKind::Function{
            ir: Mutex::new(None),
            mono_instances: Mutex::new(HashMap::new()),
            parent_trait: Some(parent_trait)
        });
        assert!(res.is_ok());
    }*/

    /// Get the IR for a function. Subs are used to find specialized IR for trait methods.
    pub fn get_ir(&self, subs: &[Sub<'vm>]) -> Arc<IRFunction<'vm>> {
        
        let ItemKind::Function{ir,parent_trait,..} = self.kind() else {
            panic!("item kind mismatch");
        };

        if let Some(parent_trait) = parent_trait {
            println!("-> {:?}",subs);
            panic!("todo trait lookup!");
        }

        loop {
            {
                let ir = ir.lock().unwrap();
                if let Some(ir) = ir.as_ref() {
                    return ir.clone();
                }
            }

            self.vm().build_function_ir(self.0.crate_id,self.0.path.clone());
        }
    }

    pub fn set_ir(&self, new_ir: IRFunction<'vm>) {

        let ItemKind::Function{ir,..} = self.kind() else {
            panic!("item kind mismatch");
        };

        let mut dest = ir.lock().unwrap();

        *dest = Some(Arc::new(new_ir));
    }

    pub fn get_adt_fields(&self) -> &Vec<Type<'vm>> {
        
        let ItemKind::Adt{fields} = self.kind() else {
            panic!("item kind mismatch");
        };

        fields
    }
}
