use std::{sync::{Mutex, Arc, OnceLock}, collections::HashMap, hash::Hash};

use crate::{vm::{VM, Function}, ir::IRFunction, types::{Sub, Type}};
use ahash::AHashMap;

pub struct CrateItems<'vm> {
    crate_id: CrateId,
    items: Vec<Item<'vm>>,
    map_path_to_item: AHashMap<ItemPath,ItemId>,
    map_did_to_item: AHashMap<rustc_hir::def_id::LocalDefId,ItemId>
}

impl<'vm> CrateItems<'vm> {
    pub fn new(crate_id: CrateId) -> Self {
        Self {
            crate_id,
            items: Default::default(),
            map_path_to_item: Default::default(),
            map_did_to_item: Default::default(),
        }
    }

    pub fn add_item(&mut self, kind: ItemKind<'vm>) -> ItemId {
        let item_id = ItemId(self.items.len() as u32);

        self.items.push(Item{
            crate_id: self.crate_id,
            item_id,
            kind
        });

        item_id
    }

    pub fn set_path(&mut self, path: ItemPath, id: ItemId) {
        let old = self.map_path_to_item.insert(path, id);
        assert!(old.is_none());
    }

    pub fn set_did(&mut self, did: rustc_hir::def_id::LocalDefId, id: ItemId) {
        let old = self.map_did_to_item.insert(did, id);
        assert!(old.is_none());
    }
}

#[derive(Eq, PartialEq, Hash)]
pub struct ItemPath(pub NameSpace,pub String);

#[derive(Eq, PartialEq, Hash)]
pub enum NameSpace {
    /// Structs, Enums, etc.
    Type,
    /// Functions
    Value
}

#[derive(PartialEq,Clone,Copy)]
pub struct CrateId(u32);

#[derive(PartialEq,Clone,Copy)]
pub struct ItemId(u32);

impl CrateId {
    pub fn new(n: u32) -> Self {
        Self(n)
    }
}

pub struct Item<'vm> {
    crate_id: CrateId,
    item_id: ItemId,
    kind: ItemKind<'vm>
}

impl<'vm> std::fmt::Debug for Item<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Item").finish()
    }
}

impl<'vm> PartialEq for Item<'vm> {
    fn eq(&self, other: &Self) -> bool {
        self.crate_id == other.crate_id &&
        self.item_id == other.item_id
    }
}

impl<'vm> Eq for Item<'vm> {}

impl<'vm> Hash for Item<'vm> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u32(self.crate_id.0);
        state.write_u32(self.item_id.0);
    }
}

pub enum ItemKind<'vm> {
    Function{
        ir: Mutex<Option<Arc<IRFunction<'vm>>>>,
        mono_instances: Mutex<HashMap<Vec<Sub<'vm>>,&'vm Function<'vm>>>,
        //parent_trait: Option<(String,Item<'vm>)>
    }
}

impl<'vm> ItemKind<'vm> {
    pub fn new_function() -> Self {
        Self::Function{
            ir: Default::default(),
            mono_instances: Default::default()
        }
    }
}

/*
/*#[derive(Copy,Clone)]
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
}*/

#[derive(Clone,Eq,PartialEq,Hash)]
pub struct ItemPathSimple {
    pub crate_id: CrateId,
    pub path: String,
}

pub struct ItemContext<'vm>{
    table_funcs: Mutex<HashMap<ItemPathSimple,&'vm ItemFunction<'vm>>>,
    arena_funcs: Arena<ItemFunction<'vm>>
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
            table_funcs: Default::default(),
            arena_funcs: Arena::new()
        }
    }

    pub fn get_func(&'vm self, path: &ItemPathSimple) -> &'vm ItemFunction<'vm> {
        let mut table = self.table_funcs.lock().unwrap();

        *table.entry(path.clone()).or_insert_with(|| {
            self.arena_funcs.alloc(Default::default())
        })
    }
}

#[derive(Default)]
pub struct ItemFunction<'vm> {
    ir: Mutex<Option<Arc<IRFunction<'vm>>>>,
    mono_instances: Mutex<HashMap<Vec<Sub<'vm>>,&'vm Function<'vm>>>,
    //parent_trait: Option<i32>
}

impl<'vm> std::fmt::Debug for ItemFunction<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ItemFunction").finish()
    }
}

impl<'vm> PartialEq for ItemFunction<'vm> {
    fn eq(&self, other: &Self) -> bool {
        let a = self as *const _;
        let b = other as *const _;
        a == b
    }
}

impl<'vm> Eq for ItemFunction<'vm> {}

impl<'vm> Hash for ItemFunction<'vm> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const _ as usize);
    }
}

/*struct InternedItem<'vm>{
    kind: OnceLock<ItemKind<'vm>>,
    crate_id: CrateId,
    path: String
}

pub enum ItemKind<'vm> {
    Function{
        ir: Mutex<Option<Arc<IRFunction<'vm>>>>,
        mono_instances: Mutex<HashMap<Vec<Sub<'vm>>,&'vm Function<'vm>>>,
        parent_trait: Option<(String,Item<'vm>)>
    },
    Adt{
        fields: Vec<Type<'vm>>
    }
}

impl<'vm> ItemKind<'vm> {
    pub fn new_function(parent_trait: Option<(String,Item<'vm>)>) -> Self {
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
}*/
*/
