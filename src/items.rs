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

    pub fn add_item(&mut self, vm: &'vm VM<'vm>, kind: ItemKind<'vm>, path: ItemPath, did: rustc_hir::def_id::LocalDefId) -> ItemId {
        let item_id = ItemId(self.items.len() as u32);

        self.items.push(Item{
            vm,
            crate_id: self.crate_id,
            item_id,
            did,
            path: path.clone(),
            kind
        });

        // add to did map
        {
            let old = self.map_did_to_item.insert(did, item_id);
            assert!(old.is_none());
        }

        // add to path map
        if path.0 != NameSpace::DebugOnly {
            let old = self.map_path_to_item.insert(path, item_id);
            assert!(old.is_none());
        }

        item_id
    }

    pub fn get(&self, id: ItemId) -> &Item<'vm> {
        &self.items[id.index()]
    }

    pub fn set_path(&mut self, path: ItemPath, id: ItemId) {
        let old = self.map_path_to_item.insert(path, id);
        assert!(old.is_none());
    }

    pub fn find_by_path(&'vm self, path: &ItemPath) -> Option<&'vm Item<'vm>> {
        self.map_path_to_item.get(path).map(|item_id| {
            &self.items[item_id.index()]
        })
    }

    pub fn find_by_did(&'vm self, did: rustc_hir::def_id::DefId) -> Option<&'vm Item<'vm>> {
        if did.krate == rustc_hir::def_id::LOCAL_CRATE {
            let local_did = rustc_hir::def_id::LocalDefId{local_def_index: did.index};
            self.map_did_to_item.get(&local_did).map(|item_id| {
                &self.items[item_id.index()]
            })
        } else {
            None
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct ItemPath(NameSpace,String);

impl ItemPath {
    pub fn new_type(path: String) -> Self {
        ItemPath(NameSpace::Type,path)
    }

    pub fn new_value(path: String) -> Self {
        ItemPath(NameSpace::Value,path)
    }

    pub fn new_debug(path: String) -> Self {
        ItemPath(NameSpace::DebugOnly,path)
    }

    pub fn as_string(&self) -> &str {
        &self.1
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
enum NameSpace {
    /// Structs, Enums, etc.
    Type,
    /// Functions
    Value,
    /// Not used for real paths (impls and ???)
    DebugOnly
}

#[derive(PartialEq,Clone,Copy)]
pub struct CrateId(u32);

impl CrateId {
    pub fn new(n: u32) -> Self {
        Self(n)
    }

    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(PartialEq,Clone,Copy)]
pub struct ItemId(u32);

impl ItemId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

pub struct Item<'vm> {
    pub vm: &'vm VM<'vm>,
    crate_id: CrateId,
    item_id: ItemId,
    pub did: rustc_hir::def_id::LocalDefId,
    pub path: ItemPath,
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
    },
    Adt{
        fields: OnceLock<Vec<Type<'vm>>>
    }
}

impl<'vm> ItemKind<'vm> {
    pub fn new_function() -> Self {
        Self::Function{
            ir: Default::default(),
            mono_instances: Default::default()
        }
    }

    pub fn new_adt() -> Self {
        Self::Adt{
            fields: Default::default()
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
*/* */
impl<'vm> Item<'vm> {
    pub fn get_function(&'vm self, subs: &[Sub<'vm>]) -> &'vm Function<'vm> {

        let ItemKind::Function{mono_instances,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        let mut mono_instances = mono_instances.lock().unwrap();
        mono_instances.entry(subs.to_owned()).or_insert_with(|| {
            self.vm.alloc_function(self, subs.to_owned())
        })
    }

    /// Get the IR for a function. Subs are used to find specialized IR for trait methods.
    pub fn get_ir(&self, subs: &[Sub<'vm>]) -> Arc<IRFunction<'vm>> {
        
        let ItemKind::Function{ir,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        /*if let Some(parent_trait) = parent_trait {
            println!("-> {:?}",subs);
            panic!("todo trait lookup!");
        }*/

        loop {
            {
                let ir = ir.lock().unwrap();
                if let Some(ir) = ir.as_ref() {
                    return ir.clone();
                }
            }

            self.vm.build_function_ir(self.crate_id,self.item_id);
        }
    }

    pub fn set_ir(&self, new_ir: IRFunction<'vm>) {

        let ItemKind::Function{ir,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        let mut dest = ir.lock().unwrap();

        *dest = Some(Arc::new(new_ir));
    }

    pub fn get_adt_fields(&self) -> &Vec<Type<'vm>> {
        
        let ItemKind::Adt{fields} = &self.kind else {
            panic!("item kind mismatch");
        };

        fields.get().expect("adt missing fields")
    }

    pub fn set_adt_fields(&self, new_fields: Vec<Type<'vm>>) {
        let ItemKind::Adt{fields} = &self.kind else {
            panic!("item kind mismatch");
        };

        fields.set(new_fields).ok();
    }
}
