use std::{sync::{Mutex, Arc, OnceLock, RwLock}, collections::HashMap, hash::Hash};

use crate::{vm::{VM, Function}, ir::IRFunction, types::{Sub, Type}};
use ahash::AHashMap;

pub struct CrateItems<'vm> {
    crate_id: CrateId,
    items: Vec<Item<'vm>>,
    map_path_to_item: AHashMap<ItemPath,ItemId>,
    map_did_to_item: AHashMap<rustc_hir::def_id::LocalDefId,ItemId>,

    extern_crate_list: Vec<ExternCrate>,
    extern_crate_id_cache: Mutex<AHashMap<rustc_span::def_id::CrateNum,CrateId>>
}

pub struct ExternCrate {
    pub name: String,
    pub id: CrateId
}

impl<'vm> CrateItems<'vm> {
    pub fn new(crate_id: CrateId, extern_crate_list: Vec<ExternCrate>) -> Self {
        Self {
            crate_id,
            items: Default::default(),
            map_path_to_item: Default::default(),
            map_did_to_item: Default::default(),
            extern_crate_list,
            extern_crate_id_cache: Default::default(),
        }
    }

    pub fn count(&self) -> usize {
        self.items.len()
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

    pub fn find_crate_id<'tcx>(&self, tcx: rustc_middle::ty::TyCtxt<'tcx>, crate_num: rustc_span::def_id::CrateNum) -> CrateId {

        let mut cache = self.extern_crate_id_cache.lock().unwrap();

        *cache.entry(crate_num).or_insert_with(|| {
            let crate_name = tcx.crate_name(crate_num);
            for entry in &self.extern_crate_list {
                if entry.name == crate_name.as_str() {
                    return entry.id;
                }
            }
            panic!("lookup for crate failed: {} {}",crate_num,crate_name.as_str());
        })
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
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

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
enum NameSpace {
    /// Structs, Enums, etc.
    Type,
    /// Functions
    Value,
    /// Not used for real paths (impls and ???)
    DebugOnly
}

#[derive(PartialEq,Clone,Copy,Debug)]
pub struct CrateId(u32);

impl CrateId {
    pub fn new(n: u32) -> Self {
        Self(n)
    }

    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(PartialEq,Clone,Copy,Debug)]
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
        f.debug_struct("Item").field("path", &self.path).finish()
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
        parent_trait: Option<TraitInfo>
    },
    Adt{
        fields: OnceLock<Vec<Type<'vm>>>
    },
    Trait{
        impl_list: RwLock<Vec<TraitImpl<'vm>>>
    },
    AssociatedType{
        parent_trait: TraitInfo
    }
}

/// Always refers to a trait item in the same crate.
pub struct TraitInfo {
    pub trait_id: ItemId,
    pub ident: String
}

pub struct TraitImpl<'vm> {
    for_types: Vec<Sub<'vm>>,
    crate_id: CrateId,
    child_fn_items: Vec<(String,ItemId)>,
    child_tys: Vec<(String,Type<'vm>)>,
}

impl<'vm> ItemKind<'vm> {
    pub fn new_function() -> Self {
        Self::Function{
            ir: Default::default(),
            mono_instances: Default::default(),
            parent_trait: None
        }
    }

    pub fn new_function_with_trait(trait_id: ItemId, ident: String) -> Self {
        Self::Function{
            ir: Default::default(),
            mono_instances: Default::default(),
            parent_trait: Some(TraitInfo{
                trait_id,
                ident
            })
        }
    }

    pub fn new_associated_type(trait_id: ItemId, ident: String) -> Self {
        Self::AssociatedType{
            parent_trait: TraitInfo{
                trait_id,
                ident
            }
        }
    }

    pub fn new_adt() -> Self {
        Self::Adt{
            fields: Default::default()
        }
    }

    pub fn new_trait() -> Self {
        Self::Trait{
            impl_list: Default::default()
        }
    }
}

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
        
        let ItemKind::Function{ir,parent_trait,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        if let Some(trait_info) = parent_trait {
            let crate_items = self.vm.get_crate_items(self.crate_id);
            let trait_item = crate_items.get(trait_info.trait_id);
            let resolved_func = trait_item.find_trait_fn(subs,&trait_info.ident);
            if let Some(resolved_func) = resolved_func {
                return resolved_func.get_ir(&[]); // <- subs shouldn't be needed here (but might in bc generation!)
            }
        }

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

    pub fn add_trait_impl(&self, for_types: Vec<Sub<'vm>>, crate_id: CrateId, child_fn_items: Vec<(String,ItemId)>, child_tys: Vec<(String,Type<'vm>)>) {
        let ItemKind::Trait{impl_list} = &self.kind else {
            panic!("item kind mismatch");
        };

        let mut impl_list = impl_list.write().unwrap();
        impl_list.push(TraitImpl{
            for_types,
            crate_id,
            child_fn_items,
            child_tys
        });
    }

    pub fn resolve_associated_ty(&self, subs: &[Sub<'vm>]) -> Type<'vm> {
        let ItemKind::AssociatedType{parent_trait} = &self.kind else {
            panic!("item kind mismatch");
        };

        let crate_items = self.vm.get_crate_items(self.crate_id);
        let trait_item = crate_items.get(parent_trait.trait_id);

        let ItemKind::Trait{impl_list} = &trait_item.kind else {
            panic!("item kind mismatch");
        };

        let impl_list = impl_list.read().unwrap();

        for candidate in impl_list.iter() {
            if check_types_match(&candidate.for_types,subs) {
                for (child_name,child_ty) in candidate.child_tys.iter() {
                    if *child_name == parent_trait.ident {
                        return *child_ty;
                    }
                }
                println!("{:?}::{} for {:?}",trait_item.path,parent_trait.ident,subs);
                panic!("trait ty lookup failed (no field)");
            }
        }
        println!("{:?}::{} for {:?}",trait_item.path,parent_trait.ident,subs);
        panic!("trait ty lookup failed (no impl)");
    }

    pub fn find_trait_fn(&self, subs: &[Sub<'vm>], member_name: &str) -> Option<&'vm Item> {
        let ItemKind::Trait{impl_list} = &self.kind else {
            panic!("item kind mismatch");
        };

        let impl_list = impl_list.read().unwrap();

        for candidate in impl_list.iter() {
            if check_types_match(&candidate.for_types,subs) {
                let crate_items = self.vm.get_crate_items(candidate.crate_id);
                for (child_name,child_item) in candidate.child_fn_items.iter() {
                    if child_name == member_name {
                        return Some(crate_items.get(*child_item));
                    }
                }
                //println!("{:?}::{} for {:?}",self.path,member_name,subs);
                //panic!("trait fn lookup failed (no field)");
            }
        }
        //println!("{:?}::{} for {:?}",self.path,member_name,subs);
        //panic!("trait fn lookup failed (no impl)");
        None
    }
}

fn check_types_match<'vm>(a: &[Sub<'vm>], b: &[Sub<'vm>]) -> bool {
    a == b
}
