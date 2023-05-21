use std::{sync::{Mutex, Arc, OnceLock, RwLock}, collections::HashMap, hash::Hash, borrow::Cow};

use crate::{vm::{VM, Function}, ir::IRFunction, types::{Sub, Type, TypeKind, ItemWithSubs, SubList}};
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
        mono_instances: Mutex<HashMap<SubList<'vm>,&'vm Function<'vm>>>,
        parent_trait: Option<TraitInfo>
    },
    Adt{
        info: OnceLock<AdtInfo<'vm>>
    },
    Trait{
        impl_list: RwLock<Vec<TraitImpl<'vm>>>
    },
    AssociatedType{
        parent_trait: TraitInfo
    }
}

pub struct AdtInfo<'vm> {
    pub variant_fields: Vec<Vec<Type<'vm>>>,
    pub discriminator_ty: Option<Type<'vm>>
}

/// Always refers to a trait item in the same crate.
pub struct TraitInfo {
    pub trait_id: ItemId,
    pub ident: String
}

pub struct TraitImpl<'vm> {
    pub for_types: SubList<'vm>,
    //impl_params: Vec<Sub<'vm>>,
    pub crate_id: CrateId,
    pub child_fn_items: Vec<(String,ItemId)>,
    pub child_tys: Vec<(String,Type<'vm>)>,
    pub bounds: Vec<ItemWithSubs<'vm>>,
    pub generics: GenericCounts
}

#[derive(Default,Debug)]
pub struct GenericCounts {
    pub lifetimes: u32,
    pub types: u32,
    pub consts: u32
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
            info: Default::default()
        }
    }

    pub fn new_trait() -> Self {
        Self::Trait{
            impl_list: Default::default()
        }
    }
}

impl<'vm> Item<'vm> {
    pub fn get_function(&'vm self, subs: &SubList<'vm>) -> &'vm Function<'vm> {

        let ItemKind::Function{mono_instances,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        let mut mono_instances = mono_instances.lock().unwrap();
        mono_instances.entry(subs.clone()).or_insert_with(|| {
            self.vm.alloc_function(self, subs.clone())
        })
    }

    /// Get the IR for a function. Subs are used to find specialized IR for trait methods.
    pub fn get_ir<'a>(&self, subs: &'a SubList<'vm>) -> (Arc<IRFunction<'vm>>,Cow<'a,SubList<'vm>>) {
        
        let ItemKind::Function{ir,parent_trait,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        if let Some(trait_info) = parent_trait {
            let crate_items = self.vm.get_crate_items(self.crate_id);
            let trait_item = crate_items.get(trait_info.trait_id);
            let resolved_func = trait_item.find_trait_fn(subs,&trait_info.ident);
            if let Some(resolved_func) = resolved_func {
                let (f,s) = resolved_func.item.get_ir(&resolved_func.subs);
                return (f,Cow::Owned(s.into_owned()));
            }
        }

        loop {
            {
                let ir = ir.lock().unwrap();
                if let Some(ir) = ir.as_ref() {
                    return (ir.clone(),Cow::Borrowed(subs));
                }
            }

            if self.vm.is_verbose {
                println!("converting ir for {:?} {:?}",self.path,subs);
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

    pub fn get_adt_info(&self) -> &AdtInfo<'vm> {
        let ItemKind::Adt{info} = &self.kind else {
            panic!("item kind mismatch");
        };

        info.get().expect("adt missing fields")
    }

    pub fn set_adt_info(&self, new_info: AdtInfo<'vm>) {
        let ItemKind::Adt{info} = &self.kind else {
            panic!("item kind mismatch");
        };

        info.set(new_info).ok();
    }

    pub fn add_trait_impl(&self, info: TraitImpl<'vm>) {
        let ItemKind::Trait{impl_list} = &self.kind else {
            panic!("item kind mismatch");
        };

        let mut impl_list = impl_list.write().unwrap();
        impl_list.push(info);
    }

    pub fn resolve_associated_ty(&self, subs: &SubList<'vm>) -> Type<'vm> {

        let ItemKind::AssociatedType{parent_trait} = &self.kind else {
            panic!("item kind mismatch");
        };

        let crate_items = self.vm.get_crate_items(self.crate_id);
        let trait_item = crate_items.get(parent_trait.trait_id);

        trait_item.find_trait_impl(subs,|trait_impl,subs| {
            for (child_name,child_ty) in trait_impl.child_tys.iter() {
                if *child_name == parent_trait.ident {
                    return *child_ty;
                }
            }
            panic!("failed to find associated type")
        }).expect("failed to find trait")
    }

    pub fn find_trait_fn(&self, subs: &SubList<'vm>, member_name: &str) -> Option<ItemWithSubs<'vm>> {

        self.find_trait_impl(subs,|trait_impl,subs| {
            let crate_items = self.vm.get_crate_items(trait_impl.crate_id);
            for (child_name,child_item) in trait_impl.child_fn_items.iter() {
                if child_name == member_name {
                    return Some(ItemWithSubs{
                        item: crate_items.get(*child_item),
                        subs
                    });
                }
            }
            panic!("failed to find associated function");
        }).expect("failed to find trait")
    }

    pub fn trait_has_impl(&self, subs: &SubList<'vm>) -> bool {
        self.find_trait_impl(subs,|trait_impl,subs| {
            ()
        }).is_some()
    }

    pub fn find_trait_impl<T>(&self, subs: &SubList<'vm>, callback: impl FnOnce(&TraitImpl<'vm>,SubList<'vm>)->T) -> Option<T> {
        let ItemKind::Trait{impl_list} = &self.kind else {
            panic!("item kind mismatch");
        };

        let impl_list = impl_list.read().unwrap();

        'search:
        for candidate in impl_list.iter() {
            if let Some(sub_map) = trait_match(subs,&candidate.for_types) {
                // first, the resulting SubMap must be translated into a proper SubList
                let mut trait_subs = SubList{list:Vec::new()};
                
                for _ in 0..candidate.generics.lifetimes {
                    trait_subs.list.push(Sub::Lifetime);
                }
                for _ in 0..candidate.generics.types {
                    let index = trait_subs.list.len() as u32;
                    trait_subs.list.push(Sub::Type(sub_map.get(index)));
                }
                for _ in 0..candidate.generics.consts {
                    trait_subs.list.push(Sub::Const);
                }

                if candidate.bounds.len() > 0 {
                    for bound in &candidate.bounds {
                        let types_to_check = bound.subs.sub(&trait_subs);
                        println!("? {} has {}",types_to_check,bound.item.path.as_string());
                        let res = bound.item.trait_has_impl(&types_to_check);
                        println!("> {}",res);

                        if !res {
                            // failed, try next candidate
                            continue 'search;
                        }
                    }
                    panic!();
                }
                // TODO additional checks

                return Some(callback(candidate,trait_subs));
            }
        }

        None
    }
}

#[derive(Default)]
struct SubMap<'vm> {
    map: Vec<(u32,Type<'vm>)>
}

impl<'vm> SubMap<'vm> {
    fn set(&mut self, key: u32, val: Type<'vm>) -> bool {
        for (ek,ev) in &self.map {
            if *ek == key {
                assert!(*ev == val);
                return true;
            }
        }
        self.map.push((key,val));
        true
    }

    fn get(&self, key: u32) -> Type<'vm> {
        for (ek,ev) in &self.map {
            if *ek == key {
                return *ev;
            }
        }
        panic!("failed to resolve substitution");
    }
}

/// Compares a list of concrete types to a candidate type.
fn trait_match<'vm>(in_subs: &SubList<'vm>, trait_subs: &SubList<'vm>) -> Option<SubMap<'vm>> {

    assert!(in_subs.is_concrete());

    let mut res_map = SubMap::default();

    if subs_match(in_subs, trait_subs, &mut res_map) {
        Some(res_map)
    } else {
        None
    }
}

fn subs_match<'vm>(in_subs: &SubList<'vm>, trait_subs: &SubList<'vm>, res_map: &mut SubMap<'vm>) -> bool {
    for pair in in_subs.list.iter().zip(&trait_subs.list) {
        match pair {
            (Sub::Type(in_ty),Sub::Type(trait_ty)) => {
                if !type_match(*in_ty,*trait_ty,res_map) {
                    return false;
                }
            },
            _ => {
                if pair.0 != pair.1 {
                    return false;
                }
            }
        }
    }
    true
}

/// Compares types loosely, allowing param types to match anything.
fn type_match<'vm>(in_ty: Type<'vm>, trait_ty: Type<'vm>, res_map: &mut SubMap<'vm>) -> bool {
    if in_ty == trait_ty {
        return true;
    }

    match (in_ty.kind(),trait_ty.kind()) {
        (TypeKind::Adt(a),TypeKind::Adt(b)) => {
            (a.item == b.item) && subs_match(&a.subs, &b.subs,res_map)
        },
        (TypeKind::Ptr(in_ref,in_mut),TypeKind::Ptr(trait_ref,trait_mut)) => {
            (in_mut == trait_mut) && type_match(*in_ref, *trait_ref,res_map)
        }
        (TypeKind::Ref(in_ref,in_mut),TypeKind::Ref(trait_ref,trait_mut)) => {
            (in_mut == trait_mut) && type_match(*in_ref, *trait_ref,res_map)
        }

        (_,TypeKind::Param(param_num)) => {
            res_map.set(*param_num,in_ty)
        }

        (TypeKind::Adt(..),_) | (_,TypeKind::Adt(..)) |
        (TypeKind::Ptr(..),_) | (_,TypeKind::Ptr(..)) |
        (TypeKind::Ref(..),_) | (_,TypeKind::Ref(..)) |

        (TypeKind::Bool,_) | (_,TypeKind::Bool) |
        (TypeKind::Char,_) | (_,TypeKind::Char) |
        (TypeKind::Never,_) | (_,TypeKind::Never) |
        (TypeKind::Int(..),_) | (_,TypeKind::Int(..)) |
        (TypeKind::Float(..),_) | (_,TypeKind::Float(..)) => false,
        _ => {
            panic!("match types {} == {}",in_ty,trait_ty)
        }
    }
}
