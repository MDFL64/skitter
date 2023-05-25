use std::{sync::{Mutex, Arc, OnceLock, RwLock}, collections::HashMap, hash::Hash, borrow::Cow};

use crate::{vm::{VM, Function}, ir::IRFunction, types::{Sub, Type, TypeKind, ItemWithSubs, SubList}, builtins::{BuiltinTrait}, rustc_worker::RustCContext};
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

    pub fn all(&self) -> impl Iterator<Item = &Item<'vm>> {
        self.items.iter()
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
    pub fn main() -> Self {
        Self(NameSpace::Value,"::main".to_owned())
    }
    pub fn can_find(&self) -> bool {
        match self.0 {
            NameSpace::DebugOnly => false,
            _ => true
        }
    }
    pub fn new_debug(name: String) -> Self {
        Self(NameSpace::DebugOnly,name)
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
        write!(f,"Item({:?})",self.path.as_string())
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
        parent_trait: Option<TraitInfo>,
        extern_name: Option<String>
    },
    Adt{
        info: OnceLock<AdtInfo<'vm>>
    },
    Trait{
        impl_list: RwLock<Vec<TraitImpl<'vm>>>,
        builtin: OnceLock<BuiltinTrait>
    },
    AssociatedType{
        parent_trait: TraitInfo
    }
}

#[derive(Debug)]
pub struct FunctionSig<'vm> {
    pub inputs: Vec<Type<'vm>>,
    pub output: Type<'vm>
}

impl<'vm> FunctionSig<'vm> {
    pub fn from_rustc<'tcx>(rs_sig: &rustc_middle::ty::FnSig<'tcx>, ctx: &RustCContext<'vm,'tcx>) -> Self {
        
        let inputs = rs_sig.inputs().iter().map(|ty| {
            ctx.vm.types.type_from_rustc(*ty, ctx)
        }).collect();
        let output = ctx.vm.types.type_from_rustc(rs_sig.output(), ctx);

        Self {
            inputs,
            output
        }
    }

    pub fn sub(&self, subs: &SubList<'vm>) -> Self {
        let inputs = self.inputs.iter().map(|ty| {
            ty.sub(subs)
        }).collect();
        let output = self.output.sub(subs);

        Self {
            inputs,
            output
        }
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
            parent_trait: None,
            extern_name: None,
        }
    }

    pub fn new_function_with_trait(trait_id: ItemId, ident: String) -> Self {
        Self::Function{
            ir: Default::default(),
            mono_instances: Default::default(),
            parent_trait: Some(TraitInfo{
                trait_id,
                ident
            }),
            extern_name: None
        }
    }

    pub fn new_function_extern(name: String) -> Self {
        Self::Function{
            ir: Default::default(),
            mono_instances: Default::default(),
            parent_trait: None,
            extern_name: Some(name)
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
            impl_list: Default::default(),
            builtin: Default::default(),
        }
    }
}

impl<'vm> Item<'vm> {
    pub fn is_func(&self) -> bool {
        if let ItemKind::Function{..} = &self.kind {
            true
        } else {
            false
        }
    }

    /// Get a monomorphic VM function from a function item.
    pub fn func_mono(&'vm self, subs: &SubList<'vm>) -> &'vm Function<'vm> {

        let ItemKind::Function{mono_instances,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        let mut mono_instances = mono_instances.lock().unwrap();
        let result_func = mono_instances.entry(subs.clone()).or_insert_with(|| {
            self.vm.alloc_function(self, subs.clone())
        });

        result_func
    }

    pub fn func_get_extern(&self) -> &Option<String> {
        let ItemKind::Function{extern_name,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        extern_name
    }

    pub fn func_sig<'a>(&self, subs: &'a SubList<'vm>) -> FunctionSig<'vm> {
        let (ir,new_subs) = self.func_ir(subs);

        ir.sig.sub(&new_subs)
    }

    /// Get the IR for a function. Subs are used to find specialized IR for trait methods.
    pub fn func_ir<'a>(&self, subs: &'a SubList<'vm>) -> (Arc<IRFunction<'vm>>,Cow<'a,SubList<'vm>>) {
        
        let ItemKind::Function{ir,parent_trait,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        if let Some(trait_info) = parent_trait {
            let crate_items = self.vm.get_crate_items(self.crate_id);
            let trait_item = crate_items.get(trait_info.trait_id);
            let resolved_func = trait_item.find_trait_fn(subs,&trait_info.ident);
            if let Some((ir,new_subs)) = resolved_func {
                return (ir,Cow::Owned(new_subs));
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
                println!("converting ir for {}{}",self.path.as_string(),subs);
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
        let ItemKind::Trait{impl_list,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        let mut impl_list = impl_list.write().unwrap();
        impl_list.push(info);
    }

    pub fn trait_set_builtin(&self, new_builtin: BuiltinTrait) {
        let ItemKind::Trait{builtin,..} = &self.kind else {
            panic!("item kind mismatch");
        };
        builtin.set(new_builtin).ok();
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
        }).unwrap_or_else(|| {
            panic!("failed to find {} for {}",self.path.as_string(),subs);
        })
    }

    pub fn find_trait_fn(&self, subs: &SubList<'vm>, member_name: &str) -> Option<(Arc<IRFunction<'vm>>,SubList<'vm>)> {

        self.find_trait_impl(subs,|trait_impl,subs| {
            let crate_items = self.vm.get_crate_items(trait_impl.crate_id);
            for (child_name,child_item) in trait_impl.child_fn_items.iter() {
                if child_name == member_name {
                    let fn_item = crate_items.get(*child_item);
                    let (ir,_) = fn_item.func_ir(&subs);

                    return Some((ir,subs));
                }
            }
            None
        }).unwrap_or_else(|| {
            panic!("failed to find {} for {}",self.path.as_string(),subs);
        })
    }

    pub fn trait_has_impl(&self, subs: &SubList<'vm>) -> bool {
        self.find_trait_impl(subs,|_,_| {
            ()
        }).is_some()
    }

    pub fn find_trait_impl<T>(&self, subs: &SubList<'vm>, callback: impl FnOnce(&TraitImpl<'vm>,SubList<'vm>)->T) -> Option<T> {

        let ItemKind::Trait{impl_list,builtin} = &self.kind else {
            panic!("item kind mismatch");
        };

        if let Some(builtin) = builtin.get() {
            let builtin_res = builtin.find(subs);
            if let Some((a,b)) = builtin_res {
                return Some(callback(&a,b));
            }
        }

        let impl_list = impl_list.read().unwrap();

        //println!("searching for {}",self.path.as_string());
        'search:
        for candidate in impl_list.iter() {
            //println!("{} / {} / {}",self.path.as_string(),candidate.for_types,subs);
            if let Some(sub_map) = trait_match(subs,&candidate.for_types) {
                //println!("ok?");
                // first, the resulting SubMap must be translated into a proper SubList
                let mut trait_subs = SubList::from_summary(&candidate.generics, self.vm);
                sub_map.apply_to(SubSide::Rhs, &mut trait_subs);
                sub_map.assert_empty(SubSide::Lhs);

                //println!("maybe candidate? {}",candidate.for_types);
                if candidate.bounds.len() > 0 {
                    //println!("?? {}",candidate.bounds.len());
                    for bound in &candidate.bounds {
                        let types_to_check = bound.subs.sub(&trait_subs);
                        let res = bound.item.trait_has_impl(&types_to_check);
                        //println!("{} ====== {} has {}? {}",candidate.for_types,types_to_check,bound.item.path.as_string(),res);

                        if !res {
                            // failed, try next candidate
                            continue 'search;
                        }
                    }
                }

                assert!(trait_subs.is_concrete());

                return Some(callback(candidate,trait_subs));
            }
        }
        
        None
    }
}

#[derive(PartialEq,Debug)]
enum SubSide {
    Lhs,
    Rhs
}

#[derive(Default)]
struct SubMap<'vm> {
    map: Vec<((SubSide,u32),Type<'vm>)>
}

impl<'vm> SubMap<'vm> {
    fn set(&mut self, side: SubSide, n: u32, val: Type<'vm>) -> bool {
        let key = (side,n);
        for (ek,ev) in &self.map {
            if *ek == key {
                assert!(*ev == val);
                return true;
            }
        }
        self.map.push((key,val));
        true
    }

    fn apply_to(&self, target_side: SubSide, target_subs: &mut SubList<'vm>) {
        for ((side,n),val) in &self.map {
            if *side == target_side {
                target_subs.list[*n as usize] = Sub::Type(*val);
            }
        }
    }

    fn assert_empty(&self, target_side: SubSide) {
        for ((side,_),_) in &self.map {
            if *side == target_side {
                panic!("SubMap::assert_empty failed");
            }
        }
    }
    /*fn get(&self, side: SubSide, n: u32) -> Option<Type<'vm>> {
        let key = (side,n);
        for (ek,ev) in &self.map {
            if *ek == key {
                return Some(*ev);
            }
        }
        println!("warning! failed to resolve substitution: {:?} / {:?}",self.map,key);
        None
    }*/
}

/// Compares a list of concrete types to a candidate type.
fn trait_match<'vm>(lhs: &SubList<'vm>, rhs: &SubList<'vm>) -> Option<SubMap<'vm>> {

    let mut res_map = SubMap::default();

    if subs_match(lhs, rhs, &mut res_map) {
        Some(res_map)
    } else {
        None
    }
}

fn subs_match<'vm>(lhs: &SubList<'vm>, rhs: &SubList<'vm>, res_map: &mut SubMap<'vm>) -> bool {
    for pair in lhs.list.iter().zip(&rhs.list) {
        match pair {
            (Sub::Type(lhs_ty),Sub::Type(rhs_ty)) => {
                if !type_match(*lhs_ty,*rhs_ty,res_map) {
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
fn type_match<'vm>(lhs_ty: Type<'vm>, rhs_ty: Type<'vm>, res_map: &mut SubMap<'vm>) -> bool {
    if lhs_ty == rhs_ty {
        return true;
    }

    match (lhs_ty.kind(),rhs_ty.kind()) {
        (TypeKind::Adt(a),TypeKind::Adt(b)) => {
            (a.item == b.item) && subs_match(&a.subs, &b.subs,res_map)
        },
        (TypeKind::Ptr(in_ref,in_mut),TypeKind::Ptr(trait_ref,trait_mut)) => {
            (in_mut == trait_mut) && type_match(*in_ref, *trait_ref,res_map)
        }
        (TypeKind::Ref(in_ref,in_mut),TypeKind::Ref(trait_ref,trait_mut)) => {
            (in_mut == trait_mut) && type_match(*in_ref, *trait_ref,res_map)
        }
        (TypeKind::Slice(in_elem),TypeKind::Slice(trait_elem)) => {
            type_match(*in_elem, *trait_elem,res_map)
        }
        (TypeKind::Tuple(lhs_children),TypeKind::Tuple(rhs_children)) => {
            if lhs_children.len() != rhs_children.len() {
                false
            } else {
                for (lhs_child,rhs_child) in lhs_children.iter().zip(rhs_children) {
                    if !type_match(*lhs_child, *rhs_child, res_map) {
                        return false;
                    }
                }
                true
            }
        }

        (TypeKind::Param(lhs_param),TypeKind::Param(rhs_param)) => {
            panic!("fixme? this looks annoying");
        }
        (_,TypeKind::Param(param_num)) => {
            res_map.set(SubSide::Rhs,*param_num,lhs_ty)
        }
        (TypeKind::Param(param_num),_) => {
            res_map.set(SubSide::Lhs,*param_num,rhs_ty)
        }

        (TypeKind::Adt(..),_) | (_,TypeKind::Adt(..)) |
        (TypeKind::Ptr(..),_) | (_,TypeKind::Ptr(..)) |
        (TypeKind::Ref(..),_) | (_,TypeKind::Ref(..)) |
        (TypeKind::Slice(..),_) | (_,TypeKind::Slice(..)) |

        (TypeKind::Bool,_) | (_,TypeKind::Bool) |
        (TypeKind::Char,_) | (_,TypeKind::Char) |
        (TypeKind::Never,_) | (_,TypeKind::Never) |
        (TypeKind::Int(..),_) | (_,TypeKind::Int(..)) |
        (TypeKind::Float(..),_) | (_,TypeKind::Float(..)) => false,
        _ => {
            panic!("match types {} == {}",lhs_ty,rhs_ty)
        }
    }
}

/// Get a path from rustc.
pub fn path_from_rustc(in_path: &rustc_hir::definitions::DefPath) -> ItemPath {
    use rustc_hir::definitions::DefPathData;

    let mut result = String::new();

    let mut is_debug = false;

    // we only handle trivial paths
    for elem in in_path.data.iter() {
        match elem.data {
            DefPathData::ValueNs(sym) |
            DefPathData::TypeNs(sym) => {
                result.push_str("::");
                result.push_str(sym.as_str());
            }
            DefPathData::Impl => {
                result.push_str("::{impl}");
                is_debug = true;
            }
            DefPathData::ForeignMod => {
                // do nothing
            }
            //DefPathData
            _ => panic!("todo path {:?} {}",elem.data,in_path.to_string_no_crate_verbose())
        }
    }

    if is_debug {
        ItemPath(NameSpace::DebugOnly,result)
    } else if let Some(last_elem) = in_path.data.last() {
        match last_elem.data {
            DefPathData::ValueNs(_) => ItemPath(NameSpace::Value,result),
            DefPathData::TypeNs(_) => ItemPath(NameSpace::Type,result),
            _ => panic!("can't determine namespace: {:?}",last_elem.data)
        }
    } else {
        panic!("zero element path?");
    }
}
