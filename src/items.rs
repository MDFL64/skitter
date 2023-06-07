use std::{
    borrow::Cow,
    collections::HashMap,
    hash::Hash,
    sync::{Arc, Mutex, OnceLock, RwLock},
};

use crate::{
    builtins::BuiltinTrait,
    bytecode_compiler::{self, BytecodeCompiler},
    ir::{glue_builder::glue_for_ctor, IRFunction},
    persist::{Persist, PersistReadContext, PersistWriteContext},
    rustc_worker::RustCContext,
    types::{ItemWithSubs, Sub, SubList, Type, TypeKind},
    vm::{Function, VM},
};
use ahash::AHashMap;

pub struct CrateItems<'vm> {
    crate_id: CrateId,
    items: Vec<Item<'vm>>,
    map_path_to_item: AHashMap<ItemPath<'vm>, ItemId>,
    map_did_to_item: AHashMap<rustc_hir::def_id::LocalDefId, ItemId>,

    extern_crate_list: Vec<ExternCrate>,
    extern_crate_id_cache: Mutex<AHashMap<rustc_span::def_id::CrateNum, CrateId>>,
}

pub struct ExternCrate {
    pub name: String,
    pub id: CrateId,
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

    pub fn save_items(&self, file_name: &str) {
        let mut write_ctx = PersistWriteContext::new(self.crate_id);
        self.items.persist_write(&mut write_ctx);

        // write types
        {
            let type_count = write_ctx.types.len();
            println!("types = {}", type_count);
            type_count.persist_write(&mut write_ctx);

            for i in 0..type_count {
                let ty = write_ctx.types[i];
                ty.kind().persist_write(&mut write_ctx);
            }

            // ensure no more types were added, if they
            // are it indicates a bug in `prepare_child_types`
            assert!(type_count == write_ctx.types.len());
        }

        write_ctx.save(file_name);
    }

    pub fn load_items(file_name: &str, vm: &'vm VM<'vm>, crate_id: CrateId) {
        let data: &'static [u8] = std::fs::read(file_name)
            .expect("failed to read data")
            .leak();
        let mut read = PersistReadContext::new(&data, vm, crate_id);

        let items: Vec<Item<'vm>> = Persist::persist_read(&mut read);

        let mut map_path_to_item = AHashMap::<ItemPath<'vm>, ItemId>::new();
        for item in items.iter() {
            if item.path.can_find() {
                let old = map_path_to_item.insert(item.path.clone(), item.item_id);
                assert!(old.is_none());
            }
        }
    }

    pub fn all(&self) -> impl Iterator<Item = &Item<'vm>> {
        self.items.iter()
    }

    pub fn count(&self) -> usize {
        self.items.len()
    }

    pub fn add_item(
        &mut self,
        vm: &'vm VM<'vm>,
        kind: ItemKind<'vm>,
        path: ItemPath<'vm>,
        did: rustc_hir::def_id::LocalDefId,
    ) -> ItemId {
        let item_id = ItemId(self.items.len() as u32);

        self.items.push(Item {
            vm,
            crate_id: self.crate_id,
            item_id,
            did: Some(did),
            path: path.clone(),
            kind,
            persist_data: None,
        });

        // add to did map
        {
            let old = self.map_did_to_item.insert(did, item_id);
            assert!(old.is_none());
        }

        // add to path map
        if path.0 != NameSpace::DebugOnly {
            let old = self.map_path_to_item.insert(path, item_id);
            if old.is_some() {
                panic!("duplicate path {:?}", did);
            }
        }

        item_id
    }

    pub fn get(&self, id: ItemId) -> &Item<'vm> {
        &self.items[id.index()]
    }

    pub fn find_by_path(&'vm self, path: &ItemPath) -> Option<&'vm Item<'vm>> {
        self.map_path_to_item
            .get(path)
            .map(|item_id| &self.items[item_id.index()])
    }

    pub fn find_by_did(&'vm self, did: rustc_hir::def_id::DefId) -> Option<&'vm Item<'vm>> {
        if did.krate == rustc_hir::def_id::LOCAL_CRATE {
            let local_did = rustc_hir::def_id::LocalDefId {
                local_def_index: did.index,
            };
            self.map_did_to_item
                .get(&local_did)
                .map(|item_id| &self.items[item_id.index()])
        } else {
            None
        }
    }

    pub fn find_crate_id<'tcx>(
        &self,
        tcx: rustc_middle::ty::TyCtxt<'tcx>,
        crate_num: rustc_span::def_id::CrateNum,
    ) -> CrateId {
        let mut cache = self.extern_crate_id_cache.lock().unwrap();

        *cache.entry(crate_num).or_insert_with(|| {
            let crate_name = tcx.crate_name(crate_num);
            for entry in &self.extern_crate_list {
                if entry.name == crate_name.as_str() {
                    return entry.id;
                }
            }
            panic!(
                "lookup for crate failed: {} {}",
                crate_num,
                crate_name.as_str()
            );
        })
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct ItemPath<'vm>(NameSpace, &'vm str);

impl<'vm> ItemPath<'vm> {
    pub fn main() -> Self {
        Self(NameSpace::Value, "::main")
    }
    pub fn can_find(&self) -> bool {
        match self.0 {
            NameSpace::DebugOnly => false,
            _ => true,
        }
    }
    pub fn new_debug(name: &str, vm: &'vm VM<'vm>) -> Self {
        Self(NameSpace::DebugOnly, vm.alloc_path(name))
    }
    pub fn as_string(&self) -> &str {
        &self.1
    }
}

impl<'vm> Persist<'vm> for ItemPath<'vm> {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        match self.0 {
            NameSpace::Type => write_ctx.write_byte('t' as u8),
            NameSpace::Value => write_ctx.write_byte('v' as u8),
            NameSpace::DebugOnly => write_ctx.write_byte('d' as u8),
        }
        write_ctx.write_str(self.1);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let ns = read_ctx.read_byte() as char;
        let ns = match ns {
            't' => NameSpace::Type,
            'v' => NameSpace::Value,
            'd' => NameSpace::DebugOnly,
            _ => panic!(),
        };

        let string = read_ctx.read_str();

        Self(ns, string)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
enum NameSpace {
    /// Structs, Enums, etc.
    Type,
    /// Functions
    Value,
    /// Not used for real paths (impls and ???)
    DebugOnly,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct CrateId(u32);

impl CrateId {
    pub fn new(n: u32) -> Self {
        Self(n)
    }

    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub struct ItemId(u32);

impl ItemId {
    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

pub struct Item<'vm> {
    pub vm: &'vm VM<'vm>,
    pub crate_id: CrateId,
    pub item_id: ItemId,
    pub did: Option<rustc_hir::def_id::LocalDefId>,
    pub path: ItemPath<'vm>,
    kind: ItemKind<'vm>,
    /// A slice of data which can be deserialized lazily.
    /// Only filled when loading IR from the disk.
    persist_data: Option<&'vm [u8]>,
}

impl<'vm> std::fmt::Debug for Item<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Item({:?})", self.path.as_string())
    }
}

impl<'vm> PartialEq for Item<'vm> {
    fn eq(&self, other: &Self) -> bool {
        self.crate_id == other.crate_id && self.item_id == other.item_id
    }
}

impl<'vm> Eq for Item<'vm> {}

impl<'vm> Hash for Item<'vm> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u32(self.crate_id.0);
        state.write_u32(self.item_id.0);
    }
}

impl<'vm> Persist<'vm> for Item<'vm> {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.path.persist_write(write_ctx);
        match &self.kind {
            ItemKind::Function {
                virtual_info,
                extern_name,
                ctor_for,
                ..
            } => {
                write_ctx.write_byte('f' as u8);
                virtual_info.persist_write(write_ctx);
                ctor_for.map(|(x, y)| (x.0, y)).persist_write(write_ctx);
                extern_name.persist_write(write_ctx);
                let ir = self.raw_ir();
                println!("has ir? {}", ir.is_some());
            }
            ItemKind::Constant {
                virtual_info,
                ctor_for,
                ..
            } => {
                write_ctx.write_byte('c' as u8);
                virtual_info.persist_write(write_ctx);
                ctor_for.map(|(x, y)| (x.0, y)).persist_write(write_ctx);
                let ir = self.raw_ir();
                println!("has ir? {}", ir.is_some());
            }
            ItemKind::AssociatedType { virtual_info } => {
                write_ctx.write_byte('y' as u8);
                virtual_info.persist_write(write_ctx);
            }
            ItemKind::Adt { info } => {
                write_ctx.write_byte('a' as u8);
                // adt info must be filled after parsing types
                write_ctx.start_block();
                info.get().unwrap().persist_write(write_ctx);
                write_ctx.end_block();
            }
            ItemKind::Trait { .. } => {
                write_ctx.write_byte('t' as u8);
                // todo builtin ONLY, handle impls separately
            }
        }
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let path = ItemPath::persist_read(read_ctx);

        // todo actual kind
        let kind_c = read_ctx.read_byte() as char;
        let (kind, persist_data) = match kind_c {
            'f' => {
                let virtual_info = Option::<VirtualInfo>::persist_read(read_ctx);
                let ctor_for =
                    Option::<(u32, u32)>::persist_read(read_ctx).map(|(a, b)| (ItemId(a), b));
                let extern_name = Option::<(FunctionAbi, String)>::persist_read(read_ctx);
                let kind = ItemKind::Function {
                    ir: Default::default(),
                    mono_instances: Default::default(),
                    virtual_info,
                    extern_name,
                    ctor_for,
                };
                (kind, None)
            }
            'c' => {
                let virtual_info = Option::<VirtualInfo>::persist_read(read_ctx);
                let ctor_for =
                    Option::<(u32, u32)>::persist_read(read_ctx).map(|(a, b)| (ItemId(a), b));
                let kind = ItemKind::Constant {
                    ir: Default::default(),
                    mono_values: Default::default(),
                    virtual_info,
                    ctor_for,
                };
                (kind, None)
            }
            'y' => {
                let virtual_info = VirtualInfo::persist_read(read_ctx);
                let kind = ItemKind::AssociatedType { virtual_info };
                (kind, None)
            }
            'a' => {
                let adt_info_block = read_ctx.read_block();
                let kind = ItemKind::new_adt();
                (kind, Some(adt_info_block))
            }
            't' => {
                let kind = ItemKind::new_trait();
                (kind, None)
            }
            _ => panic!(),
        };

        let item_id = ItemId(read_ctx.next_item_id);
        read_ctx.next_item_id += 1;

        Self {
            vm: read_ctx.vm,
            crate_id: read_ctx.crate_id,
            item_id,
            path,
            did: None,
            kind,
            persist_data,
        }
    }
}

#[derive(PartialEq)]
pub enum FunctionAbi {
    RustIntrinsic,
}

impl<'vm> Persist<'vm> for (FunctionAbi, String) {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        assert!(self.0 == FunctionAbi::RustIntrinsic);
        write_ctx.write_str(&self.1);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let ident = read_ctx.read_str().to_owned();
        (FunctionAbi::RustIntrinsic, ident)
    }
}

///
/// `virtual_info` is attached to each item appearing in a trait declaration,
/// and is used to resolve concrete implementations of those items.
pub enum ItemKind<'vm> {
    Function {
        ir: Mutex<Option<Arc<IRFunction<'vm>>>>,
        mono_instances: Mutex<HashMap<SubList<'vm>, &'vm Function<'vm>>>,
        virtual_info: Option<VirtualInfo>,
        ctor_for: Option<(ItemId, u32)>,
        extern_name: Option<(FunctionAbi, String)>,
    },
    /// Constants operate very similarly to functions, but are evaluated
    /// greedily when encountered in IR and converted directly to values.
    Constant {
        ir: Mutex<Option<Arc<IRFunction<'vm>>>>,
        mono_values: Mutex<HashMap<SubList<'vm>, &'vm [u8]>>,
        virtual_info: Option<VirtualInfo>,
        ctor_for: Option<(ItemId, u32)>,
    },
    AssociatedType {
        virtual_info: VirtualInfo,
    },
    Adt {
        info: OnceLock<AdtInfo<'vm>>,
    },
    Trait {
        impl_list: RwLock<Vec<TraitImpl<'vm>>>,
        builtin: OnceLock<BuiltinTrait>,
    },
}

#[derive(Debug)]
pub struct FunctionSig<'vm> {
    pub inputs: Vec<Type<'vm>>,
    pub output: Type<'vm>,
}

impl<'vm> FunctionSig<'vm> {
    pub fn from_rustc<'tcx>(
        rs_sig: &rustc_middle::ty::FnSig<'tcx>,
        ctx: &RustCContext<'vm, 'tcx>,
    ) -> Self {
        let inputs = rs_sig
            .inputs()
            .iter()
            .map(|ty| ctx.vm.types.type_from_rustc(*ty, ctx))
            .collect();
        let output = ctx.vm.types.type_from_rustc(rs_sig.output(), ctx);

        Self { inputs, output }
    }

    pub fn sub(&self, subs: &SubList<'vm>) -> Self {
        let inputs = self.inputs.iter().map(|ty| ty.sub(subs)).collect();
        let output = self.output.sub(subs);

        Self { inputs, output }
    }
}

pub enum AdtKind<'vm> {
    Struct,
    EnumWithDiscriminant(Type<'vm>),
    EnumNonZero,
}

pub struct AdtInfo<'vm> {
    pub variant_fields: Vec<Vec<Type<'vm>>>,
    pub kind: AdtKind<'vm>,
}

impl<'vm> AdtInfo<'vm> {
    pub fn is_enum(&self) -> bool {
        match self.kind {
            AdtKind::Struct => false,
            AdtKind::EnumNonZero | AdtKind::EnumWithDiscriminant(_) => true,
        }
    }

    pub fn discriminant_ty(&self) -> Option<Type<'vm>> {
        match self.kind {
            AdtKind::EnumWithDiscriminant(ty) => Some(ty),
            _ => None,
        }
    }
}

impl<'vm> Persist<'vm> for AdtInfo<'vm> {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.variant_fields.persist_write(write_ctx);

        match self.kind {
            AdtKind::Struct => {
                write_ctx.write_byte(0);
            }
            AdtKind::EnumWithDiscriminant(ty) => {
                write_ctx.write_byte(1);
                ty.persist_write(write_ctx);
            }
            AdtKind::EnumNonZero => {
                write_ctx.write_byte(2);
            }
        }
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let variant_fields = <Vec<Vec<Type<'vm>>>>::persist_read(read_ctx);
        panic!("fixme");
        //let discriminator_ty = <Option<Type<'vm>>>::persist_read(read_ctx);
        /*AdtInfo{
            variant_fields,
            discriminator_ty
        }*/
    }
}

/// Always refers to a trait item in the same crate.
pub struct VirtualInfo {
    pub trait_id: ItemId,
    pub ident: String,
}

impl<'vm> Persist<'vm> for VirtualInfo {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.trait_id.index().persist_write(write_ctx);
        write_ctx.write_str(self.ident.as_ref());
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let trait_id = ItemId(u32::persist_read(read_ctx));
        let ident = read_ctx.read_str().to_owned();
        Self { trait_id, ident }
    }
}

pub struct TraitImpl<'vm> {
    pub for_types: SubList<'vm>,
    //impl_params: Vec<Sub<'vm>>,
    pub crate_id: CrateId,
    pub assoc_values: AHashMap<String, AssocValue<'vm>>,
    pub assoc_tys: AHashMap<String, Type<'vm>>,
    pub bounds: Vec<BoundKind<'vm>>,
    pub generics: GenericCounts,
}

#[derive(Clone)]
pub enum AssocValue<'vm> {
    /// An item. Can be either a function or a constant.
    Item(ItemId),
    /// Used to inject IR into builtin traits without building entire new items.
    RawFunctionIR(Arc<IRFunction<'vm>>),
}

pub enum BoundKind<'vm> {
    /// Is the trait item implemented for the given subs?
    Trait(ItemWithSubs<'vm>),
    /// Are the associated type and the second type equal? Can update params in the impl.
    Projection(ItemWithSubs<'vm>, Type<'vm>),
}

#[derive(Default, Debug)]
pub struct GenericCounts {
    pub lifetimes: u32,
    pub types: u32,
    pub consts: u32,
}

impl<'vm> ItemKind<'vm> {
    pub fn new_function() -> Self {
        Self::Function {
            ir: Default::default(),
            mono_instances: Default::default(),
            virtual_info: None,
            extern_name: None,
            ctor_for: None,
        }
    }

    pub fn new_function_virtual(trait_id: ItemId, ident: String) -> Self {
        Self::Function {
            ir: Default::default(),
            mono_instances: Default::default(),
            virtual_info: Some(VirtualInfo { trait_id, ident }),
            extern_name: None,
            ctor_for: None,
        }
    }

    pub fn new_function_extern(abi: FunctionAbi, name: String) -> Self {
        Self::Function {
            ir: Default::default(),
            mono_instances: Default::default(),
            virtual_info: None,
            extern_name: Some((abi, name)),
            ctor_for: None,
        }
    }

    pub fn new_function_ctor(adt_id: ItemId, variant: u32) -> Self {
        Self::Function {
            ir: Default::default(),
            mono_instances: Default::default(),
            virtual_info: None,
            extern_name: None,
            ctor_for: Some((adt_id, variant)),
        }
    }

    pub fn new_const() -> Self {
        Self::Constant {
            ir: Default::default(),
            mono_values: Default::default(),
            virtual_info: None,
            ctor_for: None,
        }
    }

    pub fn new_const_virtual(trait_id: ItemId, ident: String) -> Self {
        Self::Constant {
            ir: Default::default(),
            mono_values: Default::default(),
            virtual_info: Some(VirtualInfo { trait_id, ident }),
            ctor_for: None,
        }
    }

    pub fn new_const_ctor(adt_id: ItemId, variant: u32) -> Self {
        Self::Constant {
            ir: Default::default(),
            mono_values: Default::default(),
            virtual_info: None,
            ctor_for: Some((adt_id, variant)),
        }
    }

    pub fn new_associated_type(trait_id: ItemId, ident: String) -> Self {
        Self::AssociatedType {
            virtual_info: VirtualInfo { trait_id, ident },
        }
    }

    pub fn new_adt() -> Self {
        Self::Adt {
            info: Default::default(),
        }
    }

    pub fn new_trait() -> Self {
        Self::Trait {
            impl_list: Default::default(),
            builtin: Default::default(),
        }
    }
}

impl<'vm> Item<'vm> {
    /// Get a monomorphic VM function from a function item.
    pub fn func_mono(&'vm self, subs: &SubList<'vm>) -> &'vm Function<'vm> {
        let ItemKind::Function{mono_instances,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        let mut mono_instances = mono_instances.lock().unwrap();
        let result_func = mono_instances
            .entry(subs.clone())
            .or_insert_with(|| self.vm.alloc_function(self, subs.clone()));

        result_func
    }

    pub fn is_function(&self) -> bool {
        if let ItemKind::Function { .. } = &self.kind {
            true
        } else {
            false
        }
    }

    pub fn func_extern(&self) -> &Option<(FunctionAbi, String)> {
        let ItemKind::Function{extern_name,..} = &self.kind else {
            panic!("item kind mismatch");
        };

        extern_name
    }

    pub fn func_sig(&self, subs: &SubList<'vm>) -> FunctionSig<'vm> {
        let (ir, new_subs) = self.ir(subs);

        ir.sig.sub(&new_subs)
    }

    /// Get the IR for a function OR a constant. Subs are used to find specialized IR for trait items.
    pub fn ir<'a>(&self, subs: &'a SubList<'vm>) -> (Arc<IRFunction<'vm>>, Cow<'a, SubList<'vm>>) {
        let (ir, virtual_info, ctor_for, is_constant) = match &self.kind {
            ItemKind::Function {
                ir,
                virtual_info,
                ctor_for,
                ..
            } => (ir, virtual_info, ctor_for, false),
            ItemKind::Constant {
                ir,
                virtual_info,
                ctor_for,
                ..
            } => (ir, virtual_info, ctor_for, true),
            _ => panic!("item kind mismatch"),
        };

        // handle ctors
        if let Some((ctor_item_id, ctor_variant)) = ctor_for {
            let crate_items = self.vm.get_crate_items(self.crate_id);
            let ctor_item = crate_items.get(*ctor_item_id);

            let ctor_ty = self.vm.ty_adt(ItemWithSubs {
                item: ctor_item,
                subs: subs.clone(),
            });

            let ir = glue_for_ctor(ctor_ty, *ctor_variant, is_constant);
            return (Arc::new(ir), Cow::Borrowed(subs));
        }

        if let Some(virtual_info) = virtual_info {
            let crate_items = self.vm.get_crate_items(self.crate_id);
            let trait_item = crate_items.get(virtual_info.trait_id);
            let resolved_func = trait_item.find_trait_item_ir(subs, &virtual_info.ident);
            if let Some((ir, new_subs)) = resolved_func {
                assert!(ir.is_constant == is_constant);
                return (ir, Cow::Owned(new_subs));
            }
        }

        loop {
            {
                let ir = ir.lock().unwrap();
                if let Some(ir) = ir.as_ref() {
                    assert!(ir.is_constant == is_constant);
                    return (ir.clone(), Cow::Borrowed(subs));
                }
            }

            if self.vm.is_verbose {
                println!("converting ir for {}{}", self.path.as_string(), subs);
            }

            self.vm.build_function_ir(self.crate_id, self.item_id);
        }
    }

    /// Try getting the IR without any complicated lookup.
    /// This is used when saving IR to the disk.
    pub fn raw_ir(&self) -> Option<Arc<IRFunction<'vm>>> {
        let ir = match &self.kind {
            ItemKind::Function { ir, .. } => ir,
            ItemKind::Constant { ir, .. } => ir,
            _ => panic!("item kind mismatch"),
        };

        let ir = ir.lock().unwrap();

        ir.clone()
    }

    pub fn set_ir(&self, new_ir: IRFunction<'vm>) {
        let ir = match &self.kind {
            ItemKind::Function { ir, .. } => ir,
            ItemKind::Constant { ir, .. } => ir,
            _ => panic!("item kind mismatch"),
        };

        let mut dest = ir.lock().unwrap();

        *dest = Some(Arc::new(new_ir));
    }

    pub fn const_value(&self, subs: &SubList<'vm>) -> &'vm [u8] {
        let (ir, new_subs) = self.ir(subs);

        let bc = BytecodeCompiler::compile(self.vm, &ir, &new_subs, self.path.as_string());

        let const_thread = self.vm.make_thread();
        const_thread.run_bytecode(&bc, 0);
        let ty = ir.sig.output; // todo sub?

        let const_bytes = const_thread.copy_result(ty.layout().assert_size() as usize);
        self.vm.alloc_constant(const_bytes)
    }

    pub fn ctor_info(&self) -> Option<(ItemId, u32)> {
        match &self.kind {
            ItemKind::Function { ctor_for, .. } => ctor_for.clone(),
            ItemKind::Constant { ctor_for, .. } => ctor_for.clone(),
            _ => panic!("item kind mismatch"),
        }
    }

    pub fn adt_info(&self) -> &AdtInfo<'vm> {
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
        let ItemKind::AssociatedType{virtual_info} = &self.kind else {
            panic!("item kind mismatch");
        };

        let crate_items = self.vm.get_crate_items(self.crate_id);
        let trait_item = crate_items.get(virtual_info.trait_id);

        trait_item
            .find_trait_impl(subs, &mut None, |trait_impl, subs| {
                let ty = trait_impl.assoc_tys.get(&virtual_info.ident);

                if let Some(ty) = ty {
                    ty.sub(&subs)
                } else {
                    panic!("failed to find associated type")
                }
            })
            .unwrap_or_else(|| {
                panic!("failed to find {} for {}", self.path.as_string(), subs);
            })
    }

    fn find_trait_item_ir(
        &self,
        subs: &SubList<'vm>,
        member_name: &str,
    ) -> Option<(Arc<IRFunction<'vm>>, SubList<'vm>)> {
        self.find_trait_impl(subs, &mut None, |trait_impl, subs| {
            let crate_items = self.vm.get_crate_items(trait_impl.crate_id);

            let ir_source = trait_impl.assoc_values.get(member_name);

            if let Some(ir_source) = ir_source {
                match ir_source {
                    AssocValue::Item(fn_item_id) => {
                        let fn_item = crate_items.get(*fn_item_id);
                        let (ir, _) = fn_item.ir(&subs);
                        return Some((ir, subs));
                    }
                    AssocValue::RawFunctionIR(ir) => return Some((ir.clone(), subs)),
                }
            } else {
                None
            }
        })
        .unwrap_or_else(|| {
            panic!("failed to find {} for {}", self.path.as_string(), subs);
        })
    }

    pub fn trait_has_impl(
        &self,
        for_tys: &SubList<'vm>,
        update_tys: &mut Option<&mut SubList<'vm>>,
    ) -> bool {
        self.find_trait_impl(for_tys, update_tys, |_, _| ())
            .is_some()
    }

    /// Find a trait implementation for a given list of types.
    pub fn find_trait_impl<T>(
        &self,
        for_tys: &SubList<'vm>,
        update_tys: &mut Option<&mut SubList<'vm>>,
        callback: impl FnOnce(&TraitImpl<'vm>, SubList<'vm>) -> T,
    ) -> Option<T> {
        let ItemKind::Trait{impl_list,builtin} = &self.kind else {
            panic!("item kind mismatch");
        };

        if let Some(builtin) = builtin.get() {
            let builtin_res = builtin.find_candidate(for_tys, self.vm);
            if let Some(candidate) = builtin_res {
                if let Some(trait_subs) = self.check_trait_impl(for_tys, &candidate, update_tys) {
                    return Some(callback(&candidate, trait_subs));
                }
            }
        }

        let impl_list = impl_list.read().unwrap();

        for candidate in impl_list.iter() {
            if let Some(trait_subs) = self.check_trait_impl(for_tys, candidate, update_tys) {
                return Some(callback(candidate, trait_subs));
            }
        }

        None
    }

    /// Builds a sub list for the impl, and checks it against the impl bounds.
    fn check_trait_impl(
        &self,
        for_tys: &SubList<'vm>,
        candidate: &TraitImpl<'vm>,
        update_tys: &mut Option<&mut SubList<'vm>>,
    ) -> Option<SubList<'vm>> {
        if let Some(sub_map) = trait_match(for_tys, &candidate.for_types) {
            let mut trait_subs = SubList::from_summary(&candidate.generics, self.vm);
            sub_map.apply_to(SubSide::Rhs, &mut trait_subs);

            if candidate.bounds.len() > 0 {
                for bound in &candidate.bounds {
                    match bound {
                        BoundKind::Trait(trait_bound) => {
                            let types_to_check = trait_bound.subs.sub(&trait_subs);
                            let res = trait_bound
                                .item
                                .trait_has_impl(&types_to_check, &mut Some(&mut trait_subs));

                            if !res {
                                return None;
                            }
                        }
                        BoundKind::Projection(assoc_ty, eq_ty) => {
                            let types_to_check = assoc_ty.subs.sub(&trait_subs);

                            let resolved_assoc_ty =
                                assoc_ty.item.resolve_associated_ty(&types_to_check);

                            let mut res_map = Default::default();

                            if !type_match(resolved_assoc_ty, *eq_ty, &mut res_map) {
                                panic!("unmatched {} = {}", resolved_assoc_ty, eq_ty);
                            }
                            res_map.assert_empty(SubSide::Lhs);
                            res_map.apply_to(SubSide::Rhs, &mut trait_subs);
                        }
                    }
                }
            }

            // Is this needed?
            assert!(trait_subs.is_concrete());

            //sub_map.assert_empty(SubSide::Lhs);
            // yucky. this does appear to be needed for some code (see iter test!)
            // probably not a great way of doing things though
            /*if let Some(update_tys) = update_tys {
                sub_map.apply_to(SubSide::Lhs, update_tys);
            }*/
            Some(trait_subs)
        } else {
            None
        }
    }
}

#[derive(PartialEq, Debug)]
enum SubSide {
    Lhs,
    Rhs,
}

#[derive(Default)]
struct SubMap<'vm> {
    map: Vec<((SubSide, u32), Type<'vm>)>,
}

impl<'vm> SubMap<'vm> {
    fn set(&mut self, side: SubSide, n: u32, val: Type<'vm>) -> bool {
        let key = (side, n);
        for (ek, ev) in &self.map {
            if *ek == key {
                assert!(*ev == val);
                return true;
            }
        }
        self.map.push((key, val));
        true
    }

    fn apply_to(&self, target_side: SubSide, target_subs: &mut SubList<'vm>) {
        for ((side, n), val) in &self.map {
            if *side == target_side {
                target_subs.list[*n as usize] = Sub::Type(*val);
            }
        }
    }

    fn assert_empty(&self, target_side: SubSide) {
        for ((side, _), _) in &self.map {
            if *side == target_side {
                for entry in &self.map {
                    println!(" - {:?}", entry);
                }
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
            (Sub::Type(lhs_ty), Sub::Type(rhs_ty)) => {
                if !type_match(*lhs_ty, *rhs_ty, res_map) {
                    return false;
                }
            }
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
    if lhs_ty.is_concrete() && lhs_ty == rhs_ty {
        return true;
    }

    match (lhs_ty.kind(), rhs_ty.kind()) {
        (TypeKind::Adt(a), TypeKind::Adt(b)) => {
            (a.item == b.item) && subs_match(&a.subs, &b.subs, res_map)
        }
        (TypeKind::Ptr(in_ref, in_mut), TypeKind::Ptr(trait_ref, trait_mut)) => {
            (in_mut == trait_mut) && type_match(*in_ref, *trait_ref, res_map)
        }
        (TypeKind::Ref(in_ref, in_mut), TypeKind::Ref(trait_ref, trait_mut)) => {
            (in_mut == trait_mut) && type_match(*in_ref, *trait_ref, res_map)
        }
        (TypeKind::Slice(in_elem), TypeKind::Slice(trait_elem)) => {
            type_match(*in_elem, *trait_elem, res_map)
        }
        (TypeKind::Tuple(lhs_children), TypeKind::Tuple(rhs_children)) => {
            if lhs_children.len() != rhs_children.len() {
                false
            } else {
                for (lhs_child, rhs_child) in lhs_children.iter().zip(rhs_children) {
                    if !type_match(*lhs_child, *rhs_child, res_map) {
                        return false;
                    }
                }
                true
            }
        }

        (TypeKind::Param(lhs_param), TypeKind::Param(rhs_param)) => {
            panic!("fixme? this looks annoying");
        }

        (_, TypeKind::Param(param_num)) => res_map.set(SubSide::Rhs, *param_num, lhs_ty),
        (TypeKind::Param(param_num), _) => res_map.set(SubSide::Lhs, *param_num, rhs_ty),

        (TypeKind::Adt(_), _)
        | (_, TypeKind::Adt(_))
        | (TypeKind::Ptr(..), _)
        | (_, TypeKind::Ptr(..))
        | (TypeKind::Ref(..), _)
        | (_, TypeKind::Ref(..))
        | (TypeKind::Bool, _)
        | (_, TypeKind::Bool)
        | (TypeKind::Char, _)
        | (_, TypeKind::Char)
        | (TypeKind::Never, _)
        | (_, TypeKind::Never)
        | (TypeKind::Int(..), _)
        | (_, TypeKind::Int(..))
        | (TypeKind::Float(..), _)
        | (_, TypeKind::Float(..)) => false,
        _ => {
            panic!("match types {} == {}", lhs_ty, rhs_ty)
        }
    }
}

/// Get a path from rustc.
pub fn path_from_rustc<'vm>(
    in_path: &rustc_hir::definitions::DefPath,
    vm: &'vm VM<'vm>,
) -> ItemPath<'vm> {
    use rustc_hir::definitions::DefPathData;

    let mut result = String::new();

    let mut is_debug = false;

    // we only handle trivial paths
    for elem in in_path.data.iter() {
        match elem.data {
            DefPathData::ValueNs(sym) | DefPathData::TypeNs(sym) => {
                if sym.as_str() == "_" {
                    is_debug = true;
                }
                result.push_str("::");
                result.push_str(sym.as_str());
            }
            DefPathData::Impl => {
                result.push_str("::{impl}");
                is_debug = true;
            }
            DefPathData::Ctor => {
                // do nothing
            }
            DefPathData::ForeignMod => {
                // do nothing
            }
            //DefPathData
            _ => panic!(
                "todo path {:?} {}",
                elem.data,
                in_path.to_string_no_crate_verbose()
            ),
        }
    }

    let result = vm.alloc_path(&result);

    if is_debug {
        ItemPath(NameSpace::DebugOnly, result)
    } else if let Some(last_elem) = in_path.data.last() {
        match last_elem.data {
            DefPathData::ValueNs(_) => ItemPath(NameSpace::Value, result),
            DefPathData::TypeNs(_) => ItemPath(NameSpace::Type, result),
            DefPathData::Ctor => ItemPath(NameSpace::Value, result),
            _ => panic!("can't determine namespace: {:?}", last_elem.data),
        }
    } else {
        panic!("zero element path?");
    }
}
