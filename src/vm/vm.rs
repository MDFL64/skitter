use ahash::{AHashMap, AHashSet};
use colosseum::sync::Arena;

use crate::abi::align;
use crate::bytecode_compiler::BytecodeCompiler;
use crate::bytecode_compiler::FunctionBytecode;
use crate::cache_provider::CacheProvider;
use crate::cli::CliArgs;
use crate::closure::Closure;
use crate::closure::ClosureRef;
use crate::crate_provider::CrateProvider;
use crate::crate_provider::TraitImplResult;
use crate::ir::IRFunction;
use crate::items::AssocValue;
use crate::items::CrateId;
use crate::items::Item;
use crate::items::ItemPath;
use crate::rustc_worker::RustCWorker;
use crate::rustc_worker::RustCWorkerConfig;
use crate::types::CommonTypes;
use crate::types::DropGlue;
use crate::types::ItemWithSubs;
use crate::types::Sub;
use crate::types::SubList;
use crate::types::Type;
use crate::types::TypeContext;
use crate::types::TypeKind;
use crate::value_debug::print_value;
use crate::vm::instr::Slot;

use std::{
    borrow::Cow, sync::atomic::AtomicPtr, sync::atomic::Ordering, sync::Arc, sync::Mutex,
    sync::OnceLock, sync::RwLock,
};

use super::externs::get_extern_fn;
use super::externs::get_extern_static;
use super::{read_stack, write_stack};

use super::instr::Instr;

pub struct VM<'vm> {
    pub cli_args: CliArgs,
    pub types: TypeContext<'vm>,

    pub core_crate: OnceLock<CrateId>,
    pub alloc_crate: OnceLock<CrateId>,
    pub std_crate: OnceLock<CrateId>,

    common_types: OnceLock<CommonTypes<'vm>>,
    crates: RwLock<Vec<&'vm Box<dyn CrateProvider<'vm>>>>,

    stack_pool: Mutex<Vec<Vec<u128>>>,

    arena_crates: Arena<Box<dyn CrateProvider<'vm>>>,
    arena_items: Arena<Item<'vm>>,
    arena_functions: Arena<Function<'vm>>,
    arena_closures: Arena<Closure<'vm>>,
    arena_bytecode: Arena<FunctionBytecode<'vm>>,
    arena_constants: Arena<Vec<u8>>,
    arena_paths: Arena<String>,
    arena_vtables: Arena<VTable<'vm>>,

    map_paths: Mutex<AHashSet<&'vm str>>,
    map_vtables: Mutex<AHashMap<(&'vm Item<'vm>, SubList<'vm>), &'vm VTable<'vm>>>,

    drop_trait: OnceLock<&'vm Item<'vm>>,
}

static TRACE_CALL_DEPTH: Mutex<usize> = Mutex::new(0);

pub struct VMThread<'vm> {
    vm: &'vm VM<'vm>,
    stack: Vec<u128>,
    drop_flags: Vec<u8>,
}

impl<'vm> VMThread<'vm> {
    pub fn call(&mut self, func: &Function<'vm>, stack_offset: u32) {
        let native = func.get_native();

        if self.vm.cli_args.debug_trace_calls && !native.is_some() {
            let mut call_depth = TRACE_CALL_DEPTH.lock().unwrap();

            let (ir, inner_subs) = func.source.ir(&func.subs);

            let ret_ty = ir.sig.output.sub(&inner_subs);

            let mut arg_offset = ret_ty.layout().assert_size();

            let arg_ptr = unsafe { (self.stack.as_ptr() as *mut u8).offset(stack_offset as isize) };

            for _ in 0..*call_depth {
                print!("  ");
            }
            *call_depth += 1;

            print!("CALL {}( ", func.source.debug_name());

            for (i, arg_ty) in ir.sig.inputs.iter().enumerate() {
                let arg_ty = arg_ty.sub(&inner_subs);
                let arg_layout = arg_ty.layout();
                if i != 0 {
                    print!(" , ");
                }
                arg_offset = align(arg_offset, arg_layout.align);
                unsafe { print_value(arg_ty, arg_ptr.offset(arg_offset as isize), 0) }
                arg_offset += arg_layout.assert_size();
            }
            println!(" )");
        }

        if let Some(native) = native {
            unsafe {
                let stack = (self.stack.as_ptr() as *mut u8).offset(stack_offset as isize);
                native(stack);
            }
            return;
        }

        // fetch bytecode
        let bc = func.bytecode();
        self.run_bytecode(bc, stack_offset);

        if self.vm.cli_args.debug_trace_calls && !native.is_some() {
            let mut call_depth = TRACE_CALL_DEPTH.lock().unwrap();

            let (ir, inner_subs) = func.source.ir(&func.subs);

            let ret_ty = ir.sig.output.sub(&inner_subs);

            let ret_ptr = unsafe { (self.stack.as_ptr() as *mut u8).offset(stack_offset as isize) };

            *call_depth -= 1;
            for _ in 0..*call_depth {
                print!("  ");
            }

            print!("RET  {} -> ", func.source.debug_name());
            unsafe { print_value(ret_ty, ret_ptr, 0) }
            println!();
        }
    }

    pub fn run_bytecode(&mut self, func: &FunctionBytecode<'vm>, stack_offset: u32) {
        let drops_base = self.drop_flags.len();
        if func.drops.len() > 0 {
            let bytes = (func.drops.len() - 1) / 8 + 1;
            self.drop_flags.resize(drops_base + bytes, 0);
        }

        unsafe {
            let mut pc = 0;
            let stack = (self.stack.as_ptr() as *mut u8).offset(stack_offset as isize);

            loop {
                let instr = &func.code[pc];
                include!(concat!(env!("OUT_DIR"), "/exec_match.rs"));
                pc += 1;
            }

            for i in (0..func.drops.len()).rev() {
                self.local_drop(drops_base, i as u32, &func.drops, stack);
            }
        }

        self.drop_flags.truncate(drops_base);
    }

    pub fn copy_result(&self, offset: usize, size: usize) -> Vec<u8> {
        let ptr = self.stack.as_ptr() as *mut u8;
        let slice = unsafe { std::slice::from_raw_parts(ptr.offset(offset as isize), size) };
        slice.to_vec()
    }

    pub fn copy_ptr(&self, slot: Slot) -> usize {
        unsafe { read_stack(self.stack.as_ptr() as _, slot) }
    }

    fn local_init(&mut self, base: usize, n: u32) {
        let offset = (n >> 8) as usize;
        let byte = &mut self.drop_flags[base + offset];

        let bit = n & 7;
        let mask = 1u8 << (bit as u8);

        if (*byte & mask) != 0 {
            panic!("drop flag was initialized twice");
        }

        *byte |= mask;
    }

    fn local_move(&mut self, base: usize, n: u32) {
        let offset = (n >> 8) as usize;
        let byte = &mut self.drop_flags[base + offset];

        let bit = n & 7;
        let mask = 1u8 << (bit as u8);

        if (*byte & mask) == 0 {
            panic!("attempt to clear uninitialized drop flag");
        }

        *byte ^= mask;
    }

    unsafe fn local_drop(
        &mut self,
        base: usize,
        n: u32,
        drops: &[(Slot, DropGlue<'vm>)],
        stack: *mut u8,
    ) {
        let offset = (n >> 8) as usize;
        let byte = &mut self.drop_flags[base + offset];

        let bit = n & 7;
        let mask = 1u8 << (bit as u8);

        if (*byte & mask) != 0 {
            *byte ^= mask;

            let (slot, glue) = drops[n as usize];

            let slot_ptr = stack.add(slot.index());

            let mut drop_thread = self.vm.make_thread();
            write_stack(drop_thread.stack.as_mut_ptr() as _, Slot::new(0), slot_ptr);

            drop_thread.call(glue.function(), 0);
        }
    }

    unsafe fn local_drop_init(
        &mut self,
        base: usize,
        n: u32,
        drops: &[(Slot, DropGlue<'vm>)],
        stack: *mut u8,
    ) {
        let offset = (n >> 8) as usize;
        let byte = &mut self.drop_flags[base + offset];

        let bit = n & 7;
        let mask = 1u8 << (bit as u8);

        if (*byte & mask) != 0 {
            let (slot, glue) = drops[n as usize];

            let slot_ptr = stack.add(slot.index());

            let mut drop_thread = self.vm.make_thread();
            write_stack(drop_thread.stack.as_mut_ptr() as _, Slot::new(0), slot_ptr);

            drop_thread.call(glue.function(), 0);
        } else {
            *byte |= mask;
        }
    }
}

impl<'vm> std::ops::Drop for VMThread<'vm> {
    fn drop(&mut self) {
        let stack = std::mem::take(&mut self.stack);

        // TODO provide some upper limit on stack pool size?
        let mut stack_pool = self.vm.stack_pool.lock().unwrap();
        stack_pool.push(stack);
    }
}

impl<'vm> VM<'vm> {
    pub fn new(cli_args: &CliArgs) -> Self {
        Self {
            cli_args: cli_args.clone(),

            core_crate: OnceLock::new(),
            alloc_crate: OnceLock::new(),
            std_crate: OnceLock::new(),

            types: TypeContext::new(),
            crates: Default::default(),
            common_types: OnceLock::new(),

            stack_pool: Default::default(),

            arena_crates: Arena::new(),
            arena_items: Arena::new(),
            arena_functions: Arena::new(),
            arena_bytecode: Arena::new(),
            arena_constants: Arena::new(),
            arena_paths: Arena::new(),
            arena_closures: Arena::new(),
            arena_vtables: Arena::new(),

            map_paths: Default::default(),
            map_vtables: Default::default(),

            drop_trait: Default::default(),
        }
    }

    pub fn setup_common_types() {
        //vm.common_types = Some(CommonTypes::new(&vm));
    }

    pub fn make_thread(&'vm self) -> VMThread<'vm> {
        let stack = {
            let mut stack_pool = self.stack_pool.lock().unwrap();
            // 1M stack
            stack_pool.pop().unwrap_or_else(|| vec![0; 65536])
        };
        VMThread {
            vm: self,
            stack,
            drop_flags: Vec::new(),
        }
    }

    /// Attempts to build a cache provider for the given crate, then falls back to rustc if that fails.
    ///
    /// I tried for so long to get this to work with scoped threads.
    /// Got it working, and then had it break again when transitioning off THIR.
    ///
    /// To hell with it. Just require a static VM to use a rustc worker.
    pub fn add_provider_auto(&'static self, worker_config: RustCWorkerConfig) -> CrateId {
        let mut crates = self.crates.write().unwrap();
        let crate_id = CrateId::new(crates.len() as u32);

        // attempt to load cached IR -- but not if we want to save
        if !worker_config.save_file {
            match CacheProvider::new(&worker_config, self, crate_id) {
                Ok(provider) => {
                    let worker_ref = self.arena_crates.alloc(Box::new(provider));
                    crates.push(worker_ref);

                    return crate_id;
                }
                Err(_) => {
                    if worker_config.crate_path.is_internal() {
                        let this_exe = std::env::current_exe().unwrap();
                        let this_exe = this_exe.file_name().unwrap().to_str().unwrap();
                        eprintln!(
                            "| WARNING: Failed to load internal crate '{}' from cache.",
                            worker_config.crate_path.name
                        );
                        eprintln!("| Falling back to rustc. This will be slow.");
                        eprintln!(
                            "| To cache the crate, run: {} @{} --save",
                            this_exe, worker_config.crate_path.name
                        );
                    }
                }
            }
        }

        let worker = Box::new(RustCWorker::new(worker_config, self, crate_id));

        let worker_ref = self.arena_crates.alloc(worker);
        crates.push(worker_ref);

        crate_id
    }

    pub fn crate_provider(&self, crate_id: CrateId) -> &'vm Box<dyn CrateProvider<'vm>> {
        let crates = self.crates.read().unwrap();
        crates[crate_id.index()]
    }

    pub fn alloc_function(
        &'vm self,
        source: FunctionSource<'vm>,
        subs: SubList<'vm>,
    ) -> &'vm Function<'vm> {
        let func = Function {
            source,
            subs,
            native: Default::default(),
            bytecode: Default::default(),
        };

        if let FunctionSource::Item(item) = source {
            let extern_native = get_extern_fn(item);

            if let Some(extern_native) = extern_native {
                func.set_native(extern_native);
            }
        }

        self.arena_functions.alloc(func)
    }

    pub fn static_value(&'vm self, static_ref: &ItemWithSubs<'vm>) -> *mut u8 {
        if let Some(res) = get_extern_static(static_ref.item) {
            res
        } else {
            static_ref.item.static_value(&static_ref.subs)
        }
    }

    pub fn alloc_item(&'vm self, item: Item<'vm>) -> &'vm Item<'vm> {
        self.arena_items.alloc(item)
    }

    pub fn alloc_bytecode(&'vm self, bc: FunctionBytecode<'vm>) -> &'vm FunctionBytecode<'vm> {
        self.arena_bytecode.alloc(bc)
    }

    /// TODO use read-only allocations, at least as a debug option
    pub fn alloc_constant(&'vm self, str: Vec<u8>) -> &'vm [u8] {
        self.arena_constants.alloc(str)
    }

    /// FIXME this is UNSOUND if the static is mutated!
    pub fn alloc_static(&'vm self, str: Vec<u8>) -> *mut u8 {
        self.arena_constants.alloc(str).as_mut_ptr()
    }

    pub fn alloc_closure(&'vm self, closure: Closure<'vm>) -> ClosureRef<'vm> {
        let closure = self.arena_closures.alloc(closure);

        ClosureRef::new(closure)
    }

    pub fn alloc_path(&'vm self, path: &str) -> &'vm str {
        let mut map_paths = self.map_paths.lock().unwrap();
        if let Some(existing) = map_paths.get(path) {
            existing
        } else {
            let res = self.arena_paths.alloc(path.to_owned());
            map_paths.insert(res);
            res
        }
    }

    pub fn find_vtable(
        &'vm self,
        trait_item: &'vm Item<'vm>,
        primary_ty: Type<'vm>,
    ) -> &'vm VTable<'vm> {
        let for_tys = SubList {
            list: vec![Sub::Type(primary_ty)],
        };

        assert!(for_tys.is_concrete());

        let mut map_vtables = self.map_vtables.lock().unwrap();

        map_vtables
            .entry((trait_item, for_tys.clone()))
            .or_insert_with(|| {
                let TraitImplResult::Static(impl_result) = trait_item
                    .find_trait_impl(&for_tys) else {
                        panic!("cannot build vtable from trait {} for {}",trait_item.path.as_string(),primary_ty);
                    };

                let impl_crate = self.crate_provider(impl_result.crate_id);

                // TODO include methods in the base trait def?
                // can specialization cause issues?
                let methods: Vec<_> = impl_result
                    .assoc_values
                    .iter()
                    .map(|val| match val {
                        Some(AssocValue::Item(item_id)) => {
                            let item = impl_crate.item_by_id(*item_id);
                            if item.is_function() {
                                Some(item.func_mono(&impl_result.impl_subs))
                            } else {
                                None
                            }
                        }
                        _ => {
                            println!("{:?}", val);
                            panic!();
                        }
                    })
                    .collect();

                let layout = primary_ty.layout();

                self.arena_vtables.alloc(VTable {
                    size: layout.assert_size(),
                    align: layout.align,
                    methods,
                })
            })
    }

    pub fn find_drop(&self, ty: Type<'vm>) -> Option<&'vm Item<'vm>> {
        let drop_trait = self.drop_trait.get_or_init(|| {
            let core_id = *self.core_crate.get().expect("no core crate");
            let core_provider = self.crate_provider(core_id);

            core_provider
                .item_by_path(&ItemPath::for_type("::ops::drop::Drop"))
                .expect("no drop trait")
        });

        let drop_impl = drop_trait.find_trait_impl(&SubList {
            list: vec![Sub::Type(ty)],
        });

        match drop_impl {
            TraitImplResult::Dynamic => {
                panic!("dyn drop, is this even valid?");
            }
            TraitImplResult::Static(drop_impl) => {
                let val = drop_impl.assoc_values[0].as_ref().unwrap();
                let AssocValue::Item(item_id) = val else {
                    panic!("drop impl is not an item");
                };

                let provider = self.crate_provider(drop_impl.crate_id);
                let item = provider.item_by_id(*item_id);
                Some(item)
            }
            TraitImplResult::None => None,
        }
    }

    pub fn common_types(&'vm self) -> &CommonTypes<'vm> {
        self.common_types.get_or_init(|| CommonTypes::new(self))
    }

    pub fn ty_func_def(&'vm self, def: ItemWithSubs<'vm>) -> Type<'vm> {
        self.types.intern(TypeKind::FunctionDef(def), self)
    }

    pub fn ty_adt(&'vm self, def: ItemWithSubs<'vm>) -> Type<'vm> {
        self.types.intern(TypeKind::Adt(def), self)
    }

    pub fn ty_tuple(&'vm self, children: Vec<Type<'vm>>) -> Type<'vm> {
        self.types.intern(TypeKind::Tuple(children), self)
    }

    /// All System allocations in the VM should run through this.
    /// Size and alloc must describe a valid Layout!
    pub unsafe fn alloc_bytes(&'vm self, size: usize, align: usize) -> *mut u8 {
        let layout = std::alloc::Layout::from_size_align_unchecked(size, align);
        std::alloc::alloc(layout)
    }

    pub unsafe fn alloc_bytes_zeroed(&'vm self, size: usize, align: usize) -> *mut u8 {
        let layout = std::alloc::Layout::from_size_align_unchecked(size, align);
        std::alloc::alloc_zeroed(layout)
    }

    pub unsafe fn realloc(
        &'vm self,
        old: *mut u8,
        old_size: usize,
        align: usize,
        new_size: usize,
    ) -> *mut u8 {
        //println!("realloc {:?} {} / {} -> {}",old,align,old_size,new_size);
        let layout = std::alloc::Layout::from_size_align_unchecked(old_size, align);
        std::alloc::realloc(old, layout, new_size)
    }
}

#[derive(Copy, Clone)]
pub enum FunctionSource<'vm> {
    Item(&'vm Item<'vm>),
    Closure(&'vm Closure<'vm>),
    RawBytecode(&'vm FunctionBytecode<'vm>),
}

impl<'vm> FunctionSource<'vm> {
    pub fn vm(&self) -> &'vm VM<'vm> {
        match self {
            Self::Item(item) => item.vm,
            Self::Closure(closure) => closure.vm,
            Self::RawBytecode(_) => panic!("raw bytecode has no vm"),
        }
    }

    pub fn ir<'a>(&self, subs: &'a SubList<'vm>) -> (Arc<IRFunction<'vm>>, Cow<'a, SubList<'vm>>) {
        match self {
            Self::Item(item) => item.ir(subs),
            Self::Closure(closure) => (closure.ir_base(), Cow::Borrowed(subs)),
            Self::RawBytecode(_) => panic!("raw bytecode has no ir"),
        }
    }

    pub fn debug_name(&self) -> &str {
        match self {
            Self::Item(item) => item.path.as_string(),
            Self::Closure(_) => "[closure]",
            Self::RawBytecode(..) => "[raw bytecode, probably drop glue]",
        }
    }
}

/// A monomorphized function which may contain bytecode or machine code
pub struct Function<'vm> {
    source: FunctionSource<'vm>,
    subs: SubList<'vm>,
    /// Store a void pointer because function pointers can't be stored by AtomicPtr(?)
    native: AtomicPtr<std::ffi::c_void>,
    bytecode: AtomicPtr<FunctionBytecode<'vm>>,
}

impl<'vm> Function<'vm> {
    pub fn get_native(&self) -> Option<unsafe fn(*mut u8)> {
        let raw = self.native.load(Ordering::Acquire);
        if raw.is_null() {
            None
        } else {
            Some(unsafe { std::mem::transmute(raw) })
        }
    }

    pub fn get_bytecode(&self) -> Option<&'vm FunctionBytecode<'vm>> {
        let raw = self.bytecode.load(Ordering::Acquire);
        if raw.is_null() {
            None
        } else {
            Some(unsafe { &*raw })
        }
    }

    pub fn set_native(&self, native: unsafe fn(*mut u8, &'vm VM<'vm>)) {
        self.native
            .store(unsafe { std::mem::transmute(native) }, Ordering::Release);
    }

    pub fn set_bytecode(&self, bc: &'vm FunctionBytecode<'vm>) {
        self.bytecode.store(bc as *const _ as _, Ordering::Release);
    }

    fn bytecode(&self) -> &'vm FunctionBytecode<'vm> {
        loop {
            if let Some(bc) = self.get_bytecode() {
                return bc;
            }

            let bc = if let FunctionSource::RawBytecode(bc) = self.source {
                bc
            } else {
                let vm = self.source.vm();
                let (ir, new_subs) = self.source.ir(&self.subs);
                let path = self.source.debug_name();

                let bc = BytecodeCompiler::compile(vm, &ir, &new_subs, path, &self.subs);
                vm.alloc_bytecode(bc)
            };

            self.set_bytecode(bc);
        }
    }
}

pub struct VTable<'vm> {
    size: u32,
    align: u32,
    // TODO drop???
    // currently only stores methods of the primary trait -- no super traits
    methods: Vec<Option<&'vm Function<'vm>>>,
}

impl<'vm> std::fmt::Debug for Function<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Function(\"{}{}\")", self.source.debug_name(), self.subs)
    }
}
