use ahash::AHashMap;
use ahash::AHashSet;
use colosseum::sync::Arena;

use crate::bytecode_compiler::BytecodeCompiler;
use crate::cache_provider::CacheProvider;
use crate::cli::CliArgs;
use crate::closure::Closure;
use crate::closure::FnTrait;
use crate::crate_provider::CrateProvider;
use crate::crate_provider::TraitImpl;
use crate::crate_provider::TraitImplResult;
use crate::ir::IRFunction;
use crate::items::AssocValue;
use crate::items::CrateId;
use crate::items::Item;
use crate::items::ItemId;
use crate::persist::PersistReadContext;
use crate::rustc_worker::RustCWorker;
use crate::rustc_worker::RustCWorkerConfig;
use crate::types::CommonTypes;
use crate::types::ItemWithSubs;
use crate::types::Mutability;
use crate::types::Sub;
use crate::types::SubList;
use crate::types::Type;
use crate::types::TypeContext;
use crate::types::TypeKind;
use crate::vm::instr::Slot;
use crate::CratePath;
use std::borrow::Cow;
use std::cell::OnceCell;
use std::error::Error;
use std::path::Path;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::sync::RwLock;

use std::sync::atomic::AtomicPtr;

use super::instr::Instr;

pub struct VM<'vm> {
    pub types: TypeContext<'vm>,
    pub is_verbose: bool,
    pub lookup_local_impls: bool,
    pub core_crate: OnceLock<CrateId>,
    common_types: OnceLock<CommonTypes<'vm>>,
    crates: RwLock<Vec<&'vm Box<dyn CrateProvider<'vm>>>>,

    arena_crates: Arena<Box<dyn CrateProvider<'vm>>>,
    arena_items: Arena<Item<'vm>>,
    arena_functions: Arena<Function<'vm>>,
    arena_closures: Arena<Closure<'vm>>,
    arena_bytecode: Arena<Vec<Instr<'vm>>>,
    arena_constants: Arena<Vec<u8>>,
    arena_paths: Arena<String>,
    arena_vtables: Arena<VTable<'vm>>,

    map_paths: Mutex<AHashSet<&'vm str>>,
    map_vtables: Mutex<AHashMap<(&'vm Item<'vm>, SubList<'vm>), &'vm VTable<'vm>>>,

    next_closure_id: AtomicU32,
}

pub struct VMThread<'vm> {
    vm: &'vm VM<'vm>,
    stack: Vec<u128>,
}

impl<'vm> VMThread<'vm> {
    pub fn call(&self, func: &Function<'vm>, stack_offset: u32) {
        /*{
            let bytes: [u8;16] = unsafe { read_stack(self.stack.as_ptr() as *mut u8, Slot::new(stack_offset)) };
            println!("enter {}{}: {:?}",func.source.debug_name(),func.subs,bytes);
        }*/
        let native = func.get_native();
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
        /*{
            let bytes: [u8;16] = unsafe { read_stack(self.stack.as_ptr() as *mut u8, Slot::new(stack_offset)) };
            println!("exit {}{}: {:?}",func.source.debug_name(),func.subs,bytes);
        }*/
    }

    pub fn run_bytecode(&self, bc: &[Instr<'vm>], stack_offset: u32) {
        unsafe {
            let mut pc = 0;
            let stack = (self.stack.as_ptr() as *mut u8).offset(stack_offset as isize);

            loop {
                let instr = &bc[pc];
                include!(concat!(env!("OUT_DIR"), "/exec_match.rs"));
                pc += 1;
            }
        }
    }

    pub fn copy_result(&self, offset: usize, size: usize) -> Vec<u8> {
        let ptr = self.stack.as_ptr() as *mut u8;
        let slice = unsafe { std::slice::from_raw_parts(ptr.offset(offset as isize), size) };
        slice.to_vec()
    }

    pub fn copy_ptr(&self, slot: Slot) -> usize {
        unsafe { read_stack(self.stack.as_ptr() as _, slot) }
    }
}

impl<'vm> VM<'vm> {
    pub fn new(cli_args: &CliArgs) -> Self {
        Self {
            is_verbose: cli_args.verbose,
            lookup_local_impls: cli_args.debug_local_impls,

            core_crate: OnceLock::new(),
            types: TypeContext::new(),
            crates: Default::default(),
            common_types: OnceLock::new(),

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

            next_closure_id: AtomicU32::new(0),
        }
    }

    pub fn setup_common_types() {
        //vm.common_types = Some(CommonTypes::new(&vm));
    }

    pub fn make_thread(&'vm self) -> VMThread<'vm> {
        VMThread {
            vm: self,
            // 1M stack
            stack: vec![0; 65536],
        }
    }

    /// I tried for so long to get this to work with scoped threads.
    /// Got it working, and then had it break again when transitioning off THIR.
    ///
    /// To hell with it. Just require a static VM to use a rustc worker.
    pub fn add_rustc_provider(&'static self, worker_config: RustCWorkerConfig) -> CrateId {
        let mut crates = self.crates.write().unwrap();
        let crate_id = CrateId::new(crates.len() as u32);

        let worker = Box::new(RustCWorker::new(worker_config, self, crate_id));

        let worker_ref = self.arena_crates.alloc(worker);
        crates.push(worker_ref);

        crate_id
    }

    pub fn add_cache_provider(
        &'vm self,
        crate_path: &CratePath,
    ) -> Result<CrateId, Box<dyn Error>> {
        let mut crates = self.crates.write().unwrap();
        let crate_id = CrateId::new(crates.len() as u32);

        let worker = Box::new(CacheProvider::new(crate_path, self, crate_id)?);

        let worker_ref = self.arena_crates.alloc(worker);
        crates.push(worker_ref);

        Ok(crate_id)
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

        // hack to glue skitter builtins together
        if let FunctionSource::Item(item) = source {
            let path = item.path.as_string();
            if path.starts_with("::_builtin::") {
                match path {
                    "::_builtin::print_int" => func.set_native(builtin_print_int),
                    "::_builtin::print_uint" => func.set_native(builtin_print_uint),
                    "::_builtin::print_float" => func.set_native(builtin_print_float),
                    "::_builtin::print_bool" => func.set_native(builtin_print_bool),
                    "::_builtin::print_char" => func.set_native(builtin_print_char),
                    "::_builtin::print_raw" => func.set_native(builtin_print_raw),
                    _ => panic!("unknown builtin {}", path),
                }
            }
        }

        self.arena_functions.alloc(func)
    }

    pub fn alloc_item(&'vm self, item: Item<'vm>) -> &'vm Item<'vm> {
        self.arena_items.alloc(item)
    }

    pub fn alloc_bytecode(&'vm self, bc: Vec<Instr<'vm>>) -> &'vm Vec<Instr<'vm>> {
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

    pub fn alloc_closure(
        &'vm self,
        def_crate: CrateId,
        def_item: ItemId,
        def_path_indices: Vec<u32>,
    ) -> &'vm Closure<'vm> {
        let n = self.next_closure_id.fetch_add(1, Ordering::AcqRel);

        self.arena_closures
            .alloc(Closure::new(n, def_crate, def_item, def_path_indices, self))
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
}

unsafe fn write_stack<T>(base: *mut u8, slot: Slot, x: T) {
    *(base.add(slot.index()) as *mut _) = x;
}

unsafe fn read_stack<T: Copy>(base: *mut u8, slot: Slot) -> T {
    *(base.add(slot.index()) as *mut _)
}

#[derive(Copy, Clone)]
pub enum FunctionSource<'vm> {
    Item(&'vm Item<'vm>),
    Closure(&'vm Closure<'vm>),
}

impl<'vm> FunctionSource<'vm> {
    pub fn vm(&self) -> &'vm VM<'vm> {
        match self {
            Self::Item(item) => item.vm,
            Self::Closure(closure) => closure.vm,
        }
    }

    pub fn ir<'a>(&self, subs: &'a SubList<'vm>) -> (Arc<IRFunction<'vm>>, Cow<'a, SubList<'vm>>) {
        match self {
            Self::Item(item) => item.ir(subs),
            Self::Closure(closure) => (closure.ir_base(), Cow::Borrowed(subs)),
        }
    }

    pub fn debug_name(&self) -> &str {
        match self {
            Self::Item(item) => item.path.as_string(),
            Self::Closure(closure) => "[closure]",
        }
    }
}

/// A monomorphized function which may contain bytecode or machine code
pub struct Function<'vm> {
    source: FunctionSource<'vm>,
    subs: SubList<'vm>,
    /// Store a void pointer because function pointers can't be stored by this(?)
    native: AtomicPtr<std::ffi::c_void>,
    bytecode: AtomicPtr<Vec<Instr<'vm>>>,
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

    pub fn get_bytecode(&self) -> Option<&'vm Vec<Instr<'vm>>> {
        let raw = self.bytecode.load(Ordering::Acquire);
        if raw.is_null() {
            None
        } else {
            Some(unsafe { &*raw })
        }
    }

    pub fn set_native(&self, native: unsafe fn(*mut u8)) {
        self.native
            .store(unsafe { std::mem::transmute(native) }, Ordering::Release);
    }

    pub fn set_bytecode(&self, bc: &'vm Vec<Instr>) {
        self.bytecode.store(bc as *const _ as _, Ordering::Release);
    }

    fn bytecode(&self) -> &'vm [Instr<'vm>] {
        loop {
            if let Some(bc) = self.get_bytecode() {
                return bc;
            }

            let vm = self.source.vm();
            let (ir, new_subs) = self.source.ir(&self.subs);
            let path = self.source.debug_name();

            let bc = BytecodeCompiler::compile(vm, &ir, &new_subs, path, &self.subs);
            let bc_ref = vm.alloc_bytecode(bc);

            self.set_bytecode(bc_ref);
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

unsafe fn builtin_print_int(stack: *mut u8) {
    let x: i128 = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_uint(stack: *mut u8) {
    let x: u128 = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_float(stack: *mut u8) {
    let x: f64 = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_bool(stack: *mut u8) {
    let x: bool = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_char(stack: *mut u8) {
    let x: char = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_raw(stack: *mut u8) {
    let x: &str = read_stack(stack, Slot::new(0));
    print!("{}", x);
}
