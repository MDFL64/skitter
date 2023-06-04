use ahash::AHashSet;
use colosseum::sync::Arena;

use crate::bytecode_compiler::BytecodeCompiler;
use crate::items::CrateId;
use crate::items::CrateItems;
use crate::items::Item;
use crate::items::ItemId;
use crate::rustc_worker::RustCWorker;
use crate::rustc_worker::RustCWorkerConfig;
use crate::types::IntSign;
use crate::types::IntWidth;
use crate::types::ItemWithSubs;
use crate::types::SubList;
use crate::types::Type;
use crate::types::TypeContext;
use crate::types::TypeKind;
use crate::vm::instr::Slot;
use std::sync::atomic::Ordering;
use std::sync::Mutex;
use std::sync::RwLock;

use std::sync::atomic::AtomicPtr;

use super::instr::Instr;

pub struct VM<'vm> {
    pub types: TypeContext<'vm>,
    pub is_verbose: bool,
    crates: RwLock<Vec<Crate<'vm>>>,

    arena_items: Arena<CrateItems<'vm>>,
    arena_functions: Arena<Function<'vm>>,
    arena_bytecode: Arena<Vec<Instr<'vm>>>,
    arena_constants: Arena<Vec<u8>>,
    arena_paths: Arena<String>,

    map_paths: Mutex<AHashSet<&'vm str>>,
}

struct Crate<'vm> {
    rustc_worker: Option<RustCWorker<'vm>>,
    items: Option<&'vm CrateItems<'vm>>
}

pub struct VMThread<'vm> {
    vm: &'vm VM<'vm>,
    stack: Vec<u128>,
}

impl<'vm> VMThread<'vm> {
    pub fn call(&self, func: &Function<'vm>, stack_offset: u32) {
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

    pub fn copy_result(&self, size: usize) -> Vec<u8> {
        let ptr = self.stack.as_ptr() as *mut u8;
        let slice = unsafe { std::slice::from_raw_parts(ptr, size) };
        slice.to_vec()
    }
}

impl<'vm> VM<'vm> {
    pub fn new() -> Self {
        Self {
            //functions: Default::default(),
            types: TypeContext::new(),
            is_verbose: false,
            crates: Default::default(),

            arena_items: Arena::new(),
            arena_functions: Arena::new(),
            arena_bytecode: Arena::new(),
            arena_constants: Arena::new(),
            arena_paths: Arena::new(),

            map_paths: Default::default(),
        }
    }

    pub fn make_thread(&'vm self) -> VMThread<'vm> {
        VMThread {
            vm: self,
            // 64k stack
            stack: vec![0; 4096],
        }
    }

    pub fn add_worker<'s>(
        &'vm self,
        worker_config: RustCWorkerConfig,
        scope: &'s std::thread::Scope<'s, 'vm>,
    ) -> CrateId {
        let mut crates = self.crates.write().unwrap();
        let crate_id = CrateId::new(crates.len() as u32);
        let worker = RustCWorker::new(worker_config, scope, self, crate_id);

        crates.push(Crate {
            rustc_worker: Some(worker),
            items: None
        });
        crate_id
    }

    pub fn set_crate_items(
        &'vm self,
        crate_id: CrateId,
        items: CrateItems<'vm>,
    ) -> &'vm CrateItems<'vm> {
        let mut crates = self.crates.write().unwrap();
        let items_ref = self.arena_items.alloc(items);
        crates[crate_id.index()].items = Some(items_ref);
        items_ref
    }

    pub fn get_crate_items(&'vm self, crate_id: CrateId) -> &'vm CrateItems<'vm> {
        let crates = self.crates.read().unwrap();
        crates[crate_id.index()]
            .items
            .expect("crate is missing items")
    }

    pub fn wait_for_setup(&self, crate_id: CrateId) {
        // structured like this to prevent deadlocking the RwLock
        let res = {
            let crates = self.crates.read().unwrap();
            let worker = crates[crate_id.index()].rustc_worker.as_ref().unwrap();
            worker.wait_for_setup()
        };
        res.wait();
    }

    pub fn build_function_ir(&self, crate_id: CrateId, item_id: ItemId) {
        let crates = self.crates.read().unwrap();
        let worker = crates[crate_id.index()].rustc_worker.as_ref().unwrap();
        worker.build_function_ir(item_id);
    }

    pub fn alloc_function(
        &'vm self,
        item: &'vm Item<'vm>,
        subs: SubList<'vm>,
    ) -> &'vm Function<'vm> {
        let func = Function {
            item,
            subs,
            native: Default::default(),
            bytecode: Default::default(),
        };

        let path = item.path.as_string();
        if path.starts_with("::_builtin::") {
            match path {
                "::_builtin::print_int" => func.set_native(builtin_print_int),
                "::_builtin::print_uint" => func.set_native(builtin_print_uint),
                "::_builtin::print_float" => func.set_native(builtin_print_float),
                "::_builtin::print_bool" => func.set_native(builtin_print_bool),
                "::_builtin::print_char" => func.set_native(builtin_print_char),
                _ => panic!("unknown builtin {}", path),
            }
        }

        self.arena_functions.alloc(func)
    }

    pub fn alloc_bytecode(&'vm self, bc: Vec<Instr<'vm>>) -> &Vec<Instr<'vm>> {
        self.arena_bytecode.alloc(bc)
    }

    pub fn alloc_constant(&'vm self, str: Vec<u8>) -> &'vm [u8] {
        self.arena_constants.alloc(str)
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

    pub fn ty_usize(&'vm self) -> Type<'vm> {
        self.types
            .intern(TypeKind::Int(IntWidth::ISize, IntSign::Unsigned), self)
    }

    pub fn ty_bool(&'vm self) -> Type<'vm> {
        self.types.intern(TypeKind::Bool, self)
    }

    pub fn ty_func_def(&'vm self, def: ItemWithSubs<'vm>) -> Type<'vm> {
        self.types.intern(TypeKind::FunctionDef(def),self)
    }

    pub fn ty_adt(&'vm self, def: ItemWithSubs<'vm>) -> Type<'vm> {
        self.types.intern(TypeKind::Adt(def),self)
    }

    pub fn ty_unknown(&'vm self) -> Type<'vm> {
        self.types.intern(TypeKind::Unknown, self)
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

/// A monomorphized function which may contain bytecode or machine code
pub struct Function<'vm> {
    item: &'vm Item<'vm>,
    subs: SubList<'vm>,
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

            let (ir, new_subs) = self.item.ir(&self.subs);
            let path = self.item.path.as_string();

            let bc = BytecodeCompiler::compile(self.item.vm, &ir, &new_subs, path);
            let bc_ref = self.item.vm.alloc_bytecode(bc);

            self.set_bytecode(bc_ref);
        }
    }
}

impl<'vm> std::fmt::Debug for Function<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Function(\"{}{}\")",
            self.item.path.as_string(),
            self.subs
        )
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
