use colosseum::sync::Arena;

use std::sync::RwLock;
use std::sync::atomic::Ordering;
use crate::cli::CliArgs;
use crate::hir_compiler::HirCompiler;
use crate::items::CrateId;
use crate::items::CrateItems;
use crate::items::ExternCrate;
use crate::items::Item;
use crate::items::ItemId;
use crate::rustc_worker::RustCWorker;
use crate::types::Sub;
use crate::types::TypeContext;
use crate::vm::instr::Slot;

use std::sync::atomic::AtomicPtr;

use super::instr::Instr;

pub struct VM<'vm> {
    stack: Vec<u128>,
    //functions: Mutex<HashMap<(DefId,SubstsRef<'tcx>),Arc<Function<'tcx>>>>,
    pub types: TypeContext<'vm>,
    pub is_verbose: bool,
    workers: RwLock<Vec<RustCWorker<'vm>>>,

    arena_items: Arena<CrateItems<'vm>>,
    arena_functions: Arena<Function<'vm>>,
    arena_bytecode: Arena<Vec<Instr<'vm>>>
}

impl<'vm> VM<'vm> {
    pub fn new() -> Self {
        Self {
            // 64k stack - TODO move out of this struct, this is not safe
            stack: vec!(0;4096),
            //functions: Default::default(),
            types: TypeContext::new(),
            is_verbose: false,
            workers: Default::default(),

            arena_items: Arena::new(),
            arena_functions: Arena::new(),
            arena_bytecode: Arena::new(),
        }
    }

    pub fn add_worker<'s>(&'vm self, source_root: &str, extern_crates: Vec<ExternCrate>, scope: &'s std::thread::Scope<'s,'vm>) -> CrateId {
        let mut workers = self.workers.write().unwrap();
        let crate_id = CrateId::new(workers.len() as u32);
        let worker = RustCWorker::new(source_root,extern_crates,scope, self, crate_id);

        workers.push(worker);
        crate_id
    }

    pub fn set_crate_items(&'vm self, crate_id: CrateId, items: CrateItems<'vm>) -> &'vm CrateItems<'vm> {
        let mut workers = self.workers.write().unwrap();
        let items_ref = self.arena_items.alloc(items);
        workers[crate_id.index()].items = Some(items_ref);
        items_ref
    } 

    pub fn get_crate_items(&'vm self, crate_id: CrateId) -> &'vm CrateItems<'vm> {
        let workers = self.workers.read().unwrap();
        workers[crate_id.index()].items.expect("crate is missing items")
    }

    pub fn wait_for_setup(&self, crate_id: CrateId) {
        // structured like this to prevent deadlocking the RwLock
        let res = {
            let workers = self.workers.read().unwrap();
            let worker = &workers[crate_id.index()];
            worker.wait_for_setup()
        };
        res.wait();
    }

    pub fn build_function_ir(&self, crate_id: CrateId, item_id: ItemId) {
        let workers = self.workers.read().unwrap();
        let worker = &workers[crate_id.index()];
        worker.build_function_ir(item_id);
    }

    pub fn alloc_function(&'vm self, item: &'vm Item<'vm>, subs: Vec<Sub<'vm>>) -> &'vm Function<'vm> {
        let func = Function {
            item,
            subs,
            native: Default::default(),
            bytecode: Default::default()
        };

        let path = item.path.as_string();
        if path.starts_with("::_builtin::") {
            match path {
                "::_builtin::print_int" => func.set_native(builtin_print_int),
                "::_builtin::print_uint" => func.set_native(builtin_print_uint),
                "::_builtin::print_float" => func.set_native(builtin_print_float),
                "::_builtin::print_bool" => func.set_native(builtin_print_bool),
                "::_builtin::print_char" => func.set_native(builtin_print_char),
                _ => panic!("unknown builtin {}",path)
            }
        }

        self.arena_functions.alloc(func)
    }

    pub fn alloc_bytecode(&'vm self, bc: Vec<Instr<'vm>>) -> &Vec<Instr<'vm>> {
        self.arena_bytecode.alloc(bc)
    }

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

        // run
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
    subs: Vec<Sub<'vm>>,
    native: AtomicPtr<std::ffi::c_void>,
    bytecode: AtomicPtr<Vec<Instr<'vm>>>
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
        self.native.store(unsafe { std::mem::transmute(native) }, Ordering::Release);
    }


    pub fn set_bytecode(&self, bc: &'vm Vec<Instr>) {
        self.bytecode.store(bc as *const _ as _, Ordering::Release);
    }

    fn bytecode(&self) -> &'vm [Instr<'vm>] {
        loop {
            if let Some(bc) = self.get_bytecode() {
                return bc;
            }

            let ir = self.item.get_ir(&self.subs);
            let path = self.item.path.as_string();

            let bc = HirCompiler::compile(self.item.vm, &ir, &self.subs, path);
            let bc_ref = self.item.vm.alloc_bytecode(bc);

            self.set_bytecode(bc_ref);
        }
    }
}

impl<'vm> std::fmt::Debug for Function<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            //.field("def_id", &self.def_id)
            //.field("subs", &self.subs)
            .finish()
    }
}

unsafe fn builtin_print_int(stack: *mut u8) {
    let x: i128 = read_stack(stack,Slot::new(0));
    println!("{}",x);
}

unsafe fn builtin_print_uint(stack: *mut u8) {
    let x: u128 = read_stack(stack,Slot::new(0));
    println!("{}",x);
}

unsafe fn builtin_print_float(stack: *mut u8) {
    let x: f64 = read_stack(stack,Slot::new(0));
    println!("{}",x);
}

unsafe fn builtin_print_bool(stack: *mut u8) {
    let x: bool = read_stack(stack,Slot::new(0));
    println!("{}",x);
}

unsafe fn builtin_print_char(stack: *mut u8) {
    let x: char = read_stack(stack,Slot::new(0));
    println!("{}",x);
}
