use rustc_hir::def_id::DefId;
use rustc_middle::ty::{Ty, TyCtxt};
use rustc_middle::ty::SubstsRef;

use std::sync::{OnceLock, Arc, Mutex};
use std::collections::HashMap;
use crate::cli::CliArgs;
use crate::hir_compiler::HirCompiler;
use crate::ir::IRFunctionBuilder;
use crate::items::ItemContext;
use crate::rustc_worker::RustCWorker;
use crate::types::{Type, TypeDef};
use crate::types::TypeContext;
use crate::vm::instr::Slot;

use super::instr::Instr;

pub struct VM<'vm> {
    stack: Vec<u128>,
    //functions: Mutex<HashMap<(DefId,SubstsRef<'tcx>),Arc<Function<'tcx>>>>,
    types: TypeContext<'vm>,
    pub items: ItemContext<'vm>,
    pub is_verbose: bool,
    workers: Vec<RustCWorker<'vm>>
}


impl<'vm> VM<'vm> {
    pub fn new() -> Self {
        Self {
            // 64k stack - TODO move out of this struct, this is not safe
            stack: vec!(0;4096),
            //functions: Default::default(),
            types: TypeContext::new(),
            items: ItemContext::new(),
            is_verbose: false,
            workers: Vec::new()
        }
    }

    /*pub fn add_worker(&mut self, worker: RustCWorker<'vm>) {
        self.workers.push(worker);
    }*/

    /*pub fn run_crate(&mut self, args: CliArgs) {
        assert!(self.crates.len() == 0);
        self.crates.push(Box::new(IRCrateLazy::new(args)));
        let main_id = self.crates[0].find("::main").unwrap();
        println!("{:?}",main_id);
    }*/

    /*pub fn get_func(&self, def_id: DefId, subs: SubstsRef<'tcx>) -> Arc<Function<'tcx>> {
        let mut functions = self.functions.lock().unwrap();
        functions.entry((def_id,subs)).or_insert_with(|| {
            let mut res = Function::new(def_id,subs);

            // builtin hack
            if def_id.krate == rustc_hir::def_id::LOCAL_CRATE {
                let local_id = rustc_hir::def_id::LocalDefId{ local_def_index: def_id.index };
                let path = self.tcx.hir().def_path(local_id).to_string_no_crate_verbose();
                if path.starts_with("::_builtin::") {
                    match path.as_str() {
                        "::_builtin::print_int" => res.native = Some(builtin_print_int),
                        "::_builtin::print_uint" => res.native = Some(builtin_print_uint),
                        "::_builtin::print_float" => res.native = Some(builtin_print_float),
                        "::_builtin::print_bool" => res.native = Some(builtin_print_bool),
                        "::_builtin::print_char" => res.native = Some(builtin_print_char),
                        _ => panic!("unknown builtin {}",path)
                    }
                }
            }

            Arc::new(res)
        }).clone()
    }*/

    pub fn call(&'vm self, func: &Function<'vm>, stack_offset: u32) {

        if let Some(native) = func.native {
            unsafe {
                let stack = (self.stack.as_ptr() as *mut u8).offset(stack_offset as isize);
                native(stack);
            }
            return;
        }

        // fetch bytecode
        let bc = func.bytecode(self);

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

    pub fn type_from_rustc<'tcx>(&'vm self, ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Type {
        self.types.type_from_rustc(ty,self,tcx)
    }
}

unsafe fn write_stack<T>(base: *mut u8, slot: Slot, x: T) {
    *(base.add(slot.index()) as *mut _) = x;
}

unsafe fn read_stack<T: Copy>(base: *mut u8, slot: Slot) -> T {
    *(base.add(slot.index()) as *mut _)
}

pub struct Function<'vm> {
    pub def: TypeDef<'vm>,
    pub native: Option<unsafe fn(*mut u8)>,
    /*pub def_id: DefId,
    pub subs: SubstsRef<'tcx>,
    bytecode: OnceLock<Vec<Instr<'tcx>>>*/
}

impl<'vm> Function<'vm> {
    fn new(def: TypeDef<'vm>) -> Self {
        Function {
            def,
            native: None,
            /*
            bytecode: Default::default()*/
        }
    }

    fn bytecode(&self, vm: &'vm VM<'vm>) -> &[Instr<'vm>] {
        panic!("todo bc");
        /*self.bytecode.get_or_init(|| {

            // resolve trait methods to a specific instance
            let resolve_arg = rustc_middle::ty::ParamEnv::reveal_all().and((self.def_id.into(),self.subs));
            let resolved = vm.tcx.resolve_instance(resolve_arg).unwrap().unwrap();

            let resolve_id = resolved.def_id();
            let resolve_subs = resolved.substs;

            if resolve_id.krate != rustc_hir::def_id::LOCAL_CRATE {
                panic!("non-local call {:?} {:?}",resolve_id,resolve_subs);
            }
            
            let local_id = rustc_hir::def_id::LocalDefId{ local_def_index: resolve_id.index };

            let (thir_body,root_expr) = vm.tcx.thir_body(WithOptConstParam::unknown(local_id)).expect("type check failed");
            let thir_body = thir_body.borrow();

            let ir = IRFunctionBuilder::build(vm,local_id, root_expr, &thir_body);

            HirCompiler::compile(vm,&ir,&resolve_subs)
        })*/
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
