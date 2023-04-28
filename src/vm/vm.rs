use rustc_hir::def_id::LocalDefId;
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::WithOptConstParam;

use std::sync::{OnceLock, Arc, Mutex};
use std::collections::HashMap;
use crate::hir_compiler::HirCompiler;
use crate::vm::instr::Slot;

use super::instr::Instr;

pub struct VM<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    stack: Vec<u128>,
    functions: Mutex<HashMap<LocalDefId,Arc<Function>>>,
    pub is_verbose: bool
}

impl<'tcx> VM<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            // 64k stack - TODO move out of this struct, this is not safe
            stack: vec!(0;4096),
            functions: Default::default(),
            is_verbose: false
        }
    }

    pub fn get_func(&self, def_id: LocalDefId) -> Arc<Function> {
        let mut functions = self.functions.lock().unwrap();
        functions.entry(def_id).or_insert_with(|| {
            let mut res = Function::new(def_id);

            let path = self.tcx.hir().def_path(def_id).to_string_no_crate_verbose();
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

            Arc::new(res)
        }).clone()
    }

    pub fn call(&self, func: &Function, stack_offset: u32) {

        if let Some(native) = func.native {
            unsafe {
                let stack = (self.stack.as_ptr() as *mut u8).offset(stack_offset as isize);
                native(stack);
            }
            return;
        }

        // fetch bytecode
        let bc = func.bytecode.get_or_init(|| {
            //let mir = self.tcx.mir_built(WithOptConstParam::unknown(func.def_id)).borrow();
            //MirCompiler::compile(self, &mir)
            let (thir_body,root_expr) = self.tcx.thir_body(WithOptConstParam::unknown(func.def_id)).expect("type check failed");
            let thir_body = thir_body.borrow();
            HirCompiler::compile(self,func.def_id,&thir_body,root_expr)
        });

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

pub struct Function {
    pub def_id: LocalDefId,
    pub native: Option<unsafe fn(*mut u8)>,
    pub bytecode: OnceLock<Vec<Instr>>
}

impl Function {
    fn new(def_id: LocalDefId) -> Self {
        Function {
            def_id,
            native: None,
            bytecode: Default::default()
        }
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function").field("def_id", &self.def_id).finish()
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
