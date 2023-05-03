use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::WithOptConstParam;
use rustc_middle::ty::SubstsRef;

use std::sync::{OnceLock, Arc, Mutex};
use std::collections::HashMap;
use crate::hir_compiler::HirCompiler;
use crate::ir::IRFunctionBuilder;
use crate::types::TypeContext;
use crate::vm::instr::Slot;

use super::instr::Instr;

pub struct VM<'vm,'tcx> {
    pub tcx: TyCtxt<'tcx>,
    stack: Vec<u128>,
    functions: Mutex<HashMap<(DefId,SubstsRef<'tcx>),Arc<Function<'tcx>>>>,
    pub types: TypeContext<'vm>,
    pub is_verbose: bool
}


impl<'vm,'tcx> VM<'vm,'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            // 64k stack - TODO move out of this struct, this is not safe
            stack: vec!(0;4096),
            functions: Default::default(),
            types: Default::default(),
            is_verbose: false
        }
    }

    pub fn get_func(&self, def_id: DefId, subs: SubstsRef<'tcx>) -> Arc<Function<'tcx>> {
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
    }

    pub fn call(&'vm self, func: &Function<'tcx>, stack_offset: u32) {

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
}

unsafe fn write_stack<T>(base: *mut u8, slot: Slot, x: T) {
    *(base.add(slot.index()) as *mut _) = x;
}

unsafe fn read_stack<T: Copy>(base: *mut u8, slot: Slot) -> T {
    *(base.add(slot.index()) as *mut _)
}

pub struct Function<'tcx> {
    pub def_id: DefId,
    pub subs: SubstsRef<'tcx>,
    pub native: Option<unsafe fn(*mut u8)>,
    bytecode: OnceLock<Vec<Instr<'tcx>>>
}

impl<'tcx> Function<'tcx> {
    fn new(def_id: DefId, subs: SubstsRef<'tcx>) -> Self {
        Function {
            def_id,
            subs,
            native: None,
            bytecode: Default::default()
        }
    }

    fn bytecode<'vm>(&self, vm: &'vm VM<'vm,'tcx>) -> &[Instr<'tcx>] {
        self.bytecode.get_or_init(|| {

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
        })
    }
}

impl<'tcx> std::fmt::Debug for Function<'tcx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("def_id", &self.def_id)
            .field("subs", &self.subs)
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
