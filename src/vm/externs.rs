use crate::{items::{Item, FunctionAbi}, vm::{VM, instr::Slot}, abi::POINTER_SIZE};
use super::{read_stack, write_stack};

pub fn get_extern_fn(item: &Item) -> Option<for<'vm> unsafe fn(*mut u8, &'vm VM<'vm>)> {
    let path = item.path.as_string();
    if path.starts_with("::_builtin::") {
        // hack for skitter builtins, should be removed at some point in the future
        Some(match path {
            "::_builtin::print_int" => builtin_print_int,
            "::_builtin::print_uint" => builtin_print_uint,
            "::_builtin::print_float" => builtin_print_float,
            "::_builtin::print_bool" => builtin_print_bool,
            "::_builtin::print_char" => builtin_print_char,
            "::_builtin::print_raw" => builtin_print_raw,
            _ => panic!("unknown builtin {}", path),
        })
    } else {
        if let Some(item_extern) = item.get_extern() {
            Some(match (item_extern.0, item_extern.1.as_str()) {
                (FunctionAbi::Rust, "__rust_alloc") => builtin_alloc_zeroed,
                (FunctionAbi::Rust, "__rust_alloc_zeroed") => builtin_alloc_zeroed,
                (FunctionAbi::Rust, "panic_impl") => builtin_panic,
                _ => panic!("todo extern? {:?}", item_extern),
            })
        } else {
            None
        }
    }
}

pub fn get_extern_static(item: &Item) -> Option<*mut u8> {
    if let Some(item_extern) = item.get_extern() {
        Some(match (item_extern.0, item_extern.1.as_str()) {
            (FunctionAbi::Rust, "__rust_no_alloc_shim_is_unstable") => (&BUILTIN_ALLOC_DUMMY) as *const _ as *mut _,
            _ => panic!("todo extern? {:?}", item_extern),
        })
    } else {
        None
    }
}

unsafe fn builtin_print_int(stack: *mut u8, _vm: &VM) {
    let x: i128 = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_uint<'vm>(stack: *mut u8, _vm: &'vm VM<'vm>) {
    let x: u128 = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_float<'vm>(stack: *mut u8, _vm: &'vm VM<'vm>) {
    let x: f64 = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_bool<'vm>(stack: *mut u8, _vm: &'vm VM<'vm>) {
    let x: bool = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_char<'vm>(stack: *mut u8, _vm: &'vm VM<'vm>) {
    let x: char = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe fn builtin_print_raw<'vm>(stack: *mut u8, _vm: &'vm VM<'vm>) {
    let x: &str = read_stack(stack, Slot::new(0));
    print!("{}", x);
}

static BUILTIN_ALLOC_DUMMY: u8 = 0;

unsafe fn builtin_alloc<'vm>(stack: *mut u8, vm: &'vm VM<'vm>) {
    let ptr_size = POINTER_SIZE.bytes();
    let size: usize = read_stack(stack, Slot::new(ptr_size));
    let align: usize = read_stack(stack, Slot::new(ptr_size * 2));

    let res = vm.alloc_bytes(size, align);

    write_stack(stack, Slot::new(0), res);
}

unsafe fn builtin_alloc_zeroed<'vm>(stack: *mut u8, vm: &'vm VM<'vm>) {
    let ptr_size = POINTER_SIZE.bytes();
    let size: usize = read_stack(stack, Slot::new(ptr_size));
    let align: usize = read_stack(stack, Slot::new(ptr_size * 2));

    let res = vm.alloc_bytes_zeroed(size, align);

    write_stack(stack, Slot::new(0), res);
}

unsafe fn builtin_panic<'vm>(_stack: *mut u8, _vm: &'vm VM<'vm>) {
    panic!("skitter panic?");
}
