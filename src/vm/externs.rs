use super::{
    read_stack,
    vm::{NativeFunc, VMThread},
    write_stack,
};
use crate::{
    abi::POINTER_SIZE,
    items::{FunctionAbi, Item},
    vm::{instr::Slot, VM},
};

pub fn get_extern_fn<'vm>(item: &Item<'vm>) -> Option<NativeFunc> {
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
                (FunctionAbi::Rust, "__rust_realloc") => builtin_realloc,
                (FunctionAbi::Rust, "__rust_dealloc") => builtin_free,

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
            (FunctionAbi::Rust, "__rust_no_alloc_shim_is_unstable") => {
                (&BUILTIN_ALLOC_DUMMY) as *const _ as *mut _
            }
            _ => panic!("todo extern? {:?}", item_extern),
        })
    } else {
        None
    }
}

unsafe extern "C" fn builtin_print_int<'vm>(stack: *mut u8, _thread: &VMThread<'vm>) {
    let x: i128 = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe extern "C" fn builtin_print_uint<'vm>(stack: *mut u8, _thread: &VMThread<'vm>) {
    let x: u128 = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe extern "C" fn builtin_print_float<'vm>(stack: *mut u8, _thread: &VMThread) {
    let x: f64 = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe extern "C" fn builtin_print_bool<'vm>(stack: *mut u8, _thread: &VMThread) {
    let x: bool = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe extern "C" fn builtin_print_char<'vm>(stack: *mut u8, _thread: &VMThread) {
    let x: char = read_stack(stack, Slot::new(0));
    println!("{}", x);
}

unsafe extern "C" fn builtin_print_raw<'vm>(stack: *mut u8, _thread: &VMThread) {
    let x: &str = read_stack(stack, Slot::new(0));
    print!("{}", x);
}

static BUILTIN_ALLOC_DUMMY: u8 = 0;

unsafe extern "C" fn builtin_alloc<'vm>(stack: *mut u8, thread: &VMThread) {
    let ptr_size = POINTER_SIZE.bytes();
    let size: usize = read_stack(stack, Slot::new(ptr_size));
    let align: usize = read_stack(stack, Slot::new(ptr_size * 2));

    let res = thread.vm.alloc_bytes(size, align);

    write_stack(stack, Slot::new(0), res);
}

unsafe extern "C" fn builtin_alloc_zeroed<'vm>(stack: *mut u8, thread: &VMThread) {
    let ptr_size = POINTER_SIZE.bytes();
    let size: usize = read_stack(stack, Slot::new(ptr_size));
    let align: usize = read_stack(stack, Slot::new(ptr_size * 2));

    let res = thread.vm.alloc_bytes_zeroed(size, align);

    write_stack(stack, Slot::new(0), res);
}

unsafe extern "C" fn builtin_realloc<'vm>(stack: *mut u8, thread: &VMThread) {
    let ptr_size = POINTER_SIZE.bytes();

    let ptr: *mut u8 = read_stack(stack, Slot::new(ptr_size));
    let old_size: usize = read_stack(stack, Slot::new(ptr_size * 2));
    let align: usize = read_stack(stack, Slot::new(ptr_size * 3));
    let new_size: usize = read_stack(stack, Slot::new(ptr_size * 4));

    let res = thread.vm.realloc_bytes(ptr, old_size, align, new_size);

    write_stack(stack, Slot::new(0), res);
}

unsafe extern "C" fn builtin_free<'vm>(stack: *mut u8, thread: &VMThread) {
    let ptr_size = POINTER_SIZE.bytes();

    let ptr: *mut u8 = read_stack(stack, Slot::new(0));
    let size: usize = read_stack(stack, Slot::new(ptr_size));
    let align: usize = read_stack(stack, Slot::new(ptr_size * 2));

    thread.vm.free_bytes(ptr, size, align);
}

unsafe extern "C" fn builtin_panic<'vm>(_stack: *mut u8, _thread: &VMThread) {
    panic!("skitter panic?");
}
