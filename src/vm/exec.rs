use super::instr::{Instr, Slot, CallTarget};

pub fn exec_main(code: &[Instr]) {
    let mut stack: Vec<u128> = vec![0; 1024];
    let stack = stack.as_mut_ptr() as *mut u8;

    unsafe {
        exec_rust(code,stack);
    }
}

unsafe fn write_stack<T>(base: *mut u8, slot: Slot, x: T) {
    *(base.add(slot.offset()) as *mut _) = x;
}

unsafe fn read_stack<T: Copy>(base: *mut u8, slot: Slot) -> T {
    *(base.add(slot.offset()) as *mut _)
}

unsafe fn exec_rust(code: &[Instr], stack: *mut u8) {
    let mut pc = 0;

    loop {
        let instr = &code[pc]; //*code.get_unchecked(pc);// [pc];
        include!("_exec_match.txt");
        pc += 1;
    }
}
