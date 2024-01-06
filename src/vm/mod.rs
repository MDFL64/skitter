mod externs;
pub mod instr;
mod vm;

pub use vm::{Function, FunctionSource, NativeFunc, VMThread, VM};

use self::instr::Slot;

unsafe fn write_stack<T>(base: *mut u8, slot: Slot, x: T) {
    *(base.add(slot.index()) as *mut _) = x;
}

unsafe fn read_stack<T: Copy>(base: *mut u8, slot: Slot) -> T {
    *(base.add(slot.index()) as *mut _)
}
