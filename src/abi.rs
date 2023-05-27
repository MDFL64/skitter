pub const POINTER_SIZE: PointerSize = match std::mem::size_of::<usize>() {
    4 => PointerSize::I32,
    8 => PointerSize::I64,
    _ => panic!("bad pointer size"),
};

pub enum PointerSize {
    I32,
    I64,
}

impl PointerSize {
    pub const fn bytes(&self) -> u32 {
        match self {
            PointerSize::I32 => 4,
            PointerSize::I64 => 8,
        }
    }
}

const _: () = {
    if std::mem::size_of::<crate::vm::instr::Instr>() != 16 {
        panic!("bad Instr size");
    }
};

/// Utility function for aligning stacks / layouts
pub fn align(x: u32, align: u32) -> u32 {
    let mask = align - 1;
    (x + mask) & !mask
}
