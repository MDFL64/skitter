#![feature(repr128)]

mod _builtin;

#[repr(u8)]
enum ETiny {
    A, B, C
}

#[repr(i128)]
enum EHuge {
    A, B, C
}

fn main() {
    _builtin::print_int(std::mem::size_of::<ETiny>() as _);
    _builtin::print_int(std::mem::size_of::<EHuge>() as _);
}
