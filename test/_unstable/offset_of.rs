// adapted from the example here
// https://rust-lang.github.io/rfcs/3308-offset_of.html

// not going to bother supporting this until it's stabilized

#![feature(offset_of)]

mod _builtin;

use core::mem::offset_of;

const EXAMPLES: &[usize] = &[
    offset_of!(Struct, a),
    offset_of!(Struct, b),
    offset_of!(TupleStruct, 0),
    offset_of!(TupleStruct, 1),
    offset_of!(Union, x),
    offset_of!(Union, y),
    offset_of!(inner::SubModAndGeneric<u8>, f1),
    offset_of!(inner::SubModAndGeneric<u8>, f2),
    offset_of!(inner::SubModAndGeneric<()>, f1),
    offset_of!(inner::SubModAndGeneric<()>, f2),
    // do not print
    offset_of!((i32, u32), 0),
    offset_of!((i32, u32), 1),
];

#[repr(C)]
struct Struct { a: u64, b: &'static str }
#[repr(C)]
struct TupleStruct(u8, i32);
#[repr(C)]
union Union { x: u8, y: u64 }

mod inner {
    #[repr(C)]
    pub struct SubModAndGeneric<T> {
        pub f1: T,
        pub f2: u16,
    }
}

fn main() {
    _builtin::print_int(EXAMPLES[0] as _);
    _builtin::print_int(EXAMPLES[1] as _);
    _builtin::print_int(EXAMPLES[2] as _);
    _builtin::print_int(EXAMPLES[3] as _);
    _builtin::print_int(EXAMPLES[4] as _);
    _builtin::print_int(EXAMPLES[5] as _);
    _builtin::print_int(EXAMPLES[6] as _);
    _builtin::print_int(EXAMPLES[7] as _);
    _builtin::print_int(EXAMPLES[8] as _);
    _builtin::print_int(EXAMPLES[9] as _);
}
