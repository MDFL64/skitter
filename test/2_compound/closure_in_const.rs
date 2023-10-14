#![feature(inline_const)]

mod _builtin;

static A: fn()->i32 = || 521;

const B: fn()->i32 = || 372;

fn get_c() -> fn()->i32 {
    const {
        || 915
    }
}

fn main() {
    _builtin::print_int(A() as _);
    _builtin::print_int(B() as _);
    _builtin::print_int(get_c()() as _);
}
