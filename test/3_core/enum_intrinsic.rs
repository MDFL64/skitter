#![feature(variant_count)]

mod _builtin;

enum Empty {}
enum Grade {
    A, B, C, D, F
}

pub fn main() {
    _builtin::print_uint(std::mem::variant_count::<Option<i32>>() as _);
    _builtin::print_uint(std::mem::variant_count::<Empty>() as _);
    _builtin::print_uint(std::mem::variant_count::<Grade>() as _);

    let x = std::mem::discriminant(&Grade::A);
    let y = std::mem::discriminant(&Grade::B);
    let z = std::mem::discriminant(&Grade::A);

    _builtin::print_bool(x == y);
    _builtin::print_bool(x == z);
    _builtin::print_bool(y == z);
}
