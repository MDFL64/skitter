mod _builtin;

pub fn main() {
    let a = 10i32;
    let b = &mut (a as i32);

    *b = 5;
    _builtin::print_int(*b as _);
    _builtin::print_int(a as _);
}
