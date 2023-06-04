mod _builtin;

pub fn pick_1(x @ ref y: i32) {
    _builtin::print_int(x as _);
    _builtin::print_int(*y as _);
}

pub fn pick_2(ref x @ y: i32) {
    _builtin::print_int(*x as _);
    _builtin::print_int(y as _);
}

pub fn main() {
    pick_1(55);
    pick_2(71);
}
