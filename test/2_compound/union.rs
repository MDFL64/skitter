mod _builtin;

union Test {
    a: f64,
    b: i64,
    c: [u8;11]
}

unsafe fn print(x: &Test) {
    _builtin::print_float(x.a as _);
    _builtin::print_int(x.b as _);
    _builtin::print_int(x.c[0] as _);
    _builtin::print_int(x.c[1] as _);
    _builtin::print_int(x.c[2] as _);
    _builtin::print_int(x.c[3] as _);
    _builtin::print_int(x.c[4] as _);
    _builtin::print_int(x.c[5] as _);
    _builtin::print_int(x.c[6] as _);
    _builtin::print_int(x.c[7] as _);
}

pub fn main() {
    let mut x = Test{a: 3.14};
    unsafe {
        print(&x);
        x.b = 1024;
        x.c[5] = 22;
        print(&x);
    }
}
