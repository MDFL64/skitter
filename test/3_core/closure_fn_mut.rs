mod _builtin;

pub fn check_1() -> f32 {
    let mut x: f32 = 0.0;
    let mut y: i8 = 0;

    let mut mutate = || {
        x = 1.23;
        y += 5;
    };

    mutate();
    mutate();

    x +  y as f32
}

struct S{
    a: f32,
    b: f64
}

impl S {
    fn check(&self) {}
}

pub fn check_2a() -> f64 {
    let mut s = S{
        a: 123.0,
        b: -5.0
    };

    let mut mutate = || {
        s.a *= 10.0;
        s.b /= 2.0;
    };

    mutate();
    mutate();

    s.a as f64 + s.b
}

pub fn check_2b() -> f64 {
    let mut s = S{
        a: 123.0,
        b: -5.0
    };

    let mut mutate = || {
        s.a *= 10.0;
        s.b /= 2.0;
        s.check();
    };

    mutate();
    mutate();

    s.a as f64 + s.b
}

// loosely based on https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/ty/enum.BorrowKind.html#variant.UniqueImmBorrow
// doesn't actually cause the linked variant to be used, but it did help test deref projections
pub fn check_3() -> f32 {
    let mut z = 3;
    let x: &mut isize = &mut z;
    
    let mut mutate = || {
        *x += 5;
    };
    mutate();

    z as f32
}

pub fn main() {
    let res = check_1();
    _builtin::print_float(res as _);

    let res = check_2a();
    _builtin::print_float(res as _);

    let res = check_2b();
    _builtin::print_float(res as _);

    let res = check_3();
    _builtin::print_float(res as _);
}
