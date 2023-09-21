mod _builtin;

pub fn check_1() -> f32 {
    let x: f32 = 0.0;
    let y: i8 = 0;

    let mutate = || {
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

pub fn check_2() -> f64 {
    let s = S{
        a: 123.0,
        b: -5.0
    };

    let mutate = || {
        s.a *= 10.0;
        s.b /= 2.0;
    };

    mutate();
    mutate();

    s.a as f64 + s.b
}

pub fn check_3() -> f32 {
    // copied from https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/ty/enum.BorrowKind.html#variant.UniqueImmBorrow
    let mut z = 3;
    let x: &mut isize = &mut z;
    let y = || *x += 5;
    y();

    z as f32
}

pub fn main() {
    let res = check_1();
    _builtin::print_float(res as _);

    let res = check_2();
    _builtin::print_float(res as _);

    let res = check_3();
    _builtin::print_float(res as _);
}
