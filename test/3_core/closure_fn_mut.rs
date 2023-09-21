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

// loosely based on https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/ty/enum.BorrowKind.html#variant.UniqueImmBorrow
// didn't end up testing the thing I wanted, but it did reveal another issue
pub fn check_3() -> f32 {
    let mut z = 3;
    let mut x: &mut isize = &mut z;

    let mut a = 1;

    let mutate_1 = || {
        *x += 5;
    };
    mutate_1();

    let mutate_2 = || {
        *x += 5;
        x = &mut a;
        *x += 5;
    };
    mutate_2();

    z as f32 + a as f32
}

pub fn main() {
    let res = check_1();
    _builtin::print_float(res as _);

    let res = check_2();
    _builtin::print_float(res as _);

    let res = check_3();
    _builtin::print_float(res as _);
}
