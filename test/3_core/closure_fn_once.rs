mod _builtin;

struct S{
    a: f32,
    b: f64
}

impl S {
    fn consume(self) {}
}

pub fn check_1() -> f64 {
    let s = S{
        a: 5.0,
        b: 3.3
    };

    let f = || {
        let res = s.a as f64 + s.b;
        s.consume();
        res
    };

    f()
}

pub fn check_2() -> f64 {
    let s = S{
        a: 5.0,
        b: 3.3
    };

    let x = 666.0;

    let f = || {
        let res = s.a as f64 + s.b + x;
        s.consume();
        res
    };

    f()
}

pub fn main() {
    _builtin::print_float(check_1());
    _builtin::print_float(check_2());
}
