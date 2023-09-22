mod _builtin;

pub fn add_3(x: f32, y: f32, z: f32) -> f32 {
    let add = |x| {
        x + y + z
    };
    add(x)
}

struct S{
    a: f32,
    b: f64
}

impl S {
    fn check(&self) {}
}

pub fn add_4a(a: f32, b: f64, c: i16, d: i8) -> f32 {
    let s = S{a,b};

    let f = || {
        s.a + s.b as f32 + c as f32 + d as f32
    };
    f()
}

pub fn add_4b(a: f32, b: f64, c: i16, d: i8) -> f32 {
    let s = S{a,b};

    let f = || {
        s.check();
        s.a + s.b as f32 + c as f32 + d as f32
    };
    f()
}

pub fn add_move(a: f32, b: f32, c: f32, d: f32, e: f32) -> f32 {
    let f = move || {
        a + b + c + d + e
    };
    f()
}

pub fn nested() {
    let z = 100;

    let curry_yum = |x| {
        move |y| {
            x + y + z
        }
    };

    let add_105 = curry_yum(5);

    let a = add_105(-20);
    let b = add_105(20);

    _builtin::print_int(a as _);
    _builtin::print_int(b as _);
}

pub fn main() {
    let add_5 = |x| {
        let z = 0;
        let a = z + 5;
        x + a
    };

    let res = add_5(10);
    _builtin::print_int(res as _);

    let res = add_3(10.5,5.0,0.9);
    _builtin::print_float(res as _);

    let res = add_4a(10.101,5.52,1,3);
    _builtin::print_float(res as _);

    let res = add_4b(10.101,5.52,1,3);
    _builtin::print_float(res as _);

    let res = add_move(-80.0,53.0,1001.0,2.999,23.0);
    _builtin::print_float(res as _);

    nested();
}
