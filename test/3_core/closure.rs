mod _builtin;

pub fn add_3(x: f32, y: f32, z: f32) -> f32 {
    let add = |x| {
        x + y + z
    };
    add(x)
}

pub fn add_4(a: f32, b: f64, c: i16, d: i8) -> f32 {
    struct S{
        a: f32,
        b: f64
    }

    impl S {
        fn check(&self) {}
    }

    let s = S{a,b};

    let f = || {
        //s.check();
        s.a + s.b as f32 + c as f32 + d as f32
    };
    f()
}

pub fn main() {
    let add_5 = |x| {
        let z = 0;
        let a = z + 5;
        x + a
    };

    let res = add_5(10);
    _builtin::print_int(res as _);

    //let res = add_3(10.0,5.0,1.0);
    //_builtin::print_int(res as _);

    add_4(10.101,5.52,1,3);
}
