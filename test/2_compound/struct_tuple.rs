mod _builtin;

struct Vec2(f32,f32);

fn check(n: f32) -> f32 {
    _builtin::print_float(n as _);
    n
}

fn print_vec2(v: &Vec2) {
    _builtin::print_float(v.0 as _);
    _builtin::print_float(v.1 as _);
}

pub fn main() {
    let v1 = Vec2{0: check(1.0), 1: check(2.0)};
    
    let mut v2 = Vec2{1: check(3.0), 0: check(4.0)};

    let v3 = Vec2(5.0,6.0);

    print_vec2(&v1);
    print_vec2(&v2);
    print_vec2(&v3);
}
