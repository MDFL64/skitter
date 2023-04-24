mod _builtin;

struct Vec2{
    x: f32,
    y: f32
}

fn check(n: f32) -> f32 {
    _builtin::print_float(n as _);
    n
}

fn print_vec2(v: &Vec2) {
    _builtin::print_float(v.x as _);
    _builtin::print_float(v.y as _);
}

pub fn main() {
    let v1 = Vec2{x: check(1.0), y: check(2.0)};
    print_vec2(&v1);

    let mut v2 = Vec2{y: check(3.0), x: check(4.0)};
    _builtin::print_float(v2.x as _);
    _builtin::print_float(v2.y as _);

    let v3 = v2;
    _builtin::print_float((&v3).x as _);
    _builtin::print_float((&&&v3).y as _);
}
