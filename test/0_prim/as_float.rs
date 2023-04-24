mod _builtin;

fn main() {

    // f32 to f64
    {
        let a: f32 = 1042.5213;
        let b: f64 = 1042.5213;
        let inf: f32 = 1.0/0.0;
        let nan: f32 = 0.0/0.0;

        _builtin::print_float(a as f64);
        _builtin::print_float(b);
        _builtin::print_float(inf as f64);
        _builtin::print_float(nan as f64);
    }

    // f64 to f32
    {
        let a: f64 = 16777216.0;
        let b: f64 = 16777217.0;
        let c: f64 = 16777218.0;

        _builtin::print_float(a as f32 as f64);
        _builtin::print_float(b as f32 as f64);
        _builtin::print_float(c as f32 as f64);

        _builtin::print_float(-a as f32 as f64);
        _builtin::print_float(-b as f32 as f64);
        _builtin::print_float(-c as f32 as f64);

        let inf: f64 = 1.0/0.0;
        let nan: f64 = 0.0/0.0;
        _builtin::print_float(inf as f32 as f64);
        _builtin::print_float(nan as f32 as f64);

        let overflows: f64 = 1e100;
        _builtin::print_float(overflows);
        _builtin::print_float(overflows as f32 as f64);
    }

    // int -> float
    {
        let x = 100;
        _builtin::print_float(x as f32 as f64);
        _builtin::print_float(x as f64);
        _builtin::print_float(-x as f32 as f64);
        _builtin::print_float(-x as f64);
        _builtin::print_float(-x as u8 as f32 as f64);
        _builtin::print_float(-x as u64 as f64);

        let a = 16777216;
        let b = 16777217;
        let c = 16777218;
    
        _builtin::print_float(a as f32 as f64);
        _builtin::print_float(b as f32 as f64);
        _builtin::print_float(c as f32 as f64);

        _builtin::print_float(-a as f32 as f64);
        _builtin::print_float(-b as f32 as f64);
        _builtin::print_float(-c as f32 as f64);

        let huge: u128 = 340282366920938463463374607431768211455;
        _builtin::print_float(huge as f32 as f64);
    }

    // float -> int
    {
        let z = 0.0;
        let a = 5.5;
        let b = -5.5;
        let c = 5.7;
        let d = -5.7;
        let e = 5_000_000_000.0;
        let f = -5_000_000_000.0;
        let inf = 1.0/0.0;
        let nan = 0.0/0.0;

        _builtin::print_int(z as i32 as _);
        _builtin::print_int(a as i32 as _);
        _builtin::print_int(b as i32 as _);
        _builtin::print_int(c as i32 as _);
        _builtin::print_int(d as i32 as _);
        _builtin::print_int(e as i32 as _);
        _builtin::print_int(f as i32 as _);
        _builtin::print_int(inf as i32 as _);
        _builtin::print_int(-inf as i32 as _);
        _builtin::print_int(nan as i32 as _);

        _builtin::print_int(z as u32 as _);
        _builtin::print_int(a as u32 as _);
        _builtin::print_int(b as u32 as _);
        _builtin::print_int(c as u32 as _);
        _builtin::print_int(d as u32 as _);
        _builtin::print_int(e as u32 as _);
        _builtin::print_int(f as u32 as _);
        _builtin::print_int(inf as u32 as _);
        _builtin::print_int(-inf as u32 as _);
        _builtin::print_int(nan as u32 as _);

        _builtin::print_int(z as i8 as _);
        _builtin::print_int(a as i8 as _);
        _builtin::print_int(b as i8 as _);
        _builtin::print_int(c as i8 as _);
        _builtin::print_int(d as i8 as _);
        _builtin::print_int(e as i8 as _);
        _builtin::print_int(f as i8 as _);
        _builtin::print_int(inf as i8 as _);
        _builtin::print_int(-inf as i8 as _);
        _builtin::print_int(nan as i8 as _);

        _builtin::print_int(z as u8 as _);
        _builtin::print_int(a as u8 as _);
        _builtin::print_int(b as u8 as _);
        _builtin::print_int(c as u8 as _);
        _builtin::print_int(d as u8 as _);
        _builtin::print_int(e as u8 as _);
        _builtin::print_int(f as u8 as _);
        _builtin::print_int(inf as u8 as _);
        _builtin::print_int(-inf as u8 as _);
        _builtin::print_int(nan as u8 as _);
    }
}
