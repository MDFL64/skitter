// Only a limited number of float ops are defined in core. The rest (in std) should have another test case. There are many intrinsics.

mod _builtin;

use std::num::FpCategory;

fn print_cat(x: FpCategory) {
    let n = match x {
        FpCategory::Nan => -1.0,
        FpCategory::Infinite => 100.0,
        FpCategory::Zero => 0.0,
        FpCategory::Subnormal => 0.001,
        FpCategory::Normal => 1.0,
    };
    _builtin::print_float(n);
}

pub fn main() {
    let a = -1234.56f32;
    let b = -0.0f32;
    let c = 1.0/0.0f32;
    let d = 0.0/0.0f32;
    let e = 1e-40f32;

    print_cat(a.classify());
    print_cat(b.classify());
    print_cat(c.classify());
    print_cat(d.classify());
    print_cat(e.classify());

    _builtin::print_bool(a.is_sign_positive());
    _builtin::print_bool(b.is_sign_positive());

    _builtin::print_bool(c.is_sign_negative());
    _builtin::print_bool(e.is_sign_negative());

    _builtin::print_float((90f32).to_radians() as _);

    _builtin::print_float(f32::from_bits(1099999999) as _);

    _builtin::print_float(a.min(b) as _);
    _builtin::print_float(c.max(d) as _);

    _builtin::print_float((a as f64).min(b as f64));
    _builtin::print_float((c as f64).max(d as f64));
}
