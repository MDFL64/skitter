mod _builtin;

pub fn main() {
    let a = -1234.56f32;

    let b = a.abs();

    _builtin::print_float(b as _);

    _builtin::print_float(a.signum() as _);
    _builtin::print_float(b.signum() as _);

    _builtin::print_float(a.floor() as _);
    _builtin::print_float(b.floor() as _);

    _builtin::print_float(a.ceil() as _);
    _builtin::print_float(b.ceil() as _);

    _builtin::print_float(a.round() as _);
    _builtin::print_float(b.round() as _);

    _builtin::print_float(a.trunc() as _);
    _builtin::print_float(b.trunc() as _);

    _builtin::print_float(a.fract() as _);
    _builtin::print_float(b.fract() as _);

    _builtin::print_float(a.powi(3) as _);
    _builtin::print_float(b.powf(0.8) as _);
    _builtin::print_float(b.sqrt() as _);

    _builtin::print_float((20f32).exp() as _);
    _builtin::print_float((20f32).exp2() as _);

    _builtin::print_float(b.log(5.0) as _);
    _builtin::print_float(b.log2() as _);
    _builtin::print_float(b.log10() as _);
}
