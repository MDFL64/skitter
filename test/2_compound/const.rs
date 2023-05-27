mod _builtin;

const PI: f64 = 3.141592653589793238;
const TAU: f64 = PI * 2.0;

pub fn main() {
    _builtin::print_float(TAU);
    _builtin::print_float(PI);
}
