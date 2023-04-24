mod _builtin;

pub fn main() {
    let mut a: i32 = 4;
    a = a;
    a = a + a;
    a = a << a;
    a = 2;
    a += a;
    a <<= a;
    _builtin::print_int(a as _);
}
