mod _builtin;

pub fn main() {
    _builtin::print_int(101);
    _builtin::print_int(-202);
    _builtin::print_uint(303);

    _builtin::print_bool(true);
    _builtin::print_bool(false);

    _builtin::print_float(1.256789);
}
