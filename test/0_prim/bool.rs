mod _builtin;

fn main() {
    let x = true;
    let y = false;

    _builtin::print_bool(x);
    _builtin::print_bool(y);

    _builtin::print_bool(!x);
    _builtin::print_bool(!y);

    _builtin::print_bool(x | x);
    _builtin::print_bool(x | y);
    _builtin::print_bool(y | x);
    _builtin::print_bool(y | y);

    _builtin::print_bool(x & x);
    _builtin::print_bool(x & y);
    _builtin::print_bool(y & x);
    _builtin::print_bool(y & y);

    _builtin::print_bool(x ^ x);
    _builtin::print_bool(x ^ y);
    _builtin::print_bool(y ^ x);
    _builtin::print_bool(y ^ y);

    _builtin::print_bool(x == x);
    _builtin::print_bool(x == y);
    _builtin::print_bool(y == x);
    _builtin::print_bool(y == y);

    _builtin::print_bool(x != x);
    _builtin::print_bool(x != y);
    _builtin::print_bool(y != x);
    _builtin::print_bool(y != y);

    _builtin::print_int(x as _);
    _builtin::print_int(y as _);
}
