mod _builtin;

fn main() {
    {
        let x = '😁';
        let a = x as u64;
        let b = x as i8;
        _builtin::print_int(a as _);
        _builtin::print_int(b as _);
    }

    {
        let x = 0xD7u8;
        _builtin::print_char(x as char);
    }

    {
        let a = 'a';
        let b = 'b';
        _builtin::print_bool(a == a);
        _builtin::print_bool(a == b);

        _builtin::print_bool(a > a);
        _builtin::print_bool(a > b);

        _builtin::print_bool(a < a);
        _builtin::print_bool(a < b);

        _builtin::print_bool(a >= a);
        _builtin::print_bool(a >= b);

        _builtin::print_bool(a <= a);
        _builtin::print_bool(a <= b);
    }
}
