mod _builtin;

pub fn main() {
    let x = 10.0;
    let x = 2;
    {
        let x = 1;
        _builtin::print_int(x as _);
    }
    _builtin::print_int(x as _);
}
