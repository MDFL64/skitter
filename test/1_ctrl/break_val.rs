mod _builtin;

pub fn main() {
    let mut i = 0.0;
    let m = 'x: loop {
        _builtin::print_float(i);
        i += 0.3;
        if i > 10.0 {
            break 'x 123;
        }
    };
    _builtin::print_int(m);
}
