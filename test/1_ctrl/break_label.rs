mod _builtin;

pub fn main() {
    let mut i = 0;
    'outer:
    while true {
        'inner:
        while i < 1000 {
            i += 1;
            if i > 100 {
                break 'outer;
            }
            continue 'inner;
            i += 10000;
        }
        _builtin::print_int(9999999);
        break 'outer;
    }
    _builtin::print_int(i);
}
