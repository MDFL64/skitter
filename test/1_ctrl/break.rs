mod _builtin;

fn max() -> i128 {
    _builtin::print_int(123);
    1000
}

pub fn main() {
    let mut i = 0;
    while i < max() {
        i += 1;
        if i > 10 {
            break;
        }
        continue;
        i += 10000;
    }
    _builtin::print_int(i);
}
