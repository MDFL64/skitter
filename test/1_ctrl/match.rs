mod _builtin;

pub fn check(n: i32) {
    match n {
        1 | 3 | 5 | 7 | 9 => {
            _builtin::print_float(1.0);
        }
        2 | 4 | 6 | 8 | 10 => {
            _builtin::print_float(2.0);
        }
        _ => {
            _builtin::print_float(3.0);
        }
    }
}

pub fn main() {
    check(0);
    check(1);
    check(2);
    check(3);
    check(4);
    check(5);
    check(6);
    check(7);
    check(8);
    check(9);
    check(10);
    check(11);
}
