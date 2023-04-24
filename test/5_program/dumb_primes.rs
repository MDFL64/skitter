mod _builtin;

fn is_prime(x: i32) -> bool {
    let mut i = 2;
    while i < x {
        if x % i == 0 {
            return false;
        }
        i += 1;
    }
    return true;
}

pub fn main() {
    let mut i = 0;
    while i < 10000 {
        i += 1;
        if is_prime(i) {
            _builtin::print_int(i as _);
        }
    }
}
