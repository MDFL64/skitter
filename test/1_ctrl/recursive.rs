mod _builtin;

fn r(a: i32) -> i32 {
    if a <= 0 {
        0
    } else {
        r(a-1) + 5
    }
}

pub fn main() {
    _builtin::print_int(r(10) as _);
}
