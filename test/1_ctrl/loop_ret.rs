mod _builtin;

fn test() -> i32 {
    let mut x = 12;
    loop {
        x += 5;
        if x > 100 {
            return x;
        }
    }
}

pub fn main() {
    _builtin::print_int(test() as _);
}
