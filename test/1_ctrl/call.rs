mod _builtin;

fn add(a: i32, b: i32) -> i32 {
    a + b + 100
}

pub fn main() {
    let x = add(10,20);
    _builtin::print_int(x as _);
    _builtin::print_int(add(add(1,2),add(3,4)) as _);
}
