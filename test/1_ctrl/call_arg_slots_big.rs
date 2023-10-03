mod _builtin;

fn add(a: i32, b: i32, c: i32) -> i32 {
    let data = [255u8;100];
    a + b + c
}

pub fn main() {
    let r = add(add(1,2,3),add(4,5,6),add(7,8,9));
    _builtin::print_int(r as _);
}
