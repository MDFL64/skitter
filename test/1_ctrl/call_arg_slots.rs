mod _builtin;

fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn main() {
    let x = add((1,2,3).1,(3,4,5).2);
    _builtin::print_int(x as _);
}
