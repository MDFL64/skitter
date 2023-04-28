mod _builtin;

fn add(a: &i32, b: &i32, c: &i32) -> i32 {
    let m: i128 = 0;
    let m: i128 = 0;
    let m: i128 = 0;
    let m: i128 = 0;
    let m: i128 = 0;
    *a + *b + *c
}

pub fn main() {
    let x = 5;
    let y = 10;
    let z = 20;
    let r = add(&(x+100),&(y+200),&(z+300));
    _builtin::print_int(r as _);
}
