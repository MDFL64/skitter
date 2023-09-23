mod _builtin;

fn call_a(f: impl Fn(i32)->i32) {
    let res = f(5);
    _builtin::print_int(res as _);
}

fn call_b(mut f: impl FnMut(i32)->i32) {
    let res = f(6);
    _builtin::print_int(res as _);
}

fn call_c(f: impl FnOnce(i32)->i32) {
    let res = f(7);
    _builtin::print_int(res as _);
}

fn function(x: i32) -> i32 {
    x * 100
}

pub fn main() {
    let ptr: fn(i32)->i32 = function;
    call_a(ptr);
    call_b(ptr);
    call_c(ptr);
}
