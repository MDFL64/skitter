mod _builtin;

fn add(x: i32, y: i32) -> i32 {
    x + y
}

fn sub(x: i32, y: i32) -> i32 {
    x - y
}

fn mul(x: i32, y: i32) -> i32 {
    x * y
}

fn div(x: i32, y: i32) -> i32 {
    x / y
}

fn get_op(n: i32) -> fn(i32, i32) -> i32 {
    match n {
        0 => add,
        1 => sub,
        2 => mul,
        _ => div
    }
}

fn main() {
    _builtin::print_int(get_op(0)(1,2) as _);
    _builtin::print_int(get_op(1)(10,5) as _);
    _builtin::print_int(get_op(2)(101,3) as _);
    _builtin::print_int(get_op(10)(999,4) as _);
}
