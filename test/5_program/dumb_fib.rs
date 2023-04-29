mod _builtin;

pub fn main() {
    _builtin::print_uint(fib(32));
}


fn fib(n: u128) -> u128 {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}
