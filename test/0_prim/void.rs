mod _builtin;

fn f(x: (), y: (), z:(), n: f64, m: i64) -> () {
    _builtin::print_float(n);
    _builtin::print_int(m as _);
}

fn main() {
    let res: () = if true {
        f((),(),(),100.0,200);
    };
}
