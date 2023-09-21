mod _builtin;

fn select<T>(c: bool, a: T, b: T) -> T {
    let inner = |c,a,b| {
        if c { b } else { a }
    };
    inner(c,a,b)
}

fn select_capture<T>(c: bool, a: T, b: T) -> T {
    let inner = |c| {
        if c { b } else { a }
    };
    inner(c)
}

fn main() {
    _builtin::print_int(select::<i16>(false,10,20) as _);
    _builtin::print_int(select::<i16>(true,10,20) as _);

    _builtin::print_float(select::<f64>(false,1.1,2.2) as _);
    _builtin::print_float(select::<f64>(true,1.1,2.2) as _);

    _builtin::print_int(select_capture::<i16>(false,10,20) as _);
    _builtin::print_int(select_capture::<i16>(true,10,20) as _);

    _builtin::print_float(select_capture::<f64>(false,1.1,2.2) as _);
    _builtin::print_float(select_capture::<f64>(true,1.1,2.2) as _);
}
