mod _builtin;

fn select<G,T>(c: bool, a: T, b: T) -> T {
    let inner = |c,a,b| {
        if c { b } else { a }
    };
    inner(c,a,b)
}

fn select_capture<G,T>(c: bool, a: T, b: T) -> T {
    let inner = |c| {
        if c { b } else { a }
    };
    inner(c)
}

fn main() {
    _builtin::print_int(select::<i8,i16>(false,10,20) as _);
    _builtin::print_int(select::<i8,i16>(true,10,20) as _);

    _builtin::print_float(select::<i8,f64>(false,1.1,2.2) as _);
    _builtin::print_float(select::<i8,f64>(true,1.1,2.2) as _);

    _builtin::print_int(select_capture::<i8,i16>(false,10,20) as _);
    _builtin::print_int(select_capture::<i8,i16>(true,10,20) as _);

    _builtin::print_float(select_capture::<i8,f64>(false,1.1,2.2) as _);
    _builtin::print_float(select_capture::<i8,f64>(true,1.1,2.2) as _);
}
