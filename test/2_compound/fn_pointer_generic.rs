mod _builtin;

fn get_select<T>() -> fn(bool, T, T) -> T {
    fn select<T>(c: bool, a: T, b: T) -> T {
        if c { b } else { a }
    }

    select
}

fn main() {
    _builtin::print_int(get_select::<i16>()(false,10,20) as _);
    _builtin::print_int(get_select::<i16>()(true,10,20) as _);

    _builtin::print_float(get_select::<f64>()(false,1.1,2.2) as _);
    _builtin::print_float(get_select::<f64>()(true,1.1,2.2) as _);
}
