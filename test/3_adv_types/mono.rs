mod _builtin;

fn pick<T>(s: bool, x: T, y: T) -> T {
    if s {
        x
    } else {
        y
    }
}

fn main() {

    let x = pick(true,5,2);
    let y = pick(false,6.5f32,3.14);

    _builtin::print_int(x as _);
    _builtin::print_float(y as _);
}
