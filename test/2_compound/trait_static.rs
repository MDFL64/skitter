mod _builtin;

trait Greeter {
    fn greet(&self);
}

impl Greeter for i32 {
    fn greet(&self) {
        _builtin::print_int((*self+1000) as _);
    }
}

impl Greeter for f32 {
    fn greet(&self) {
        _builtin::print_float((*self+0.5) as _);
    }
}

trait Favorite {
    fn get() -> Self;
}

impl Favorite for (i32,i32,i32) {
    fn get() -> Self {
        (1,2,3)
    }
}

fn main() {
    let x = 5;
    x.greet();

    let y = 1.1;
    y.greet();

    let f: (_,_,_) = Favorite::get();
    _builtin::print_int(f.0 as _);
    _builtin::print_int(f.1 as _);
    _builtin::print_int(f.2 as _);
}
