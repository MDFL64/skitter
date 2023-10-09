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

fn main() {
    let x = 5;
    let x2: &dyn Greeter = &x;

    x2.greet();

    //let y = 1.1;
    //y.greet();
}
