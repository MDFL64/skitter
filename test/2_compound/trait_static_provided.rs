mod _builtin;

trait Greeter {
    fn turbo_greet(&self) {
        self.greet();
        self.greet();
        self.greet();
        self.greet();
        self.greet();
    }

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

    fn turbo_greet(&self) {
        _builtin::print_float(123.456);
    }
}

fn main() {
    let x = 5;
    x.turbo_greet();

    let y = 1.1;
    y.turbo_greet();
}
