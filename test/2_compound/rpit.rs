mod _builtin;

// trait 1 (simple)

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

// trait 2 (with generics)

trait Holder<T> {
    fn get(self) -> T;
}

struct RealHolder<T> {
    x: T
}

impl<T> Holder<T> for RealHolder<T> {
    fn get(self) -> T {
        self.x
    }
}

fn get_greeters() -> (impl Greeter,impl Greeter) {
    (10i32,5.0f32)
}

fn get_holder<A,B,C>(a: A, b: B, c: C) -> impl Holder<C> {
    RealHolder{x: c}
}

// trait 3 (with associated type)

trait Calc<T> {
    type Result;

    fn calc(&self, x: T) -> Self::Result;
}

impl<T> Calc<T> for () {
    type Result = f32;

    fn calc(&self, x: T) -> Self::Result {
        56.78
    }
}

fn get_calc<T>() -> impl Calc<T,Result = impl Greeter> {
    ()
}

// simple test for impl trait arguments, which shouldn't pose issues

fn use_greeter(x: impl Greeter) {
    x.greet();
}

// driver

fn main() {
    let (a,b) = get_greeters();
    a.greet();
    b.greet();

    let c = get_holder(14u8,7u16,8i32);
    _builtin::print_int(c.get() as _);

    let d = get_calc();
    d.calc("").greet();

    use_greeter(7);
}
