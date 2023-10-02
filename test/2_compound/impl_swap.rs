mod _builtin;

struct Pair<A,B> {
    x: A,
    y: B
}

impl<B,A> Pair<A,B> where A: Copy, B: Copy {
    fn new(x: A, y: B) -> Self {
        Self { x, y }
    }

    fn a(&self) -> A {
        self.x
    }

    fn b(&self) -> B {
        self.y
    }
}

pub fn main() {
    let p1 = Pair::new(true,101i128);
    let p2 = Pair::new(5.2f64,2i8);

    _builtin::print_bool(p1.a());
    _builtin::print_int(p1.b() as _);

    _builtin::print_float(p2.a() as _);
    _builtin::print_int(p2.b() as _);
}
