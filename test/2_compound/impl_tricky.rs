// This doesn't really test anything useful right now -- need to test the lookup between crate boundaries.

mod _builtin;

struct Point<T> {
    x: T,
    y: T
}

impl<T> Point<T> where T: core::marker::Copy {
    pub fn new(x: T, y: T) -> Self {
        Self { x, y }
    }

    pub fn foo(&self) {
        _builtin::print_int(1);
    }
}

impl Point<i32> {
    pub fn bar(&self) {
        _builtin::print_int(2);
    }
}

impl Point<f32> {
    pub fn bar(&self) {
        _builtin::print_int(3);
    }
}

pub fn main() {
    let p = Point::new(1,2);
    p.foo();
    p.bar();
    let p = Point::new(3.0,4.0);
    p.foo();
    p.bar();
}
