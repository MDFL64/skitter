mod _builtin;

struct Point {
    x: i32,
    y: i32
}

impl Point {
    pub fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }

    pub fn print(&self) {
        _builtin::print_int(self.x as _);
        _builtin::print_int(self.y as _);
    }
}

pub fn main() {
    let p = Point::new(1,2);
    p.print();
}
