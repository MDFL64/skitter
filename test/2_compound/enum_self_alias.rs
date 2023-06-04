mod _builtin;

enum Value{
    A{a:i32},
    B{b:i32}
}

impl Value {
    pub fn new_a(a: i32) -> Self {
        Self::A{a}
    }

    pub fn new_b(b: i32) -> Self {
        Self::B{b}
    }
}

fn main() {
    let a = Value::new_a(5);
    let b = Value::new_b(5);
}
