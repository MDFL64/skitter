mod _builtin;

trait A {
    type T: B;
}

trait B {
    fn print();
}

impl A for i32 {
    type T = bool;
}

struct S<T1,T2> {
    a: T1,
    b: T2
}

impl<T1,T2: B> A for S<T1,T2> {
    type T = T2;
}

impl B for bool {
    fn print() {
        _builtin::print_int(1);
    }
}

impl B for i8 {
    fn print() {
        _builtin::print_int(2);
    }
}

impl B for u8 {
    fn print() {
        _builtin::print_int(3);
    }
}

fn run<T: A>() {
    T::T::print();
}

pub fn main() {
    run::<i32>();
    run::<S<bool,bool>>();
    run::<S<i32,u8>>();
    run::<S<f32,i8>>();
}
