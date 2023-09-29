mod _builtin;

trait Print {
    fn print();
}

trait HasGAT {
    type GAT<T: Print>: Print;
}

struct Triple<A,B,C> {
    a: A,
    b: B,
    c: C,
}

impl<A: Print,B: Print,C: Print> Print for Triple<A,B,C> {
    fn print() {
        A::print();
        B::print();
        C::print();
    }
}

impl HasGAT for i8 {
    type GAT<T: Print> = Triple<i8,i8,i8>;
}

impl HasGAT for i16 {
    type GAT<T: Print> = Triple<i16,T,i16>;
}

impl HasGAT for i32 {
    type GAT<T: Print> = Triple<T,T,T>;
}

impl Print for i8 {
    fn print() {
        _builtin::print_int(1);
    }
}

impl Print for i16 {
    fn print() {
        _builtin::print_int(2);
    }
}

impl Print for i32 {
    fn print() {
        _builtin::print_int(3);
    }
}

fn go<A: HasGAT,B: Print>() {
    A::GAT::<B>::print();
}

fn main() {
    go::<i8,i8>();
    go::<i8,i16>();
    go::<i8,i32>();

    go::<i16,i8>();
    go::<i16,i16>();
    go::<i16,i32>();

    go::<i32,i8>();
    go::<i32,i16>();
    go::<i32,i32>();
}
