mod _builtin;

trait Print {
    fn print();
}

impl Print for i32 {
    fn print() {
        _builtin::print_int(123);
    }
}

impl Print for f32 {
    fn print() {
        _builtin::print_int(456);
    }
}

struct Buddy<T>(T);

impl<T> Buddy<T> {
    // unstable: #![feature(inherent_associated_types)]
    //type FavType = i64;

    const BEST_NUM: i8 = 10;

    pub fn go() where T: Print {
        T::print()
    }

    pub fn go_2<V>() where V: Print {
        V::print()
    }
}

pub fn main() {
    Buddy::<i32>::go();
    Buddy::<f32>::go();
    Buddy::<i32>::go_2::<f32>();
    Buddy::<f32>::go_2::<i32>();

    _builtin::print_int(Buddy::<i32>::BEST_NUM as _);
}
