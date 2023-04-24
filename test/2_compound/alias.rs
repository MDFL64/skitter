mod _builtin;

type Number = f64;
type SmallNumber = i8;
type Ref<'a,T> = &'a T;
type Tup<T> = (T,T,T);
type A<T> = T;

fn f(x: Number) -> SmallNumber {
    x as SmallNumber
}

struct S<T> {
    m1: (Number,Number),
    m2: Tup<T>
}

pub fn main() {
    let x: A<char> = 'Z';
    _builtin::print_char(x);
    
    _builtin::print_int(f(10.0) as _);
    _builtin::print_int(f(10.1) as _);
    _builtin::print_int(f(1000.0) as _);

    let float_ref: Ref<f32> = &102030405060809.0;
    _builtin::print_float(*float_ref as _);

    let s = S{
        m1: (32.0,64.0),
        m2: ('w','o','w')
    };

    _builtin::print_float(s.m1.0 as _);
    _builtin::print_float(s.m1.1 as _);
    _builtin::print_char(s.m2.0 as _);
    _builtin::print_char(s.m2.1 as _);
    _builtin::print_char(s.m2.2 as _);
}
