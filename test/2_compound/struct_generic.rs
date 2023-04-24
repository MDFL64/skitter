mod _builtin;

struct Vec2<T> {
    x: T,
    y: T
}

struct Pair<A,B>(A,B);

pub fn main() {
    let v1 = Vec2{x: 123.0, y: 456.0};
    _builtin::print_float(v1.x as _);
    _builtin::print_float(v1.y as _);

    let mut v2 = Vec2{x: 100, y: 0i8};
    if true {
        v2.x += 100;
    }
    _builtin::print_int(v2.x as _);
    _builtin::print_int(v2.y as _);

    {
        let pair = Pair{
            0: Vec2{x: true, y: false},
            1: Vec2{x: 'x', y: 'y'}
        };
    
        _builtin::print_bool(pair.0.x);
        _builtin::print_bool(pair.0.y);
        _builtin::print_char(pair.1.x);
        _builtin::print_char(pair.1.y);
    }

    {
        let mut pair = Pair::<u8,i8> {0: 100, 1: 100};
    
        if true {
            pair.0 += 200;
            pair.1 += 100;
        }
    
        _builtin::print_int(pair.0 as _);
        _builtin::print_int(pair.1 as _);
    }

    {
        let mut pair: Pair<u8,i8> = Pair {0: 100, 1: 100};
    
        if true {
            pair.0 += 200;
            pair.1 += 100;
        }
    
        _builtin::print_int(pair.0 as _);
        _builtin::print_int(pair.1 as _);
    }
}
