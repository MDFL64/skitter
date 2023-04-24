mod _builtin;

struct Pair<A,B>(A,B);

type PairX = Pair<u8,i8>;
type PairY<T> = Pair<T,T>;
type PairZ = PairY<u16>;
type PairYY<T> = PairY<PairY<T>>;
type PairTup<A,B> = Pair<(A,A,A,A),(B,B,B,B)>;
type Tri<A,B,C> = Pair<Pair<A, B>, C>;

pub fn main() {

    {
        let mut pair = PairX {0: 100, 1: 100};
    
        if true {
            pair.0 += 200;
            pair.1 += 100;
        }
    
        _builtin::print_int(pair.0 as _);
        _builtin::print_int(pair.1 as _);
    }

    {
        let mut pair = PairY {0: 100i8, 1: 100 };

        if true {
            pair.0 += 34;
            pair.1 += 34;
        }

        _builtin::print_int(pair.0 as _);
        _builtin::print_int(pair.1 as _);
    }

    {
        let mut pair = PairY::<i16> {0: 30000, 1: 30000 };

        if true {
            pair.0 += 5000;
            pair.1 += 5000;
        }

        _builtin::print_int(pair.0 as _);
        _builtin::print_int(pair.1 as _);
    }

    {
        let mut pair = PairZ {0: 60000, 1: 60000};
    
        if true {
            pair.0 += 10000;
            pair.1 += 10000;
        }
    
        _builtin::print_int(pair.0 as _);
        _builtin::print_int(pair.1 as _);
    }

    {
        let mut pair = PairYY::<char> {
            0: Pair{0: 'a', 1: 'b'},
            1: Pair{0: 'c', 1: 'd'}
        };

        _builtin::print_char(pair.0.0 as _);
        _builtin::print_char(pair.0.1 as _);
        _builtin::print_char(pair.1.0 as _);
        _builtin::print_char(pair.1.1 as _);
    }

    {
        let mut pair = PairTup {
            0: (1,2,3,4.1 as _),
            1: (5.0,6.0,7.0,8i8 as _)
        };

        _builtin::print_float(pair.0.0 as _);
        _builtin::print_float(pair.0.1 as _);
        _builtin::print_float(pair.0.2 as _);
        _builtin::print_float(pair.0.3 as _);

        _builtin::print_float(pair.1.0 as _);
        _builtin::print_float(pair.1.1 as _);
        _builtin::print_float(pair.1.2 as _);
        _builtin::print_float(pair.1.3 as _);
    }

    {
        let mut tri: Tri<char,f32,i64> = Pair{0: Pair{0: 'a', 1: 1.0}, 1: 10};

        _builtin::print_char(tri.0.0 as _);
        _builtin::print_float(tri.0.1 as _);
        _builtin::print_int(tri.1 as _);
    }
}
