mod _builtin;

fn main() {
    {
        let mut x = &5.0;
        let mut y = 10.0;
        x = &mut y;
        _builtin::print_float(*x);
    }

    {
        let mut x = &5.0;
        let mut y = 20.0;
        x = { &mut y };
        _builtin::print_float(*x);
    }

    {
        let tuple: (&_,&_) = (&mut 100, &mut 200);
        _builtin::print_int(*tuple.0);
        _builtin::print_int(*tuple.1);
    }
}

/* TODO

    propagation tests, such as:
    let x: (&i32,&i32) = if meme() {
        (&mut b,&mut a)
    } else {
        (&mut a,&mut b)
    };

*/
