mod _builtin;

pub fn butt(mut x @ mut y @ mut z: i32) {
    x += 1;
    y += 20;
    z += 30;

    _builtin::print_int(x as _);
    _builtin::print_int(y as _);
    _builtin::print_int(z as _);
}

pub fn main() {
    {
        let t = (1,2,999,3);
        let (x,y,_,z) = t;
        _builtin::print_int(x as _);
        _builtin::print_int(y as _);
        _builtin::print_int(z as _);
    }
    {
        let (ref x,y,_,ref z) = (1,2,999,3);
        _builtin::print_int(*x as _);
        _builtin::print_int(y as _);
        _builtin::print_int(*z as _);
    }
    {
        let mut t = (1,2,999,3);
        let (x,y,..) = &mut t;
        *x += 100;
        *y += 200;
        _builtin::print_int(t.0 as _);
        _builtin::print_int(t.1 as _);
    }
    {
        let mut x @ mut y @ mut z = 15;
        _builtin::print_int(x as _);
        x += 15;
        _builtin::print_int(x as _);
        _builtin::print_int(y as _);
        _builtin::print_int(z as _);
    }
    butt(3);
    {
        let &x = &111;
        _builtin::print_int(x as _);
    }
}
