mod _builtin;

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
        let t = (1,2,999,3);
        let (x,y,_,z) = &t;
        _builtin::print_int(*x as _);
        _builtin::print_int(*y as _);
        _builtin::print_int(*z as _);
    }
}
