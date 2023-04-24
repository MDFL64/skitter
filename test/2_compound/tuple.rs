mod _builtin;

pub fn main() {
    let mut x = (1,2,3,4);

    x.0 += 10;

    let y = &mut x.1;
    *y += 10;

    let z = &mut x;
    z.2 += 10i8;

    z.3 = z.1;

    _builtin::print_int(x.0 as _);
    _builtin::print_int((&x).1 as _);
    _builtin::print_int((&&x).2 as _);
    _builtin::print_int((&&&x).3 as _);
    
    let copy = if x.0 > 0 { x } else { (0,0,0,0) };
    
    _builtin::print_int((copy.0 + copy.1 + copy.3) as _);
}
