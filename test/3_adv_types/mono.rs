mod _builtin;

fn pick<T>(s: bool, x: T, y: T) -> T {
    if s {
        x
    } else {
        y
    }
}

struct S<T>(T);

impl<T: Copy> S<T> {
    fn new(x: T) -> Self {
        Self(x)
    }

    fn re_pick(&self, x: T, s: bool) -> T {
        pick(s,self.0,x)
    } 
}

fn main() {

    let x = pick(true,5,2);
    let y = pick(false,6.5f32,3.14);
    let z = pick(true,&50,&200);

    _builtin::print_int(x as _);
    _builtin::print_float(y as _);
    _builtin::print_int(*z);

    let s = S::new(1i8);
    _builtin::print_int(s.0 as _);
    let m = s.re_pick(7,true);
    let n = s.re_pick(8,false);
    _builtin::print_int(m as _);
    _builtin::print_int(n as _);
}
