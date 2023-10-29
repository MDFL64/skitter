mod _builtin;

struct R {
    a: S,
    b: Option<S>
}

struct S {
    x: i32
}

fn pat_let(r: &mut R) {
    _builtin::print_int(r.a.x as _);
    let S{ref mut x} = r.a;
    *x += 1;
    _builtin::print_int(r.a.x as _);
}

fn pat_if(r: &mut R) {
    if let Some(ref mut s) = r.b {
        _builtin::print_int(s.x as _);
        s.x += 1;
        _builtin::print_int(s.x as _);
    }
}

fn pat_match(r: &mut R) {
    match r.b {
        Some(ref mut s) => {
            _builtin::print_int(s.x as _);
            s.x += 1;
            _builtin::print_int(s.x as _);
        }
        None => ()
    }
}

pub fn main() {
    let mut r = R{
        a: S{x: 0},
        b: Some(S{x: 0})
    };
    pat_let(&mut r);
    pat_let(&mut r);

    pat_if(&mut r);
    pat_if(&mut r);

    pat_match(&mut r);
    pat_match(&mut r);
}
