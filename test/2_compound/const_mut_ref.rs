mod _builtin;

const ALPHA: f64 = 3.5;

fn simple() {
    _builtin::print_float(ALPHA);

    let a = &mut ALPHA;
    *a += 100.0;

    _builtin::print_float(*a);
    _builtin::print_float(ALPHA);
}

fn deref() {
    let a = &mut *&mut ALPHA;
    *a += 100.0;
}

const BUNDLE: [(i8,i8);4] = [
    (1,2),
    (3,4),
    (5,6),
    (7,8)
];

fn complex() {
    let a = &mut BUNDLE;

    a[0] = (0,0);
    a[1] = (0,0);
    a[2] = (0,0);
    a[3] = (0,0);

    let b = &mut BUNDLE[0];
    *b = (1,1);

    let c = &mut BUNDLE[1].1;
    *c = 2;

    _builtin::print_int(BUNDLE[0].0 as _);
    _builtin::print_int(BUNDLE[1].1 as _);
    _builtin::print_int(BUNDLE[2].0 as _);
    _builtin::print_int(BUNDLE[3].1 as _);
}

pub fn main() {
    simple();
    deref();
    simple();
    complex();
}
