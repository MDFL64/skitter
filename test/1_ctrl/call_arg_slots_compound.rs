mod _builtin;

type Group = (i32,f32,u8);

fn add(a: Group, b: Group) -> Group {
    (a.0+b.0,a.1+b.1,a.2+b.2)
}

pub fn main() {
    let p = add((1,2.5,3),(3,4.15,5));
    _builtin::print_int(p.0 as _);
    _builtin::print_float(p.1 as _);
    _builtin::print_int(p.2 as _);
}
