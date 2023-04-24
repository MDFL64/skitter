mod _builtin;

pub fn main() {
    let mut x = ((),(),(2,),());

    let b = x.0;
    x.2.0 = 5;

    _builtin::print_uint(x.2.0 as _);
}
