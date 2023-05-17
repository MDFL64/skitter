mod _builtin;

pub fn main() {
    let a = -1234i32;

    let b = a.abs();

    _builtin::print_int(b as _);

    _builtin::print_int(a.signum() as _);
    _builtin::print_int(b.signum() as _);

    let c = (7i32).pow(10);
    _builtin::print_int(c as _);

    //let d = (10i32).swap_bytes();

    //let e = (5i32).reverse_bits();

    //let f = (2i32).rotate_left(5);

    //let d = (10i32).to_be();

    //let bytes = a.to_le_bytes();
    //_builtin::print_int(a.ilog10() as _);
    //let min = a.min(b);

    //let c = a.count_zeros();

    //_builtin::print_int(c as _);
}
