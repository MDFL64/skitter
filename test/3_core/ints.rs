mod _builtin;

pub fn main() {
    let a = -1234i32;

    let b = a.abs();

    _builtin::print_int(b as _);

    _builtin::print_int(a.signum() as _);
    _builtin::print_int(b.signum() as _);

    let c = (7i32).pow(10);
    _builtin::print_int(c as _);

    _builtin::print_int((a).div_euclid(5) as _);
    _builtin::print_int((a).div_euclid(-5) as _);
    _builtin::print_int((-a).div_euclid(5) as _);
    _builtin::print_int((-a).div_euclid(-5) as _);

    _builtin::print_int(a.swap_bytes() as _);

    let bytes = c.to_le_bytes();
    _builtin::print_int(bytes[0] as _);
    _builtin::print_int(bytes[1] as _);
    _builtin::print_int(bytes[2] as _);
    _builtin::print_int(bytes[3] as _);

    let bytes = c.to_be_bytes();
    _builtin::print_int(bytes[0] as _);
    _builtin::print_int(bytes[1] as _);
    _builtin::print_int(bytes[2] as _);
    _builtin::print_int(bytes[3] as _);

    _builtin::print_bool((1u32).is_power_of_two());
    _builtin::print_bool((2u32).is_power_of_two());
    _builtin::print_bool((3u32).is_power_of_two());
    _builtin::print_bool((4u32).is_power_of_two());
    _builtin::print_bool((5u32).is_power_of_two());
    _builtin::print_bool((6u32).is_power_of_two());
    _builtin::print_bool((7u32).is_power_of_two());
    _builtin::print_bool((8u32).is_power_of_two());
    _builtin::print_bool((9u32).is_power_of_two());

    _builtin::print_int((123i32).ilog10() as _);
    _builtin::print_int((456789i32).ilog10() as _);
    
    _builtin::print_int(a.min(b) as _);
    _builtin::print_int(a.max(b) as _);

    _builtin::print_int(a.count_zeros() as _);
    _builtin::print_int(a.count_ones() as _);

    _builtin::print_int(a.reverse_bits() as _);
    _builtin::print_int(a.rotate_left(5) as _);
    _builtin::print_int(a.rotate_right(5) as _);
}
