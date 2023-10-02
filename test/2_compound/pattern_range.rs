#![feature(exclusive_range_pattern)]

mod _builtin;

pub fn check_1(x: i32) -> i32 {
    match x {
        20.. => 1,
        _ => 0
    }
}

pub fn check_2(x: i32) -> i32 {
    match x {
        ..20 => 2,
        _ => 3
    }
}

pub fn check_3(x: i32) -> i32 {
    match x {
        15..20 => 5,
        _ => 4
    }
}

pub fn check_4(x: i32) -> i32 {
    match x {
        15..=20 => 7,
        _ => 6
    }
}

pub fn main() {
    _builtin::print_int(check_1(-999) as _);
    _builtin::print_int(check_1(19) as _);
    _builtin::print_int(check_1(20) as _);
    _builtin::print_int(check_1(21) as _);
    _builtin::print_int(check_1(999) as _);

    _builtin::print_int(check_2(-999) as _);
    _builtin::print_int(check_2(19) as _);
    _builtin::print_int(check_2(20) as _);
    _builtin::print_int(check_2(21) as _);
    _builtin::print_int(check_2(999) as _);

    _builtin::print_int(check_3(14) as _);
    _builtin::print_int(check_3(15) as _);
    _builtin::print_int(check_3(16) as _);
    _builtin::print_int(check_3(17) as _);
    _builtin::print_int(check_3(18) as _);
    _builtin::print_int(check_3(19) as _);
    _builtin::print_int(check_3(20) as _);
    _builtin::print_int(check_3(21) as _);

    _builtin::print_int(check_4(14) as _);
    _builtin::print_int(check_4(15) as _);
    _builtin::print_int(check_4(16) as _);
    _builtin::print_int(check_4(17) as _);
    _builtin::print_int(check_4(18) as _);
    _builtin::print_int(check_4(19) as _);
    _builtin::print_int(check_4(20) as _);
    _builtin::print_int(check_4(21) as _);
}
