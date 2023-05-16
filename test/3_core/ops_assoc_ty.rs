mod _builtin;

use std::ops::{Add,Mul};

pub fn wacky_math<T>(a: T, b: T) -> <T as Mul>::Output
    where T: Add<Output = T> + Mul + Copy
{
    (a + b) * b
}

pub fn main() {
    let res = wacky_math(3i8,8);
    _builtin::print_int(res as _);
}
