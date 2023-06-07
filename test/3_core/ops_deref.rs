mod _builtin;

pub fn main() {

    _builtin::print_int(*DerefA{value: 7} as _);
    _builtin::print_int(*DerefB as _);


    let x: &i64 = &DerefA{value: 123};
    let y: &i64 = &DerefB;

    _builtin::print_int(*x as _);
    _builtin::print_int(*y as _);
}

struct DerefA {
    value: i64
}

struct DerefB;

impl std::ops::Deref for DerefA {
    type Target = i64;

    fn deref(&self) -> &i64 {
        &self.value
    }
}


impl std::ops::Deref for DerefB {
    type Target = i64;

    fn deref(&self) -> &i64 {
        &999
    }
}
