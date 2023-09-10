mod _builtin;

pub fn main() {
    let add_5 = |x| {
        let z = 0;
        let a = z + 5;
        x + a
    };

    let res = add_5(10);
    _builtin::print_int(res as _);
}
