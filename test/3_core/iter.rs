mod _builtin;

pub fn main() {
    for i in 1..=10000 {
        _builtin::print_int(i);
    }
    /*let array: &[u8] = &[4,2,7,100,3,4];
    for x in array {
        _builtin::print_int(*x as _);
    }*/

    fn add_100(x: i32) -> i32 {
        x + 100
    }

    let mapped = (1..=10).map(add_100);

    for i in mapped {
        _builtin::print_int(i as _);
    }
}
