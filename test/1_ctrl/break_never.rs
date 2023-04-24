mod _builtin;

pub fn main() {
    let mut i = 0;
    while i < 1000 {
        i += 1;
        if i > 100 {
            let m = break;
        }
        i = if true {
            continue
        } else {
            123
        };
        i += 10000;
    }
    _builtin::print_int(i);
}
