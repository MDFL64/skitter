mod _builtin;

pub fn main() {
    let mut i = 0;
    let mut m = 0;
    while i < 10 {
        let mut j = 0;
        while j < 10 {
            j += 1;
            m += 3;
        }
        i += 1;
        m += 5;
    }
    _builtin::print_int(m);
}
