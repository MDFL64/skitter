mod _builtin;

pub fn main() {
    let mut i = 0;
    let mut sum = 0;
    while i < 100_000_000 {
        i += 1;
        sum = (sum + 5) & 0xFFF;
    }
    _builtin::print_int(sum as _);
}
