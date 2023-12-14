mod _builtin;

enum Pow2 {
    One = 1,
    Two = 2,
    Four = 4,
    Eight = 8,
    Neg = -123
}

fn print_value(v: Pow2) {
    _builtin::print_int(v as _);
}

fn main() {
    print_value(Pow2::One);
    print_value(Pow2::Two);
    print_value(Pow2::Four);
    print_value(Pow2::Eight);
    print_value(Pow2::Neg);
}
