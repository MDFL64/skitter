mod _builtin;

const COUNT: u64 = 999_000;

pub fn main() {
    let mut primes = [true;COUNT as usize];
    primes[0] = false;
    primes[1] = false;

    let mut a: u64 = 0;
    while a < COUNT {
        if primes[a as usize] {
            _builtin::print_int(a as _);

            let mut b: u64 = 2;
            loop {
                let r = a * b;
                if r < COUNT {
                    primes[r as usize] = false;
                } else {
                    break;
                }
                b += 1;
            }
        }

        a += 1;
    }
}
