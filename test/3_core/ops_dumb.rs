// A dumb test for a non-issue.

use std::ops::Add;

mod _builtin;

struct Bundle<A,B> {
    x: A,
    y: B
}

impl<A: Copy + Add<Output = A>,B: Copy + Add<Output = B>> Bundle<B,A> {
    pub fn add(&self, other: &Self) -> Self {
        Bundle{
            x: self.x + other.x,
            y: self.y + other.y
        }
    }
}

pub fn main() {
    let joe = Bundle{x: 15.5f32, y: 12i8};
    let sam = Bundle{x: 2.15f32, y: 100i8};
    let carl = joe.add(&sam);
    _builtin::print_float(carl.x as _);
    _builtin::print_int(carl.y as _);
}
