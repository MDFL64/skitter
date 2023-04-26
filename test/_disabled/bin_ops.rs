use std::ops::Add;

pub fn main() {
    let mut x = 10;
    x += 5;
    //println!("{}",Thing(5) + Thing(77));
}


struct Thing(i64);

impl Add for Thing {
    type Output = i64;

    fn add(self, other: Self) -> Self {
        other.0 + self.0 + 10
    }
}
