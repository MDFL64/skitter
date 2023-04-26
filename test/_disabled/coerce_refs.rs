pub fn main() {

    let x: &i64 = &DerefExample{value: 123};

    println!("{}",x);

    println!("{}",DerefExample{value: 5}.min(*DerefExample{value: 7}));
}

struct DerefExample {
    value: i64
}

impl std::ops::Deref for DerefExample {
    type Target = i64;

    fn deref(&self) -> &i64 {
        &self.value
    }
}
