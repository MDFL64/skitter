trait Meme {
    fn meme(&self);
}

impl Meme for &i32 {
    fn meme(&self) {
        println!("A");
    }
}

impl Meme for &mut i32 {
    fn meme(&self) {
        println!("B");
    }
}

fn main() {
    println!("yo");
    (&10i32).meme();
    (&mut 10i32).meme();
    (& &mut 10i32).meme();
    (&mut & 10i32).meme();
}
