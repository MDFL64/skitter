mod _builtin;

trait Meme {
    fn meme(&self);
}

impl Meme for &i32 {
    fn meme(&self) {
        _builtin::print_int(1);
    }
}

impl Meme for &mut i32 {
    fn meme(&self) {
        _builtin::print_int(2);
    }
}

fn main() {
    (&10i32).meme();
    (&mut 10i32).meme();
    (& &mut 10i32).meme();
    (&mut & 10i32).meme();
}
