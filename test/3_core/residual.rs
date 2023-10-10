mod _builtin;

fn f1() -> Option<i32> {
    let a = Some(2)?;
    Some(2)
}

pub fn main() {
    f1();

    panic!();
}
