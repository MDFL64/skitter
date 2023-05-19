mod _builtin;

#[derive(Clone,PartialEq)]
struct Thing<'a> {
    x: i32,
    y: bool,
    z: f32,
    next: Option<&'a Thing<'a>>
}

fn main() {
    {
        let thing1 = Thing {
            x: 15,
            y: false,
            z: 55.0,
            next: None
        };
        let thing2 = thing1.clone();
        _builtin::print_bool(thing1 == thing2);
        _builtin::print_bool(thing1 != thing2);
    }
}
