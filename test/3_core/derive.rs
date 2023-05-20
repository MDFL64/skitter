mod _builtin;

#[derive(Clone,PartialEq,Default)]
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
            z: 55.5,
            next: None
        };
        let thing2 = thing1.clone();

        _builtin::print_int(thing2.x as _);
        _builtin::print_bool(thing2.y);
        _builtin::print_float(thing2.z as _);

        let thing3: Thing = Default::default();

        _builtin::print_bool(thing1 == thing2);
        _builtin::print_bool(thing1 != thing2);
        _builtin::print_bool(thing1 == thing3);
        _builtin::print_bool(thing1 != thing3);
    }
}
