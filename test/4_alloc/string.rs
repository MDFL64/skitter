mod _builtin;

#[derive(Debug)]
struct Beep<'a>{
    a: i32,
    b: f32,
    c: Option<&'a [i8]>
}

pub fn main() {
    {
        let mut text = String::new();
        text.push_str("foo");
        text.push_str("bar");
        text.push(' ');
        text.push_str("baz");
        _builtin::print_raw(&text);
        _builtin::print_raw("\n");
        let edited = text.replace("foob","@");
        _builtin::print_raw(&edited);
        _builtin::print_raw("\n");
    }
    {
        let array = [5,4,3,2,1];

        let beep = Beep{
            a: 12345,
            b: 0.12345,
            c: Some(&array)
        };

        let text = format!("{:?}\n",beep);
        _builtin::print_raw(&text);
    }
}
