mod _builtin;

pub fn main() {
    {
        let mut text = String::new();
        text.push_str("foo");
        text.push_str("bar");
        text.push(' ');
        text.push_str("baz");
        _builtin::print_raw(&text);
        _builtin::print_raw("\n");
    }
    {
        let text = format!("{} {}\n",12345,0.12345);
        _builtin::print_raw(&text);
    }
}
