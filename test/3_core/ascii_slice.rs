mod _builtin;

fn print_string(s: &str) {
    _builtin::print_raw(s);
    _builtin::print_raw("\n");
}

pub fn main() {
    let text = "The quick brown fox jumps over the lazy dog.";

    print_string(text);

    let mut data = [0;50];

    let source = text.as_bytes();

    let len = source.len().min(data.len());

    for i in 0..len {
        data[i] = source[i];
    }

    print_string(core::str::from_utf8(&data).unwrap_or("error"));

    data.make_ascii_uppercase();

    print_string(core::str::from_utf8(&data).unwrap_or("error"));

    data.make_ascii_lowercase();

    print_string(core::str::from_utf8(&data).unwrap_or("error"));
}
