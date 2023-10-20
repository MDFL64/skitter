mod _builtin;

pub fn match_string(x: &str) {
    let res = match x {
        "one" => 1,
        "two" => 2,
        "three" => 3,
        "four" => 4,
        "five" => 5,
        "six" => 6,
        "seven" => 7,
        _ => 0
    };

    _builtin::print_int(res);
}

pub fn main() {
    match_string("two");
    match_string("seven");
    match_string("one");
    match_string("five");
    match_string("six");
    match_string("four");
    match_string("three");
    match_string("???");
}
