mod _builtin;

fn match_string(x: &str) {
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

fn match_array(x: [i32;5]) {
    let res = match x {
        [1,0,..] => 0,
        [1,..] => 1,
        [2,2,..] => 2,
        [..,2,3] => 3,
        [..,3] => 4,
        [5,..,5] => 5,
        [6,6,6,6,6] => 6,
        [7,a @ ..,7] => a[0] + a[1] + a[2],
        [8,8,8,8,ref a @ ..] => a[0],
        [9,9,9,9,9,a @ ..] => a.len() as i32 + 9,
        [..] => 10
    };

    _builtin::print_int(res as _);
}

fn match_array_ref(x: &[i32;5]) {
    let res = match x {
        [1,0,..] => 0,
        [1,..] => 1,
        [2,2,..] => 2,
        [..,2,3] => 3,
        [..,3] => 4,
        [5,..,5] => 5,
        [6,6,6,6,6] => 6,
        [7,a @ ..,7] => a[0] + a[1] + a[2],
        [8,8,8,8,ref a @ ..] => a[0],
        [9,9,9,9,9,a @ ..] => a.len() as i32 + 9,
        [..] => 10
    };

    _builtin::print_int(res as _);
}

fn match_slice(x: &[i32]) {
    let res = match x {
        [1,0,..] => 0,
        [1,..] => 1,
        [2,2,..] => 2,
        [..,2,3] => 3,
        [..,3] => 4,
        [5,..,5] => 5,
        [6,6,6,6,6] => 6,
        [7,a @ ..,7] => a[0] + a[1] + a[2],
        [8,8,8,8,ref a @ ..] => a[0],
        [9,9,9,9,9,a @ ..] => a.len() as i32 + 9,
        [..] => 10
    };

    _builtin::print_int(res as _);
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
    _builtin::print_int(111111111);
    match_array([1,0,0,2,3]);
    match_array([1,1,0,2,3]);
    match_array([2,2,0,2,3]);
    match_array([2,1,0,2,3]);
    match_array([2,1,0,4,3]);
    match_array([5,1,0,4,5]);
    match_array([6,6,6,6,6]);
    match_array([7,6,6,6,7]);
    match_array([8,8,8,8,8]);
    match_array([9,9,9,9,9]);
    match_array([9,9,1,9,9]);
    _builtin::print_int(111111111);
    match_array_ref(&[1,0,0,2,3]);
    match_array_ref(&[1,1,0,2,3]);
    match_array_ref(&[2,2,0,2,3]);
    match_array_ref(&[2,1,0,2,3]);
    match_array_ref(&[2,1,0,4,3]);
    match_array_ref(&[5,1,0,4,5]);
    match_array_ref(&[6,6,6,6,6]);
    match_array_ref(&[7,6,6,6,7]);
    match_array_ref(&[8,8,8,8,8]);
    match_array_ref(&[9,9,9,9,9]);
    match_array_ref(&[9,9,1,9,9]);
    _builtin::print_int(111111111);
    match_slice(&[1,0,0,2,3]);
    match_slice(&[1,1,0,2,3]);
    match_slice(&[2,2,0,2,3]);
    match_slice(&[2,1,0,2,3]);
    match_slice(&[2,1,0,4,3]);
    match_slice(&[5,1,0,4,5]);
    match_slice(&[6,6,6,6,6]);
    match_slice(&[7,6,6,6,7]);
    match_slice(&[8,8,8,8,8]);
    match_slice(&[9,9,9,9,9]);
    match_slice(&[9,9,1,9,9]);
}
