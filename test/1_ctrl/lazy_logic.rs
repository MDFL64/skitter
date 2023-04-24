mod _builtin;

fn test(x: i32, y: bool) -> bool {
    _builtin::print_int(x as _);
    y
}

pub fn main() {
    if test(2,true) && test(3,true)  {
        test(5,true);
    }

    if test(17,true) && test(14,false)  {
        test(12,true);
    }

    if test(23,false) && test(29,true)  {
        test(20,true);
    }

    if test(32,true) || test(33,true)  {
        test(35,true);
    }

    if test(47,true) || test(44,false)  {
        test(42,true);
    }

    if test(53,false) || test(59,true)  {
        test(50,true);
    }
}
