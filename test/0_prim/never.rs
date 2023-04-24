mod _builtin;


fn ret_expr() -> i32 {
    10
}

fn ret_keyword() -> i32 {
    return 20;
}

fn ret_nested_1() -> i32 {
    {
        {
            return 30;
        };
    };
}

fn ret_nested_2() -> i32 {
    let x: f32 = {
        return 40;
    };
}

fn ret_ret() -> i32 {
    let x: f32 = ((return 50));
}

// fail to compile on rustc
/*
fn ret_unary() -> i32 {
    !(return 1)
}

fn ret_binary() -> i32 {
    let a: i32 = 10;
    10 + (return 1)
}
*/

fn ret_if_1() -> i32 {
    if true {
        return 60;
    } else {
        return -1;
    };
}

fn ret_if_2() -> i32 {
    if false {
        return -1;
    } else {
        70
    }
}

fn ret_if_3() -> i32 {
    if true {
        80
    } else {
        return -1;
    }
}

fn ret_never_val() -> i32 {
    let x = return 90;
    x
}

fn ret_nested() -> i32 {
    return return return 100;
}

fn ret_logic() -> i32 {
    (return 110) || true;
}

pub fn main() {
    _builtin::print_int(ret_expr() as _);
    _builtin::print_int(ret_keyword() as _);
    _builtin::print_int(ret_nested_1() as _);
    _builtin::print_int(ret_nested_2() as _);
    _builtin::print_int(ret_ret() as _);

    _builtin::print_int(ret_if_1() as _);
    _builtin::print_int(ret_if_2() as _);
    _builtin::print_int(ret_if_3() as _);

    _builtin::print_int(ret_never_val() as _);
    _builtin::print_int(ret_nested() as _);
    _builtin::print_int(ret_logic() as _);


    //_builtin::print_int(g() as _);
    //_builtin::print_int(h() as _);

    //_builtin::print_int(i() as _);
    //_builtin::print_int(j() as _);
}
