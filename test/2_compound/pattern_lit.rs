mod _builtin;

pub fn pick(x: i32) {
    if let 1 = x {
        _builtin::print_int(123);
    } else if let 2 = x {
        _builtin::print_int(456);
    } else if let 3 = &x {
        _builtin::print_int(789);
    } else if let _ = x {
        _builtin::print_int(1000);
    }
}

pub fn pick_tup(t: (i32,i32,i32)) {
    if let (1,x,2) = t {
        _builtin::print_int(1000 + (x as i128));
    } else if let (3,3,x) = t {
        _builtin::print_int(2000 + (x as i128));
    } else if let (2,_,x) = t {
        _builtin::print_int(3000 + (x as i128));
    } else if let (x,..) = t {
        _builtin::print_int(4000 + (x as i128));
    } else {
        _builtin::print_int(9999);
    }
}

pub fn pick_tup_or(t: (i32,i32,i32)) {
    if let (1,x,2) | (2,_,x) = t {
        _builtin::print_int(1000 + (x as i128));
    } else if let (3,3,x) | (x,..) = t {
        _builtin::print_int(2000 + (x as i128));
    } else {
        _builtin::print_int(9999);
    }
}

pub fn pick_tup_ref(t: &(i32,i32,i32)) {
    if let (1,x,2) = t {
        _builtin::print_int(1000 + (*x as i128));
    } else if let (3,3,x) = t {
        _builtin::print_int(2000 + (*x as i128));
    } else if let (2,_,x) = t {
        _builtin::print_int(3000 + (*x as i128));
    } else if let (x,..) = t {
        _builtin::print_int(4000 + (*x as i128));
    } else {
        _builtin::print_int(9999);
    }
}

pub fn pick_tup_or_ref(t: &(i32,i32,i32)) {
    if let (1,x,2) | (2,_,x) = t {
        _builtin::print_int(1000 + (*x as i128));
    } else if let (3,3,x) | (x,..) = t {
        _builtin::print_int(2000 + (*x as i128));
    } else {
        _builtin::print_int(9999);
    }
}

pub fn main() {
    {
        pick(3);
        pick(1);
        pick(0);
        pick(2);
    }
    {
        pick_tup((1,2,3));
        pick_tup((3,3,5));
        pick_tup((1,7,2));
        pick_tup((2,2,2));
    }
    {
        pick_tup_or((1,2,3));
        pick_tup_or((3,3,5));
        pick_tup_or((1,7,2));
        pick_tup_or((2,2,2));
    }
    {
        pick_tup_ref(&(1,2,3));
        pick_tup_ref(&(3,3,5));
        pick_tup_ref(&(1,7,2));
        pick_tup_ref(&(2,2,2));
    }
    {
        pick_tup_or_ref(&(1,2,3));
        pick_tup_or_ref(&(3,3,5));
        pick_tup_or_ref(&(1,7,2));
        pick_tup_or_ref(&(2,2,2));
    }
}
