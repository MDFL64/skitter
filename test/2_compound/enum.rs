mod _builtin;

enum Value{
    Int(i32),
    Float(f32),
    Pair(i32,i32),
    None
}

fn print_value(v: Value) {
    if let Value::Int(i) = v {
        _builtin::print_int(i as _);
    } else if let Value::Float(f) = v {
        _builtin::print_float(f as _);
    } else if let Value::Pair(a,b) = v {
        _builtin::print_int(a as _);
        _builtin::print_int(b as _);
    } else if let Value::None = v {
        _builtin::print_bool(false);
    }
}

fn print_value_ref(v: &Value) {
    if let &Value::Int(i) = v {
        _builtin::print_int(i as _);
    } else if let &Value::Float(f) = v {
        _builtin::print_float(f as _);
    } else if let &Value::Pair(a,b) = v {
        _builtin::print_int(a as _);
        _builtin::print_int(b as _);
    } else if let &Value::None = v {
        _builtin::print_bool(false);
    }
}

fn get_n(v: Value) -> i32 {
    if let Value::Int(x) | Value::Pair(x,_)  = v {
        x
    } else if let Value::Float(f) = v {
        (f * 100.0) as i32
    } else if let Value::None = v {
        -1
    } else {
        loop {}
    }
}

fn main() {
    {
        let x = Value::Int(5);
        let y = Value::Float(3.5);
        let z = Value::Pair(6,2);
        let q = Value::None;
    
        print_value(x);
        print_value(y);    
        print_value(z);    
        print_value(q);
    }

    {
        let x = Value::Int(5);
        let y = Value::Float(3.5);
        let z = Value::Pair(6,2);
        let q = Value::None;

        print_value_ref(&x);
        print_value_ref(&y);
        print_value_ref(&z);
        print_value_ref(&q);
    }

    {
        let x = Value::Int(5);
        let y = Value::Float(3.5);
        let z = Value::Pair(6,2);
        let q = Value::None;

        _builtin::print_int(get_n(x) as _);
        _builtin::print_int(get_n(y) as _);
        _builtin::print_int(get_n(z) as _);
        _builtin::print_int(get_n(q) as _);
    }
}
