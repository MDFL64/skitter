mod _builtin;

pub fn main() {
    {
        let x: f64 = 13521880010.0;
        let y: f64 = 0.12352371238;
        
        _builtin::print_float((-x) as _);
        _builtin::print_float((x + y) as _);
        _builtin::print_float((x - y) as _);
        _builtin::print_float((x * y) as _);
        _builtin::print_float((x / y) as _);
        _builtin::print_float((x % y) as _);
    
        _builtin::print_bool(x == y);
        _builtin::print_bool(x != y);
        _builtin::print_bool(x < y);
        _builtin::print_bool(x > y);
        _builtin::print_bool(x <= y);
        _builtin::print_bool(x >= y);

        let mut m = x;
        m += y; _builtin::print_float(m as _);
        m -= y; _builtin::print_float(m as _);
        m *= y; _builtin::print_float(m as _);
        m /= y; _builtin::print_float(m as _);
        m %= y; _builtin::print_float(m as _);
    }

    {
        let x: f64 = 13521880010.0;
        let y: f64 = 1.0/0.0;
        
        _builtin::print_float((-x) as _);
        _builtin::print_float((x + y) as _);
        _builtin::print_float((x - y) as _);
        _builtin::print_float((x * y) as _);
        _builtin::print_float((x / y) as _);
        _builtin::print_float((x % y) as _);
    
        _builtin::print_bool(x == y);
        _builtin::print_bool(x != y);
        _builtin::print_bool(x < y);
        _builtin::print_bool(x > y);
        _builtin::print_bool(x <= y);
        _builtin::print_bool(x >= y);
    }

    {
        let x: f64 = 13521880010.0;
        let y: f64 = 0.0/0.0;
        
        _builtin::print_float((-x) as _);
        _builtin::print_float((x + y) as _);
        _builtin::print_float((x - y) as _);
        _builtin::print_float((x * y) as _);
        _builtin::print_float((x / y) as _);
        _builtin::print_float((x % y) as _);
    
        _builtin::print_bool(x == y);
        _builtin::print_bool(x != y);
        _builtin::print_bool(x < y);
        _builtin::print_bool(x > y);
        _builtin::print_bool(x <= y);
        _builtin::print_bool(x >= y);
    }

    {
        let x: f64 = 0.0/0.0;
        let y: f64 = 0.0/0.0;
        
        _builtin::print_float((-x) as _);
        _builtin::print_float((x + y) as _);
        _builtin::print_float((x - y) as _);
        _builtin::print_float((x * y) as _);
        _builtin::print_float((x / y) as _);
        _builtin::print_float((x % y) as _);
    
        _builtin::print_bool(x == y);
        _builtin::print_bool(x != y);
        _builtin::print_bool(x < y);
        _builtin::print_bool(x > y);
        _builtin::print_bool(x <= y);
        _builtin::print_bool(x >= y);
    }
}
