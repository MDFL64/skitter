mod _builtin;

pub fn main() {
    {
        let x: isize = 1003258271;
        let y: isize = -258271;
        
        _builtin::print_int((-x) as _);
        _builtin::print_int((x + y) as _);
        _builtin::print_int((x - y) as _);
        _builtin::print_int((x * y) as _);
        _builtin::print_int((x / y) as _);
        _builtin::print_int((x % y) as _);
    
        _builtin::print_int((!x) as _);
        _builtin::print_int((x & y) as _);
        _builtin::print_int((x | y) as _);
        _builtin::print_int((x ^ y) as _);
        _builtin::print_int((x >> 4) as _);
        _builtin::print_int((y >> 4) as _);
        _builtin::print_int((x << 4) as _);
        _builtin::print_int((y << 4) as _);
    
        _builtin::print_bool(x == y);
        _builtin::print_bool(x != y);
        _builtin::print_bool(x < y);
        _builtin::print_bool(x > y);
        _builtin::print_bool(x <= y);
        _builtin::print_bool(x >= y);

        let mut m = x;
        m += y; _builtin::print_int(m as _);
        m -= y; _builtin::print_int(m as _);
        m *= y; _builtin::print_int(m as _);
        m /= y; _builtin::print_int(m as _);
        m %= y; _builtin::print_int(m as _);
        m |= x; _builtin::print_int(m as _);
        m &= y; _builtin::print_int(m as _);
        m ^= y; _builtin::print_int(m as _);
        m >>= 4; _builtin::print_int(m as _);
        m <<= 4; _builtin::print_int(m as _);
    }

    {
        let x: usize = 10032582712;
        let y: usize = 6482902;
        
        _builtin::print_uint((x + y) as _);
        _builtin::print_uint((x - y) as _);
        _builtin::print_uint((x * y) as _);
        _builtin::print_uint((x / y) as _);
        _builtin::print_uint((x % y) as _);
    
        _builtin::print_uint((!x) as _);
        _builtin::print_uint((x & y) as _);
        _builtin::print_uint((x | y) as _);
        _builtin::print_uint((x ^ y) as _);
        _builtin::print_uint((x >> 4) as _);
        _builtin::print_uint((y >> 4) as _);
        _builtin::print_uint((x << 4) as _);
        _builtin::print_uint((y << 4) as _);
    
        _builtin::print_bool(x == y);
        _builtin::print_bool(x != y);
        _builtin::print_bool(x < y);
        _builtin::print_bool(x > y);
        _builtin::print_bool(x <= y);
        _builtin::print_bool(x >= y);

        let mut m = x;
        m += y; _builtin::print_uint(m as _);
        m -= y; _builtin::print_uint(m as _);
        m *= y; _builtin::print_uint(m as _);
        m /= y; _builtin::print_uint(m as _);
        m %= y; _builtin::print_uint(m as _);
        m |= x; _builtin::print_uint(m as _);
        m &= y; _builtin::print_uint(m as _);
        m ^= y; _builtin::print_uint(m as _);
        m >>= 4; _builtin::print_uint(m as _);
        m <<= 4; _builtin::print_uint(m as _);
    }
}
