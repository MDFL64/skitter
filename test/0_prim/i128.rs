mod _builtin;

pub fn main() {
    {
        let x: i128 = 166892710123969045;
        let y: i128 = -25000012382712952;
        
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
        let x: u128 = 306892710123969045;
        let y: u128 = 25000012382712952;
        
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
