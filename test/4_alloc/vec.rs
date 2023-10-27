mod _builtin;

pub fn main() {
    {
        let mut data = Vec::new();
        for i in 0..1000 {
            data.push(i);
        }
        _builtin::print_int(data.len() as _);
        _builtin::print_int(data.iter().sum::<i64>() as _);
    }
    {
        let mut data = vec!(51,12,53,37,52,43,33,65,94,58);
        _builtin::print_int(data.iter().sum::<i64>() as _);
        for i in 0..5 {
            data.pop();
        }
        _builtin::print_int(data.iter().sum::<i64>() as _);
        _builtin::print_int(data.capacity() as _);
        data.shrink_to_fit(); // NOTE: may not behave consistently?
        _builtin::print_int(data.capacity() as _);

        let bs = data.into_boxed_slice();
        _builtin::print_int(bs.len() as _);
    }
}
