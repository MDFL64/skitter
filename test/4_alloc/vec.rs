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
        let data = vec!(1,3,5,7);
        _builtin::print_int(data.iter().sum::<i64>() as _);
    }
}
