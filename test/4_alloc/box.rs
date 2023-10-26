#![feature(new_uninit)]
#![feature(allocator_api)]

mod _builtin;

pub fn main() {
    {
        let void = Box::new(());
        let n1 = Box::new(5);
        let n2 = Box::new(7);
        let n3 = Box::new(3);
    
        let res = *n1 + *n2 + *n3;
        _builtin::print_int(res);
    }

    {
        let tup = Box::try_new((1,2,3,4)).unwrap();
        _builtin::print_int(tup.0 as _);
        _builtin::print_int(tup.1 as _);
        _builtin::print_int(tup.2 as _);
        _builtin::print_int(tup.3 as _);
    }

    {
        let data = Box::<[u8;64]>::new_zeroed();
        let mut data = unsafe { data.assume_init() };
        data[0] = 63;
        data[16] = 51;
        data[32] = 24;
        data[48] = 87;
        let mut sum = 0;
        for e in data.iter() {
            sum += *e;
        }
        _builtin::print_int(sum as _);
    }
}
