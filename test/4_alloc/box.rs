#![feature(new_uninit)]

mod _builtin;

pub fn main() {
    let void = Box::new(());
    let n1 = Box::new(5);
    let n2 = Box::new(7);
    let n3 = Box::new(3);

    let res = *n1 + *n2 + *n3;
    _builtin::print_int(res);

    /*{
        let data = Box::<[u8;64]>::new_zeroed();
        let data = unsafe { data.assume_init() };
    }*/
}
