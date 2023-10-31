use std::rc::Rc;

mod _builtin;

pub fn main() {
    {
        let void = Rc::new(());
        let n1 = Rc::new(5);
        let n2 = Rc::new(7);
        let n3 = Rc::new(3);
    
        let res = *n1 + *n2 + *n3;
        _builtin::print_int(res);
    }
    {
        let rc_slice: Rc<[u64]> = Rc::new([1,2,3,4]);
        let x: u64 = rc_slice.iter().sum();
    }
}
