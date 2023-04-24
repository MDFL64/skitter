mod _builtin;

pub fn main() {
    {
        let a = 100;
        let b = &a;
        let c = { &*b };
        let d = &{ *b };
    
        let pb = b as *const i32 as usize;
        let pc = c as *const i32 as usize;
        let pd = d as *const i32 as usize;

        _builtin::print_bool(pb == pc);
        _builtin::print_bool(pc != pd);

        unsafe {
            _builtin::print_int( *(pb as *mut i32) as _ );
            let x = &*(pb as *mut i32);
            _builtin::print_int(*x as _);
        }
    }
}
