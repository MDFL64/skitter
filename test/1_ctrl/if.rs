mod _builtin;

pub fn main() {
    {
        let c = true;
        if c {
            _builtin::print_int(111);
        }
    }

    {
        let c = false;
        if c {
            _builtin::print_int(222);
        }
    }
}
