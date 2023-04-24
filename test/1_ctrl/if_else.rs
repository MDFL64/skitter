mod _builtin;

pub fn main() {
    {
        let c = true;
        if c {
            _builtin::print_int(11);
        } else {
            _builtin::print_int(22);
        }
    }

    {
        let c = false;
        if c {
            _builtin::print_int(33);
        } else {
            _builtin::print_int(44);
        }
    }
}
