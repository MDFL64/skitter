mod _builtin;

pub fn main() {
    {
        let c = true;
        let r = 5 + if c {
            11
        } else {
            22
        } + 5;
        _builtin::print_int(r);
    }

    {
        let c = false;
        let r = 5 + if c {
            33
        } else {
            44
        } + 5;
        _builtin::print_int(r);
    }
}
