mod _builtin;

pub fn main() {
    let x = {
        let a = 5;
        let b = a;
        let c = a;
        let d = a;
        a + b + c + d
    } + ({
        let a = 10;
        let b = a;
        let c = a;
        let d = a;
        a + b + c + d
    } + {
        let a = 100;
        let b = a;
        let c = a;
        let d = a;
        a + b + c + d
    });
    _builtin::print_int(x);

    let y = { 11 + 100 };
    _builtin::print_int(y);

    _builtin::print_int({1 + 2 + 4 + 8});
}
