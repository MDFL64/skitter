mod _builtin;

pub fn main() {
    //let x: i64 = 53482;
    //let y: f64 = std::mem::transmute(x);
    //_builtin::print_float(y);

    let size = std::mem::size_of::<i32>();
    _builtin::print_uint(size as _);
}
