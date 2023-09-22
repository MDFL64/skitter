mod _builtin;

pub fn get_add() -> fn(f32,f32) -> f32 {
    |x,y| x + y
}

pub fn get_mid<T>() -> fn(T,T,T) -> T {
    |a,b,c| b
}

pub fn main() {
    let add = get_add();
    _builtin::print_float(add(5.0,2.5) as _);

    let mid_f64 = get_mid::<f64>();
    _builtin::print_float(mid_f64(1.1,2.2,3.3) as _);

    let mid_u8 = get_mid::<u8>();
    _builtin::print_float(mid_u8(50,100,150) as _);
}
