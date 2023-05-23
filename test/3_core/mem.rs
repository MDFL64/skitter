mod _builtin;

pub fn main() {
    unsafe {
        let a = std::mem::align_of::<()>();
        let b = std::mem::align_of::<(i32,)>();
        let c = std::mem::align_of::<(i8,i64)>();
        let d = std::mem::align_of::<&i8>();
        let e = std::mem::align_of::<&str>();

        _builtin::print_uint(a as _);
        _builtin::print_uint(b as _);
        _builtin::print_uint(c as _);
        _builtin::print_uint(d as _);
        _builtin::print_uint(e as _);

        let a = std::mem::size_of::<()>();
        let b = std::mem::size_of::<(i32,)>();
        let c = std::mem::size_of::<(i8,i64)>();
        let d = std::mem::size_of::<&i8>();
        let e = std::mem::size_of::<&str>();

        _builtin::print_uint(a as _);
        _builtin::print_uint(b as _);
        _builtin::print_uint(c as _);
        _builtin::print_uint(d as _);
        _builtin::print_uint(e as _);

        let a = std::mem::align_of_val(&1i16);
        let b = std::mem::align_of_val(&[1,2,3,4i64]);
        let tmp: &[i32] = &[1,2,3,4];
        let c = std::mem::align_of_val(tmp);
        let d = std::mem::align_of_val("aaaaaaaaaaaaaaa");

        _builtin::print_uint(a as _);
        _builtin::print_uint(b as _);
        _builtin::print_uint(c as _);
        _builtin::print_uint(d as _);

        let a = std::mem::size_of_val(&1i16);
        let b = std::mem::size_of_val(&[1,2,3,4i64]);
        let tmp: &[i32] = &[1,2,3,4];
        let c = std::mem::size_of_val(tmp);
        let d = std::mem::size_of_val("aaaaaaaaaaaaaaa");

        _builtin::print_uint(a as _);
        _builtin::print_uint(b as _);
        _builtin::print_uint(c as _);
        _builtin::print_uint(d as _);

        let x: i64 = 53482;
        let y: f64 = std::mem::transmute(x);
        _builtin::print_float(y);

        let mut x: i64 = 10;
        let mut y: i64 = 20;
        std::mem::swap(&mut x,&mut y);
        _builtin::print_int(x as _);
        _builtin::print_int(y as _);

        let mut z = std::mem::replace(&mut x,5);
        _builtin::print_int(x as _);
        _builtin::print_int(z as _);

        let mut x = Some(10.5f32);
        print_opt(&x);

        let y = std::mem::take(&mut x);
        print_opt(&x);
        print_opt(&y);

        let mut m: (i32,f32) = std::mem::zeroed();
        _builtin::print_int(m.0 as _);
        _builtin::print_float(m.1 as _);

    }

    //let size = std::mem::size_of::<i32>();
    //_builtin::print_uint(size as _);
}

fn print_opt(opt: &Option<f32>) {
    match opt {
        Some(n) => _builtin::print_float(*n as _),
        None => _builtin::print_bool(false)
    }
}
