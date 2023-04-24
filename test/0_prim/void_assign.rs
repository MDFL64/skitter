
mod _builtin;

fn main() {
    let mut x = 10;
    _builtin::print_int(x);
    
    let r1 = if true { x = 90 } else { () };
    _builtin::print_int(x);

    let r2 = if true { x += 10 } else { () };
    _builtin::print_int(x);

    let r3 = if true { x <<= 1 } else { () };
    _builtin::print_int(x);

    let a = ();
    let b = a;
    let c = *&();

    *(&mut ()) = ();
}
