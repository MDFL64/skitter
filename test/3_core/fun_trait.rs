#![feature(fn_traits,unboxed_closures)]

mod _builtin;

struct Callable;

impl FnOnce<(i32,)> for Callable {
    type Output = i32;

    extern "rust-call" fn call_once(self,args: (i32,)) -> i32 {
        args.0 + 10
    }
}

struct CallableIdent;

impl<T> FnOnce<(T,)> for CallableIdent {
    type Output = T;

    extern "rust-call" fn call_once(self,args: (T,)) -> T {
        args.0
    }
}

fn call_it(f: impl FnOnce(i32)->i32) {
    let res = f(5);
    _builtin::print_int(res as _);
}

fn function(x: i32) -> i32 {
    x * 100
}

fn identity<T>(x: T) -> T {
    x
}

pub fn main() {
    call_it(Callable);
    call_it(CallableIdent);
    call_it(function);
    //call_it(identity);
}
