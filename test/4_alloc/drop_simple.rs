mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;

fn f(x: &'static str) -> LogDrop {
    let a = LogDrop("f");
    LogDrop(x)
}

fn g(x: LogDrop, y: LogDrop) {
    LogDrop("g");
}

pub fn main() {
    LogDrop("main 1");
    _builtin::print_int(1);
    let a = LogDrop("main 2");
    let c = LogDrop("main 3");
    let b = f("a");
    {
        _builtin::print_int(2);
        let d = f("b");
        _builtin::print_int(3);
        let e = f("c");
        _builtin::print_int(4);
        g(d,e);
        _builtin::print_int(5);
        let f = c;
    }
    _builtin::print_int(6);
}
