mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;

fn f(x: bool) {
    let mut a;
    if x {
        a = LogDrop("f1");
    }
    LogDrop("f2");
    a = LogDrop("f3");
    LogDrop("f4");
}

fn g() {
    let mut a = LogDrop("g1");
    let b = &mut a;
    *b = LogDrop("g2");
    a = LogDrop("g3");
}

pub fn main() {
    let mut a = LogDrop("main 1");
    a = LogDrop("main 2");

    f(true);
    f(false);

    g();
}
