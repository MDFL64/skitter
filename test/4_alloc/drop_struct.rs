mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;

struct DropPair {
    a: LogDrop,
    b: LogDrop,
}

pub fn main() {
    let pair = DropPair{
        a: LogDrop("1/a"),
        b: LogDrop("1/b"),
    };
}
