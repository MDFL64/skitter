mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;

struct DropPair {
    a: LogDrop,
    b: LogDrop,
}

pub fn main() {
    {
        let a = LogDrop("1/a");

        let mut pair = DropPair{
            a,
            b: LogDrop("1/b"),
        };
    
        pair.a = LogDrop("2/a");
    }

    {
        let b = LogDrop("3/b");

        let mut pair = DropPair{
            a: LogDrop("3/a"),
            b,
        };

        pair = DropPair{
            a: LogDrop("4/a"),
            b: LogDrop("4/b"),
        }
    }
}
