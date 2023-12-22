mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;

struct DropPair {
    a: LogDrop,
    b: LogDrop,
}

impl Drop for DropPair {
    fn drop(&mut self) {
        _builtin::print_raw("dropping pair\n");    
    }
}

pub fn main() {
    {
        let mut pair = DropPair{
            a: LogDrop("1/a"),
            b: LogDrop("1/b"),
        };
    
        pair.a = LogDrop("2/a");
    }

    /*{
        let mut pair = DropPair{
            a: LogDrop("3/a"),
            b: LogDrop("3/b"),
        };

        pair = DropPair{
            a: LogDrop("4/a"),
            b: LogDrop("4/b"),
        }
    }*/
}
