mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;

enum OptionDrop {
    Pair(LogDrop,LogDrop),
    Single(LogDrop),
    None
}

impl Drop for OptionDrop {
    fn drop(&mut self) {
        _builtin::print_raw("dropping option\n");
    }
}

pub fn main() {
    {
        let a = OptionDrop::None;
        let b = OptionDrop::Pair(LogDrop("1/a"),LogDrop("1/b"));
    }
    {
        let mut a = OptionDrop::None;
        let mut b = OptionDrop::Pair(LogDrop("2/a"),LogDrop("2/b"));
        a = OptionDrop::Pair(LogDrop("3/a"),LogDrop("3/b"));
        b = OptionDrop::Pair(LogDrop("4/a"),LogDrop("4/b"));
    }

    /*{
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
    }*/
}
