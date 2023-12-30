mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;

enum OptionDrop {
    Pair(LogDrop,LogDrop),
    Single(LogDrop),
    None
}

pub fn main() {
    {
        let a = OptionDrop::None;
        let b = OptionDrop::Pair(LogDrop("1/a"),LogDrop("1/b"));
    }
    {
        let mut a = OptionDrop::None;
        let mut b = OptionDrop::Pair(LogDrop("2/a"),LogDrop("2/b"));
        a = OptionDrop::Single(LogDrop("3/a"));
        b = OptionDrop::Pair(LogDrop("4/a"),LogDrop("4/b"));
        a = OptionDrop::None;
        b = OptionDrop::Pair(LogDrop("5/a"),LogDrop("5/b"));
    }
}
