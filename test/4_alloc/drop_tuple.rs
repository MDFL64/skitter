mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;

pub fn main() {
    {
        let a = LogDrop("1/a");

        let mut pair = (a,LogDrop("1/b"));
    
        pair.0 = LogDrop("2/a");
    }

    {
        let b = LogDrop("3/b");

        let mut pair = (LogDrop("3/a"),b);

        pair = (LogDrop("4/a"),LogDrop("4/b"));
    }
}
