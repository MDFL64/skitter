mod _builtin;
mod _log_drop;

use _log_drop::LogDrop;

pub fn main() {
    {
        let a = LogDrop("1/a");
        let mut list = [a.clone(),LogDrop("1/b"),a.clone(),a.clone(),LogDrop("1/c"),LogDrop("1/d")];
    
        list[2] = LogDrop("2/a");
    }

    {
        let b = LogDrop("3/a");
        let mut list = [b,LogDrop("3/b"),LogDrop("3/c")];

        list = [LogDrop("4/a"),LogDrop("4/b"),LogDrop("4/c")];
    }
}
