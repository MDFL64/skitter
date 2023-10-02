use std::fmt::Write;

mod _builtin;

struct SimpleWriter;

impl Write for SimpleWriter {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        _builtin::print_raw(s);

        Ok(())
    }
}

pub fn main() {
    let mut w = SimpleWriter;

    write!(&mut w,"test\n");
    //write!(&mut w,"alpha {}\n","beta");
}
