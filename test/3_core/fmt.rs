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
    write!(&mut w,"alpha {}\n","beta");
    let x: i32 = 100;
    write!(&mut w,"int {}\n",x);
    let y: f32 = 5.12;
    write!(&mut w,"float {}\n",y);
}
