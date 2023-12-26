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

    for x in 0..=255u8 {
        for y in 0..=255u8 {
            write!(&mut w,"u {} + {} = {:?}\n",x,y,x.overflowing_add(y));
        }
    }

    // interpreter is slow, running this whole thing is annoying
    /*for x in -128..=127i8 {
        for y in -128..=127i8 {
            write!(&mut w,"s {} + {} = {:?}\n",x,y,x.overflowing_add(y));
        }
    }

    for x in 0..=255u8 {
        for y in 0..=255u8 {
            write!(&mut w,"u {} - {} = {:?}\n",x,y,x.overflowing_sub(y));
        }
    }
    for x in -128..=127i8 {
        for y in -128..=127i8 {
            write!(&mut w,"s {} - {} = {:?}\n",x,y,x.overflowing_sub(y));
        }
    }

    for x in 0..=255u8 {
        for y in 0..=255u8 {
            write!(&mut w,"u {} * {} = {:?}\n",x,y,x.overflowing_mul(y));
        }
    }
    for x in -128..=127i8 {
        for y in -128..=127i8 {
            write!(&mut w,"s {} * {} = {:?}\n",x,y,x.overflowing_mul(y));
        }
    }*/
}
