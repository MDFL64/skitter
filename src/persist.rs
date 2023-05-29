use crate::{items::CrateId, vm::VM};

#[derive(Default)]
pub struct PersistWriteContext {
    output: Vec<u8>,
}

impl PersistWriteContext {
    pub fn write_byte(&mut self, bytes: u8) {
        self.output.push(bytes);
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.output.extend_from_slice(bytes);
    }

    pub fn save(&self, file_name: &str) {
        std::fs::write(file_name, &self.output).expect("failed to write file");
    }
}

pub struct PersistReadContext<'vm> {
    pub vm: &'vm VM<'vm>,
    pub crate_id: CrateId,
    pub next_item_id: u32,

    index: usize,
    data: &'vm [u8],
}

impl<'vm> PersistReadContext<'vm> {
    pub fn new(data: &'vm [u8], vm: &'vm VM<'vm>, crate_id: CrateId) -> Self {
        Self {
            vm,
            crate_id,
            next_item_id: 0,

            index: 0,
            data,
        }
    }

    pub fn read_byte(&mut self) -> u8 {
        let n = self.data[self.index];
        self.index += 1;
        n
    }

    pub fn read_bytes(&mut self, n: usize) -> &'vm [u8] {
        let start = self.index;
        let end = start + n;
        self.index += n;
        &self.data[start..end]
    }
}

pub trait Persist<'vm> {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext);

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self;
}

macro_rules! persist_uint {
    ($ty:ty) => {
        impl<'vm> Persist<'vm> for $ty {
            fn persist_write(&self, write_ctx: &mut PersistWriteContext) {
                if *self < 252 {
                    write_ctx.write_byte(*self as u8);
                } else if *self < 65536 {
                    write_ctx.write_byte(252);
                    write_ctx.write_bytes(&(*self as u16).to_le_bytes());
                } else {
                    panic!("TOO FAT! {}", self);
                }
            }

            fn persist_read(read_ctx: &mut PersistReadContext) -> Self {
                let n = read_ctx.read_byte();
                match n {
                    255 | 254 | 253 => todo!(),
                    252 => {
                        let bs = read_ctx.read_bytes(2);
                        u16::from_le_bytes(bs.try_into().unwrap()) as _
                    }
                    _ => n as _,
                }
            }
        }
    };
}

persist_uint!(usize);
persist_uint!(u32);

impl<'vm> Persist<'vm> for &'vm str {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext) {
        self.len().persist_write(write_ctx);
        write_ctx.write_bytes(self.as_bytes());
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let len = usize::persist_read(read_ctx);
        let data = read_ctx.read_bytes(len);
        std::str::from_utf8(data).unwrap()
        //String::from_utf8(data.to_owned()).expect("deserialized bad string")
        //String::new()
    }
}

impl<'vm, T> Persist<'vm> for Vec<T>
where
    T: Persist<'vm>,
{
    fn persist_write(&self, write_ctx: &mut PersistWriteContext) {
        self.len().persist_write(write_ctx);
        for item in self {
            item.persist_write(write_ctx);
        }
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let len = usize::persist_read(read_ctx);
        let mut result = Vec::with_capacity(len);
        for _ in 0..len {
            result.push(T::persist_read(read_ctx));
        }
        result
    }
}

impl<'vm, T> Persist<'vm> for Option<T>
where
    T: Persist<'vm>,
{
    fn persist_write(&self, write_ctx: &mut PersistWriteContext) {
        match self {
            Option::None => {
                write_ctx.write_byte(0);
            }
            Option::Some(val) => {
                write_ctx.write_byte(1);
                val.persist_write(write_ctx);
            }
        }
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let is_present = read_ctx.read_byte() != 0;

        if is_present {
            Some(T::persist_read(read_ctx))
        } else {
            None
        }
    }
}
