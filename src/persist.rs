use crate::{items::CrateId, types::Type, vm::VM};

pub struct PersistWriteContext<'vm> {
    output: Vec<u8>,
    stack: Vec<Vec<u8>>,

    pub this_crate: CrateId,
    pub types: Vec<Type<'vm>>,
}

impl<'vm> PersistWriteContext<'vm> {
    pub fn new(this_crate: CrateId) -> Self {
        Self {
            output: vec![],
            stack: vec![],
            this_crate,
            types: vec![],
        }
    }

    pub fn offset(&self) -> u32 {
        self.output.len() as u32
    }

    pub fn write_byte(&mut self, bytes: u8) {
        self.output.push(bytes);
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.output.extend_from_slice(bytes);
    }

    pub fn write_str(&mut self, string: &str) {
        string.len().persist_write(self);
        self.write_bytes(string.as_bytes());
    }

    /// Start a nested sub-block that can be skipped over and parsed later.
    pub fn push_writer(&mut self) {
        let parent = std::mem::take(&mut self.output);
        self.stack.push(parent);
    }

    pub fn pop_writer(&mut self) -> Vec<u8> {
        let parent = self.stack.pop().unwrap();

        let child = std::mem::replace(&mut self.output, parent);
        child
    }

    pub fn flip(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.output)
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

    pub fn read_str(&mut self) -> &'vm str {
        let len = usize::persist_read(self);
        let data = self.read_bytes(len);
        std::str::from_utf8(data).unwrap()
    }

    pub fn read_block(&mut self) -> &'vm [u8] {
        let len = usize::persist_read(self);
        self.read_bytes(len)
    }

    pub fn reset(&mut self, data: &'vm [u8]) {
        self.data = data;
        self.index = 0;
    }
}

pub trait Persist<'vm> {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>);

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self;
}

macro_rules! persist_uint {
    ($ty:ty) => {
        impl<'vm> Persist<'vm> for $ty {
            fn persist_write(&self, write_ctx: &mut PersistWriteContext) {
                let n = *self as u64;
                if n < 252 {
                    write_ctx.write_byte(n as u8);
                } else if n < 256 {
                    write_ctx.write_byte(252);
                    write_ctx.write_byte(n as u8);
                } else if n < 65536 {
                    write_ctx.write_byte(253);
                    write_ctx.write_bytes(&(n as u16).to_le_bytes());
                } else if n < 4294967296 {
                    write_ctx.write_byte(254);
                    write_ctx.write_bytes(&(n as u32).to_le_bytes());
                } else {
                    panic!("TOO FAT! {}", self);
                }
            }

            fn persist_read(read_ctx: &mut PersistReadContext) -> Self {
                let n = read_ctx.read_byte();
                match n {
                    255 |
                    254 => {
                        let bs = read_ctx.read_bytes(4);
                        u32::from_le_bytes(bs.try_into().unwrap()) as _
                    }
                    253 => {
                        let bs = read_ctx.read_bytes(2);
                        u16::from_le_bytes(bs.try_into().unwrap()) as _
                    }
                    252 => {
                        let b = read_ctx.read_byte();
                        b as _
                    }
                    _ => n as _,
                }
            }
        }
    };
}

persist_uint!(usize);
persist_uint!(u32);

impl<'vm, T> Persist<'vm> for Vec<T>
where
    T: Persist<'vm>,
{
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
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
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
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

impl<'vm, A, B> Persist<'vm> for (A, B)
where
    A: Persist<'vm>,
    B: Persist<'vm>,
{
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.0.persist_write(write_ctx);
        self.1.persist_write(write_ctx);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let a = A::persist_read(read_ctx);
        let b = B::persist_read(read_ctx);
        (a, b)
    }
}
