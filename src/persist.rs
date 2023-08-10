use std::{cell::RefCell, rc::Rc};

use crate::{items::CrateId, vm::VM, types::{Type, TypeKind}};

pub struct PersistWriteContext<'vm> {
    pub this_crate: CrateId,
    pub types: RefCell<Vec<Type<'vm>>>,
}

pub struct PersistWriter<'vm> {
    output: Vec<u8>,

    pub context: Rc<PersistWriteContext<'vm>>
}

impl<'vm> PersistWriter<'vm> {
    pub fn new(this_crate: CrateId) -> Self {
        Self {
            output: vec![],
            context: Rc::new(PersistWriteContext{
                this_crate,
                types: Default::default()
            })
        }
    }

    pub fn new_child_context(&self) -> Self {
        Self {
            output: vec![],
            context: self.context.clone()
        }
    }

    pub fn iter_types(&self) -> WriterTypes<'vm> {
        WriterTypes{
            index: 0,
            context: self.context.clone()
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

    pub fn write_byte_slice(&mut self, bytes: &[u8]) {
        bytes.len().persist_write(self);
        self.write_bytes(bytes);
    }

    pub fn write_str(&mut self, string: &str) {
        self.write_byte_slice(string.as_bytes());
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

    pub fn reset(&mut self, data: &'vm [u8]) {
        self.data = data;
        self.index = 0;
    }
}

pub trait Persist<'vm> {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>);

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self;
}

macro_rules! persist_uint {
    ($ty:ty) => {
        impl<'vm> Persist<'vm> for $ty {
            fn persist_write(&self, writer: &mut PersistWriter) {
                let n = *self as u128;
                if n < 251 {
                    writer.write_byte(n as u8);
                } else if n < 256 {
                    writer.write_byte(251);
                    writer.write_byte(n as u8);
                } else if n < 65536 {
                    writer.write_byte(252);
                    writer.write_bytes(&(n as u16).to_le_bytes());
                } else if n < 4294967296 {
                    writer.write_byte(253);
                    writer.write_bytes(&(n as u32).to_le_bytes());
                } else if n < 18446744073709551616 {
                    writer.write_byte(254);
                    writer.write_bytes(&(n as u32).to_le_bytes());
                } else {
                    writer.write_byte(255);
                    writer.write_bytes(&(n as u64).to_le_bytes());
                }
            }

            fn persist_read(read_ctx: &mut PersistReadContext) -> Self {
                let n = read_ctx.read_byte();
                match n {
                    255 | 254 => panic!(),
                    253 => {
                        let bs = read_ctx.read_bytes(4);
                        u32::from_le_bytes(bs.try_into().unwrap()) as _
                    }
                    252 => {
                        let bs = read_ctx.read_bytes(2);
                        u16::from_le_bytes(bs.try_into().unwrap()) as _
                    }
                    251 => {
                        let b = read_ctx.read_byte();
                        b as _
                    }
                    _ => n as _,
                }
            }
        }
    };
}

macro_rules! persist_sint {
    ($ty:ty,$uty:ty) => {
        impl<'vm> Persist<'vm> for $ty {
            fn persist_write(&self, writer: &mut PersistWriter) {
                let n = if *self < 0 {
                    ((-self) << 1) | 1
                } else {
                    *self << 1
                };
                (n as $uty).persist_write(writer);
            }

            fn persist_read(read_ctx: &mut PersistReadContext) -> Self {
                panic!();
            }
        }
    };
}

persist_uint!(u128);
persist_uint!(u64);
persist_uint!(u32);
persist_uint!(usize);

persist_sint!(i128,u128);

impl<'vm> Persist<'vm> for bool {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        writer.write_byte(*self as u8);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        read_ctx.read_byte() != 0
    }
}

impl<'vm> Persist<'vm> for String {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        writer.write_str(&self);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        read_ctx.read_str().to_owned()
    }
}

impl<'vm, T> Persist<'vm> for Vec<T>
where
    T: Persist<'vm>,
{
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.len().persist_write(writer);
        for item in self {
            item.persist_write(writer);
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
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        match self {
            Option::None => {
                writer.write_byte(0);
            }
            Option::Some(val) => {
                writer.write_byte(1);
                val.persist_write(writer);
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
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.0.persist_write(writer);
        self.1.persist_write(writer);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let a = A::persist_read(read_ctx);
        let b = B::persist_read(read_ctx);
        (a, b)
    }
}

pub struct WriterTypes<'vm> {
    context: Rc<PersistWriteContext<'vm>>,
    index: usize
}

impl<'vm> Iterator for WriterTypes<'vm> {
    type Item = &'vm TypeKind<'vm>;

    fn next(&mut self) -> Option<Self::Item> {
        let types = self.context.types.borrow();
        let ty = types.get(self.index).copied();
        self.index += 1;
        ty.map(|ty| ty.kind())
    }
}
