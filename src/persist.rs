use std::{
    cell::RefCell,
    path::PathBuf,
    rc::Rc,
    sync::{Arc, OnceLock},
};

use ahash::AHashMap;

use crate::{
    items::{CrateId, Item, ItemId},
    lazy_collections::{LazyArray, LazyTable},
    types::{Type, WriterTypes},
    vm::VM,
};

pub struct PersistWriteContext<'vm> {
    pub this_crate: CrateId,
    pub types: RefCell<Vec<Type<'vm>>>,
}

impl<'vm> PersistWriteContext<'vm> {
    pub fn new(this_crate: CrateId) -> Self {
        Self {
            this_crate,
            types: Default::default(),
        }
    }
}

pub struct PersistWriter<'vm> {
    output: Vec<u8>,

    pub context: Rc<PersistWriteContext<'vm>>,
}

impl<'vm> PersistWriter<'vm> {
    pub fn new(context: Rc<PersistWriteContext<'vm>>) -> Self {
        Self {
            output: vec![],
            context,
        }
    }

    pub fn new_child_writer(&self) -> Self {
        Self {
            output: vec![],
            context: self.context.clone(),
        }
    }

    pub fn iter_types(&self) -> WriterTypes<'vm> {
        WriterTypes::new(self.context.clone())
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

    pub fn write_item_ref(&mut self, item: &Item<'vm>) {
        // Just write our crate and item IDs. The deserializer will remap these IDs.
        item.crate_id.index().persist_write(self);
        item.item_id.index().persist_write(self);
    }

    pub fn flip(&mut self) -> Vec<u8> {
        std::mem::take(&mut self.output)
    }

    pub fn save(&self, file_name: &str) {
        std::fs::write(file_name, &self.output).expect("failed to write file");
    }
}

pub struct PersistReadContext<'vm> {
    pub this_crate: CrateId,
    pub vm: &'vm VM<'vm>,
    pub types: OnceLock<LazyArray<'vm, Type<'vm>>>,
    pub items: OnceLock<LazyTable<'vm, &'vm Item<'vm>>>,
    pub crate_id_map: OnceLock<AHashMap<u32, CrateId>>,
}

pub struct PersistReader<'vm> {
    index: usize,
    data: &'vm [u8],

    pub context: Arc<PersistReadContext<'vm>>,
}

impl<'vm> PersistReader<'vm> {
    pub fn new(data: &'vm [u8], context: Arc<PersistReadContext<'vm>>) -> Self {
        Self {
            index: 0,
            data,
            context,
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

    pub fn read_byte_slice(&mut self) -> &'vm [u8] {
        let n = usize::persist_read(self);
        self.read_bytes(n)
    }

    pub fn read_str(&mut self) -> &'vm str {
        let data = self.read_byte_slice();
        std::str::from_utf8(data).unwrap()
    }

    pub unsafe fn read_raw_array<T>(&mut self, n: usize) -> Vec<T> {
        let num_bytes = n * std::mem::size_of::<T>();
        let bytes = self.read_bytes(num_bytes);

        let mut result = Vec::<T>::with_capacity(n);

        let src = bytes.as_ptr();
        let dst = result.as_mut_ptr() as *mut u8;

        unsafe {
            std::ptr::copy(src, dst, num_bytes);
        }

        result.set_len(n);

        result
    }

    pub fn read_item_ref(&mut self) -> &'vm Item<'vm> {
        let crate_id = CrateId::persist_read(self);
        let item_id = ItemId::persist_read(self);

        self.get_item_ref(item_id, crate_id)
    }

    pub fn get_item_ref(&self, item_id: ItemId, crate_id: CrateId) -> &'vm Item<'vm> {
        // either look up the item in our local table, or request it from another crate
        if crate_id == self.context.this_crate {
            let items = self.context.items.get().unwrap();
            *items.array.get(item_id.index())
        } else {
            let provider = self.context.vm.crate_provider(crate_id);
            provider.item_by_id(item_id)
        }
    }

    pub fn reset(&mut self, data: &'vm [u8]) {
        self.data = data;
        self.index = 0;
    }
}

pub trait Persist<'vm> {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>);

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self;
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
                    writer.write_bytes(&(n as u64).to_le_bytes());
                } else {
                    writer.write_byte(255);
                    writer.write_bytes(&(n as u128).to_le_bytes());
                }
            }

            fn persist_read(reader: &mut PersistReader) -> Self {
                let n = reader.read_byte();
                match n {
                    255 => {
                        let bs = reader.read_bytes(16);
                        u128::from_le_bytes(bs.try_into().unwrap()) as _
                    }
                    254 => {
                        let bs = reader.read_bytes(8);
                        u64::from_le_bytes(bs.try_into().unwrap()) as _
                    }
                    253 => {
                        let bs = reader.read_bytes(4);
                        u32::from_le_bytes(bs.try_into().unwrap()) as _
                    }
                    252 => {
                        let bs = reader.read_bytes(2);
                        u16::from_le_bytes(bs.try_into().unwrap()) as _
                    }
                    251 => {
                        let b = reader.read_byte();
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

            fn persist_read(reader: &mut PersistReader) -> Self {
                let n = <$uty>::persist_read(reader);
                let neg = (n & 1) != 0;
                if neg {
                    let tmp = (n >> 1) as $ty;
                    -tmp
                } else {
                    (n >> 1) as _
                }
            }
        }
    };
}

persist_uint!(u128);
persist_uint!(u64);
persist_uint!(u32);
persist_uint!(u16);
persist_uint!(usize);

persist_sint!(i128, u128);

impl<'vm> Persist<'vm> for bool {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        writer.write_byte(*self as u8);
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        reader.read_byte() != 0
    }
}

impl<'vm> Persist<'vm> for String {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        writer.write_str(&self);
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        reader.read_str().to_owned()
    }
}

impl<'vm> Persist<'vm> for &'vm str {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        writer.write_str(&self);
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        reader.read_str()
    }
}

impl<'vm> Persist<'vm> for &'vm [u8] {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        writer.write_byte_slice(&self);
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        reader.read_byte_slice()
    }
}

#[cfg(windows)]
impl<'vm> Persist<'vm> for PathBuf {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        use std::os::windows::ffi::OsStrExt;

        let code_points: Vec<_> = self.as_os_str().encode_wide().collect();
        code_points.persist_write(writer);
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        use std::ffi::OsString;
        use std::os::windows::ffi::OsStringExt;

        let code_points = Vec::<u16>::persist_read(reader);

        OsString::from_wide(&code_points).into()
    }
}

#[cfg(unix)]
impl<'vm> Persist<'vm> for PathBuf {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        use std::os::unix::ffi::OsStrExt;

        writer.write_byte_slice(self.as_os_str().as_bytes());
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        use std::ffi::OsString;
        use std::os::unix::ffi::OsStringExt;

        let bytes = reader.read_byte_slice().to_owned();

        OsString::from_vec(bytes).into()
    }
}

impl<'vm, T> Persist<'vm> for Arc<T>
where
    T: Persist<'vm>,
{
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.as_ref().persist_write(writer);
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        T::persist_read(reader).into()
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

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let len = usize::persist_read(reader);
        let mut result = Vec::with_capacity(len);
        for _ in 0..len {
            result.push(T::persist_read(reader));
        }
        result
    }
}

impl<'vm, T> Persist<'vm> for Arc<[T]>
where
    T: Persist<'vm>,
{
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.len().persist_write(writer);
        for item in self.iter() {
            item.persist_write(writer);
        }
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let len = usize::persist_read(reader);
        let mut result = Vec::with_capacity(len);
        for _ in 0..len {
            result.push(T::persist_read(reader));
        }
        result.into()
    }
}

impl<'vm, K, V> Persist<'vm> for AHashMap<K, V>
where
    K: Persist<'vm> + std::hash::Hash + Eq,
    V: Persist<'vm>,
{
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.len().persist_write(writer);
        for (k, v) in self {
            k.persist_write(writer);
            v.persist_write(writer);
        }
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let len = usize::persist_read(reader);
        let mut result = AHashMap::with_capacity(len);
        for _ in 0..len {
            let key = K::persist_read(reader);
            let val = V::persist_read(reader);

            result.insert(key, val);
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

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let is_present = reader.read_byte() != 0;

        if is_present {
            Some(T::persist_read(reader))
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

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let a = A::persist_read(reader);
        let b = B::persist_read(reader);
        (a, b)
    }
}
