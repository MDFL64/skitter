use skitter_macro::Persist;

use crate::persist::Persist;

#[derive(Copy, Clone, Eq, PartialEq, Persist, Debug)]
pub struct VariantIndex(u32);

impl VariantIndex {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(&self) -> u32 {
        self.0
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Persist, Debug)]
pub struct Discriminant(Option<i128>);

impl Discriminant {
    pub const NONE: Self = Discriminant(None);

    pub fn new(val: i128) -> Self {
        Self(Some(val))
    }

    pub fn value(&self) -> Option<i128> {
        self.0
    }
}

#[derive(Clone, Debug)]
pub struct Variants<T> {
    list: Vec<T>,
}

impl<'vm, T> Persist<'vm> for Variants<T>
where
    T: Persist<'vm>,
{
    fn persist_read(reader: &mut crate::persist::PersistReader<'vm>) -> Self {
        Self {
            list: Persist::persist_read(reader),
        }
    }

    fn persist_write(&self, writer: &mut crate::persist::PersistWriter<'vm>) {
        self.persist_write(writer);
    }
}

impl<T> Variants<T> {
    pub fn new(list: Vec<T>) -> Self {
        Self { list }
    }

    pub fn empty() -> Self {
        Self { list: vec![] }
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = (VariantIndex, &T)> {
        self.list
            .iter()
            .enumerate()
            .map(|(i, x)| (VariantIndex(i as u32), x))
    }

    pub fn assert_single(&self) -> &T {
        if self.list.len() == 1 {
            return &self.list[0];
        }
        panic!("Variants::assert_single() failed - {}", self.list.len());
    }

    pub fn get(&self, key: VariantIndex) -> &T {
        &self.list[key.0 as usize]
    }
}