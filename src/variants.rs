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
    list: Vec<(Discriminant, T)>,
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
    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn empty() -> Self {
        Self { list: vec![] }
    }

    pub fn iter(&self) -> impl Iterator<Item = (VariantIndex, Discriminant, &T)> {
        self.list
            .iter()
            .enumerate()
            .map(|(index, (disc, val))| (VariantIndex(index as u32), *disc, val))
    }

    pub fn from_iter(iter: impl Iterator<Item = (Discriminant, T)>) -> Self {
        Self {
            list: iter.collect(),
        }
    }

    pub fn assert_single(&self) -> &T {
        if self.list.len() == 1 {
            if let (Discriminant(None), ref val) = self.list[0] {
                return val;
            }
        }
        panic!("Variants::assert_single() failed - {}", self.list.len());
    }

    pub fn get(&self, key: VariantIndex) -> &T {
        &self.list[key.0 as usize].1
    }

    pub fn get_discriminant(&self, key: VariantIndex) -> Discriminant {
        self.list[key.0 as usize].0
    }

    pub fn for_discriminant(&self, key: Discriminant) -> &T {
        panic!("todo for discriminant");
    }

    pub fn index_for_discriminant(&self, key: Discriminant) -> VariantIndex {
        panic!("todo index for discriminant");
    }
}
