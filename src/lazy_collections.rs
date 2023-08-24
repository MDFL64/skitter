use std::{
    hash::{BuildHasher, Hash, Hasher},
    marker::PhantomData,
    sync::{Arc, OnceLock},
};

use ahash::AHasher;

use crate::{
    persist::{Persist, PersistReadContext, PersistReader, PersistWriter},
    vm::VM,
};

/// Items stored in lazy arrays generally need interned in the VM. This trait handles that process.
pub trait LazyItem<'vm> {
    type Input;

    fn input(&self) -> &Self::Input;

    fn build(input: Self::Input, vm: &'vm VM<'vm>) -> Self;
}

pub trait LazyKey<'vm>: LazyItem<'vm> {
    type Key;

    fn key(input: &Self::Input) -> Option<&Self::Key>;
}

/// An array which is lazily parsed
pub struct LazyArray<'vm, T> {
    /// offset of every item in data
    item_indices: Vec<u32>,
    data: &'vm [u8],
    array_ty: PhantomData<T>,
    loaded: Vec<OnceLock<T>>,
    read_context: Arc<PersistReadContext<'vm>>,
}

// SERIALIZED FORM
// ---------------
// u32: entry count
// [u32;count]: entry offsets
// u32: data length
// [u8;length]: data

impl<'vm, T> LazyArray<'vm, T>
where
    T: LazyItem<'vm>,
    T::Input: Persist<'vm> + std::fmt::Debug + 'vm,
{
    /// Returns the item count.
    pub fn write<'a>(out_writer: &mut PersistWriter<'vm>, items: impl Iterator<Item = &'a T::Input>)
    where
        'vm: 'a,
    {
        let mut data_writer = out_writer.new_child_context();

        let mut item_indices = Vec::<u32>::new();

        for item in items {
            let offset = data_writer.offset();
            item.persist_write(&mut data_writer);
            item_indices.push(offset);
        }

        let num_bytes = item_indices.len() * std::mem::size_of::<u32>();
        let index_bytes: &[u8] =
            unsafe { std::slice::from_raw_parts(item_indices.as_ptr() as _, num_bytes) };

        let data = data_writer.flip();

        // write final result
        item_indices.len().persist_write(out_writer);
        out_writer.write_bytes(index_bytes);

        data.len().persist_write(out_writer);
        out_writer.write_bytes(&data);
    }

    pub fn read(reader: &mut PersistReader<'vm>) -> Self {
        let indices_len = usize::persist_read(reader);
        let item_indices = unsafe { reader.read_raw_array(indices_len) };

        let data_len = usize::persist_read(reader);
        let data = reader.read_bytes(data_len);

        let loaded = item_indices.iter().map(|_| OnceLock::new()).collect();

        LazyArray {
            item_indices,
            data,
            array_ty: PhantomData,
            loaded,
            read_context: reader.context.clone(),
        }
    }

    pub fn len(&self) -> usize {
        self.item_indices.len()
    }

    pub fn get(&self, index: usize) -> &T {
        self.loaded[index].get_or_init(|| {
            let start_index = self.item_indices[index] as usize;
            let data = &self.data[start_index..];

            let mut reader = PersistReader::new(data, self.read_context.clone());

            let input = T::Input::persist_read(&mut reader);
            LazyItem::build(input, self.read_context.vm)
        })
    }
}

/// A perfect hash table which is lazily parsed
pub struct LazyTable<'vm, T> {
    pub array: LazyArray<'vm, T>,
    // primary hashes to indices (positive) OR hash seeds (negative)
    table_1: Vec<i32>,
    // secondary hashes to indices
    table_2: Vec<i32>,
}

// SERIALIZED FORM
// ---------------
// LazyArray: keys and values
// int: table size (may be smaller than array)
// [i32;count]: table 1
// [i32;count]: table 2

const TABLE_SLOT_INVALID: i32 = std::i32::MAX;

impl<'vm, T> LazyTable<'vm, T>
where
    T: LazyItem<'vm> + LazyKey<'vm>,
    T::Input: Persist<'vm> + std::fmt::Debug + 'vm,
    T::Key: Hash + Eq + std::fmt::Debug,
{
    pub fn write<'a>(out_writer: &mut PersistWriter<'vm>, items: impl Iterator<Item = &'a T::Input>)
    where
        'vm: 'a,
    {
        let mut keys = Vec::new();

        let items = items.enumerate().map(|(i, item)| {
            let key = <T as LazyKey>::key(item);
            if let Some(key) = key {
                keys.push((key, i));
            }

            item
        });

        LazyArray::<T>::write(out_writer, items);

        let table_size = keys.len();
        table_size.persist_write(out_writer);

        // determine which bucket each entry will fall in
        let mut buckets = vec![vec!(); table_size];

        for (key, item_index) in keys {
            let mut hasher = get_hasher(0);
            key.hash(&mut hasher);
            let bucket_index = hasher.finish() as usize % table_size;
            buckets[bucket_index].push((key, item_index));
        }

        // sort buckets, most full come first
        let mut buckets_sorted: Vec<_> = buckets.iter().enumerate().collect();
        buckets_sorted.sort_by_key(|(_, bucket)| -(bucket.len() as isize));

        let mut table_1 = vec![TABLE_SLOT_INVALID; table_size];
        let mut table_2 = vec![TABLE_SLOT_INVALID; table_size];

        for (bucket_i, bucket_keys) in buckets_sorted {
            if bucket_keys.len() == 1 {
                let item_index = bucket_keys[0].1;
                table_1[bucket_i] = item_index as i32;
                println!("> {:?} t1[{}]={}",bucket_keys[0].0,bucket_i,item_index);
            } else if bucket_keys.len() > 1 {
                let mut seed = -1;
                loop {
                    let mut new_indices = Vec::with_capacity(bucket_keys.len());

                    for (key, item_index) in bucket_keys {
                        let mut hasher = get_hasher(seed);
                        key.hash(&mut hasher);
                        let hash_index = hasher.finish() as usize % table_size;

                        if table_2[hash_index] != TABLE_SLOT_INVALID
                            || new_indices.iter().any(|(x, _, _)| *x == hash_index)
                        {
                            break;
                        }
                        new_indices.push((hash_index, item_index, key));
                    }

                    if new_indices.len() == bucket_keys.len() {
                        for (hash_index, item_index, key) in new_indices {
                            table_2[hash_index] = *item_index as i32;
                            println!("> {:?} t1[{}]={} t2[{}]={}",key,bucket_i,seed,hash_index,item_index);
                        }
                        table_1[bucket_i] = seed;
                        break;
                    }

                    seed -= 1;
                }
            }
        }

        let num_bytes = table_size * std::mem::size_of::<i32>();

        let t1_bytes: &[u8] =
            unsafe { std::slice::from_raw_parts(table_1.as_ptr() as _, num_bytes) };
        let t2_bytes: &[u8] =
            unsafe { std::slice::from_raw_parts(table_2.as_ptr() as _, num_bytes) };

        out_writer.write_bytes(t1_bytes);
        out_writer.write_bytes(t2_bytes);
    }

    pub fn read(reader: &mut PersistReader<'vm>) -> Self {
        let array = LazyArray::read(reader);

        let table_len = usize::persist_read(reader);

        let table_1 = unsafe { reader.read_raw_array(table_len) };
        let table_2 = unsafe { reader.read_raw_array(table_len) };

        Self {
            array,
            table_1,
            table_2,
        }
    }

    pub fn get(&self, key: &T::Key) -> &T {
        let t1_index = {
            let mut hasher = get_hasher(0);
            key.hash(&mut hasher);
            self.table_1[hasher.finish() as usize % self.table_1.len()]
        };
        //println!("get {:?} {}",key,t1_index);

        let final_index = if t1_index >= 0 {
            // fast path
            t1_index as usize
        } else {
            // slow path, seed a secondary hasher
            let t2_index = {
                let mut hasher = get_hasher(t1_index);
                key.hash(&mut hasher);
                self.table_2[hasher.finish() as usize % self.table_2.len()]
            };

            t2_index as usize
        };

        let item = self.array.get(final_index);

        let item_key = <T as LazyKey>::key(item.input());
        assert!(item_key == Some(key));

        item
    }
}

fn get_hasher(seed: i32) -> AHasher {
    ahash::RandomState::with_seeds(seed as _, 0, 0, 0).build_hasher()
}
