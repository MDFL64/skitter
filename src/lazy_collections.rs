use std::marker::PhantomData;

use crate::{
    items::CrateId,
    persist::{Persist, PersistReadContext, PersistWriteContext},
    vm::VM,
};

/// A sorted array which is lazily parsed.
pub struct LazyArray<'vm, T> {
    /// offset of every item in data
    item_indices: Vec<u32>,
    data: &'vm [u8],
    array_ty: PhantomData<T>,
}

impl<'vm, T> LazyArray<'vm, T>
where
    T: Persist<'vm> + Ord + std::fmt::Debug,
{
    /// Input must already be sorted!
    pub fn write(this_crate: CrateId, items: Vec<T>) -> Vec<u8> {
        let mut write_ctx = PersistWriteContext::new(this_crate);

        let mut item_indices = Vec::<u32>::new();

        for item in &items {
            let offset = write_ctx.offset();
            item.persist_write(&mut write_ctx);
            item_indices.push(offset);
        }

        let num_bytes = item_indices.len() * std::mem::size_of::<u32>();
        let index_bytes: &[u8] =
            unsafe { std::slice::from_raw_parts(item_indices.as_ptr() as _, num_bytes) };

        let data = write_ctx.flip();

        // write final result
        item_indices.len().persist_write(&mut write_ctx);
        write_ctx.write_bytes(index_bytes);

        data.len().persist_write(&mut write_ctx);
        write_ctx.write_bytes(&data);

        write_ctx.flip()
    }

    pub fn read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let indices_len = usize::persist_read(read_ctx);

        let num_bytes = indices_len * std::mem::size_of::<u32>();
        let index_bytes = read_ctx.read_bytes(indices_len * 4);

        let data_len = usize::persist_read(read_ctx);
        let data = read_ctx.read_bytes(data_len);

        let mut item_indices = Vec::<u32>::with_capacity(indices_len);
        item_indices.resize(indices_len, 0);

        let src = index_bytes.as_ptr();
        let dst = item_indices.as_mut_ptr() as *mut u8;

        unsafe {
            std::ptr::copy(src, dst, num_bytes);
        }

        LazyArray {
            item_indices,
            data,
            array_ty: PhantomData,
        }
    }

    pub fn find(&self, val: &T, vm: &'vm VM<'vm>) -> usize {
        let mut read_ctx = PersistReadContext::new(&[], vm, CrateId::new(0));

        self.item_indices
            .binary_search_by(|index| {
                let data = &self.data[*index as usize..];
                read_ctx.reset(data);

                let candidate = T::persist_read(&mut read_ctx);

                candidate.cmp(val)
            })
            .unwrap()
    }
}
