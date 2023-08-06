use std::marker::PhantomData;

use crate::{
    items::CrateId,
    persist::{Persist, PersistReadContext, PersistWriteContext},
    vm::VM,
};

/// An array which is lazily parsed
pub struct LazyArray<'vm, T> {
    /// offset of every item in data
    item_indices: Vec<u32>,
    data: &'vm [u8],
    array_ty: PhantomData<T>,
}

// serialized form:
// entry count
// count * u32 entry offsets
// data length
// data

impl<'vm, T> LazyArray<'vm, T>
where
    T: Persist<'vm> + std::fmt::Debug + 'vm,
{
    pub fn write(out_ctx: &mut PersistWriteContext, items: impl ExactSizeIterator<Item = &'vm T>) {
        let mut data_ctx = PersistWriteContext::new(out_ctx.this_crate);

        let mut item_indices = Vec::<u32>::new();

        for item in items {
            let offset = data_ctx.offset();
            item.persist_write(&mut data_ctx);
            item_indices.push(offset);
        }

        let num_bytes = item_indices.len() * std::mem::size_of::<u32>();
        let index_bytes: &[u8] =
            unsafe { std::slice::from_raw_parts(item_indices.as_ptr() as _, num_bytes) };

        let data = data_ctx.flip();

        // write final result
        item_indices.len().persist_write(out_ctx);
        out_ctx.write_bytes(index_bytes);

        data.len().persist_write(out_ctx);
        out_ctx.write_bytes(&data);

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
}
