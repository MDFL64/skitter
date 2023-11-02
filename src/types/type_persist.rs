use std::rc::Rc;

use crate::{
    lazy_collections::LazyItem,
    persist::{Persist, PersistReader, PersistWriteContext, PersistWriter},
};

use super::{ItemWithSubs, Type, TypeKind};

impl<'vm> LazyItem<'vm> for Type<'vm> {
    type Input = TypeKind<'vm>;

    fn build(kind: Self::Input, vm: &'vm crate::vm::VM<'vm>) -> Self {
        vm.types.intern(kind, vm)
    }
}

// cannot use derive: loads from lazy array
impl<'vm> Persist<'vm> for Type<'vm> {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        let type_id = self.0.persist_id.get_or_init(|| {
            let mut writer_types = writer.context.types.borrow_mut();

            let i = writer_types.len();
            writer_types.push(*self);
            i as u32
        });

        type_id.persist_write(writer);
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let type_id = u32::persist_read(reader);

        let types = reader.context.types.get().unwrap();

        types.get(type_id as usize).clone()
    }
}

// cannot use derive: item refs need special handling
impl<'vm> Persist<'vm> for ItemWithSubs<'vm> {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        writer.write_item_ref(self.item);

        self.subs.persist_write(writer);
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let item = reader.read_item_ref();

        let subs = Persist::persist_read(reader);

        ItemWithSubs { item, subs }
    }
}

pub struct WriterTypes<'vm> {
    context: Rc<PersistWriteContext<'vm>>,
    index: usize,
}

impl<'vm> WriterTypes<'vm> {
    pub fn new(context: Rc<PersistWriteContext<'vm>>) -> Self {
        Self { context, index: 0 }
    }
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
