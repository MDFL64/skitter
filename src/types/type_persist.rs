use crate::{persist::{Persist, PersistReadContext, PersistWriter}, types::{IntWidth, IntSign}};

use super::{
    ArraySize, ItemWithSubs, Mutability, Sub, SubList, Type,
    TypeKind, FloatWidth,
};

const TYPE_ID_OFFSET: usize = 100;

impl<'vm> Persist<'vm> for Type<'vm> {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        let type_id = self.0.persist_id.get_or_init(|| {
            //prepare_child_types(self.kind(), writer);
            let mut writer_types = writer.context.types.borrow_mut();

            let i = TYPE_ID_OFFSET + writer_types.len();
            writer_types.push(*self);
            i as u32
        });

        type_id.persist_write(writer);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        todo!()
    }
}

impl<'vm> Persist<'vm> for TypeKind<'vm> {


    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        match self {
            TypeKind::Int(IntWidth::I8, IntSign::Signed) => {
                writer.write_byte(0);
            }
            TypeKind::Int(IntWidth::I16, IntSign::Signed) => {
                writer.write_byte(1);
            }
            TypeKind::Int(IntWidth::I32, IntSign::Signed) => {
                writer.write_byte(2);
            }
            TypeKind::Int(IntWidth::I64, IntSign::Signed) => {
                writer.write_byte(3);
            }
            TypeKind::Int(IntWidth::I128, IntSign::Signed) => {
                writer.write_byte(4);
            }
            TypeKind::Int(IntWidth::ISize, IntSign::Signed) => {
                writer.write_byte(5);
            }
            TypeKind::Int(IntWidth::I8, IntSign::Unsigned) => {
                writer.write_byte(6);
            }
            TypeKind::Int(IntWidth::I16, IntSign::Unsigned) => {
                writer.write_byte(7);
            }
            TypeKind::Int(IntWidth::I32, IntSign::Unsigned) => {
                writer.write_byte(8);
            }
            TypeKind::Int(IntWidth::I64, IntSign::Unsigned) => {
                writer.write_byte(9);
            }
            TypeKind::Int(IntWidth::I128, IntSign::Unsigned) => {
                writer.write_byte(10);
            }
            TypeKind::Int(IntWidth::ISize, IntSign::Unsigned) => {
                writer.write_byte(11);
            }
            TypeKind::Float(FloatWidth::F32) => {
                writer.write_byte(12);
            }
            TypeKind::Float(FloatWidth::F64) => {
                writer.write_byte(13);
            }
            TypeKind::Bool => {
                writer.write_byte(14);
            }
            TypeKind::Char => {
                writer.write_byte(15);
            }
            TypeKind::Never => {
                writer.write_byte(16);
            }

            TypeKind::Ref(child, Mutability::Const) => {
                writer.write_byte(20);
                child.persist_write(writer);
            }
            TypeKind::Ref(child, Mutability::Mut) => {
                writer.write_byte(21);
                child.persist_write(writer);
            }

            TypeKind::Tuple(children) => {
                writer.write_byte(22);
                children.persist_write(writer);
            }

            TypeKind::FunctionDef(item_with_subs) => {
                writer.write_byte(30);
                item_with_subs.persist_write(writer);
            }

            /*
            TypeKind::Ptr(child, Mutability::Const) => {
                writer.write_byte(18);
                child.persist_write(writer);
            }
            TypeKind::Ptr(child, Mutability::Mut) => {
                writer.write_byte(19);
                child.persist_write(writer);
            }
            TypeKind::Array(child, ArraySize::Static(n)) => {
                writer.write_byte(20);
                child.persist_write(writer);
                n.persist_write(writer);
            }
            TypeKind::Array(child, ArraySize::ConstParam(n)) => {
                writer.write_byte(21);
                child.persist_write(writer);
                // TODO
            }
            TypeKind::Slice(child) => {
                writer.write_byte(22);
                child.persist_write(writer);
            }
            TypeKind::StringSlice => {
                writer.write_byte(23);
            }
            TypeKind::Tuple(children) => {
                writer.write_byte(24);
                children.persist_write(writer);
            }
            TypeKind::Adt(item_with_subs) => {
                writer.write_byte(25);
                item_with_subs.persist_write(writer);
            }
            TypeKind::FunctionDef(item_with_subs) => {
                writer.write_byte(26);
                item_with_subs.persist_write(writer);
            }
            TypeKind::AssociatedType(item_with_subs) => {
                writer.write_byte(27);
                item_with_subs.persist_write(writer);
            }
            TypeKind::Param(n) => {
                writer.write_byte(28);
                n.persist_write(writer);
            }
            TypeKind::Never => {
                writer.write_byte(29);
            }
            TypeKind::Foreign => {
                writer.write_byte(30);
            }
            TypeKind::Dynamic => {
                writer.write_byte(31);
            }
            TypeKind::FunctionPointer => {
                writer.write_byte(32);
            }*/

            _ => panic!("write type {:?}", self),
        }
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        todo!()
    }
}

impl<'vm> Persist<'vm> for Sub<'vm> {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        match self {
            Sub::Type(ty) => {
                writer.write_byte('T' as _);
                ty.persist_write(writer);
            }
            Sub::Const => {
                writer.write_byte('C' as _);
            }
            Sub::Lifetime => {
                writer.write_byte('L' as _);
            }
        }
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        let c = read_ctx.read_byte() as char;
        match c {
            'T' => Sub::Type(Type::persist_read(read_ctx)),
            'C' => Sub::Const,
            'L' => Sub::Lifetime,
            _ => panic!(),
        }
    }
}

impl<'vm> Persist<'vm> for SubList<'vm> {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        self.list.persist_write(writer);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        SubList {
            list: Vec::<Sub>::persist_read(read_ctx),
        }
    }
}

impl<'vm> Persist<'vm> for ItemWithSubs<'vm> {
    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        if self.item.crate_id == writer.context.this_crate {
            writer.write_byte(0);
            self.item.item_id.index().persist_write(writer);
        } else {
            // todo foreign items
            todo!()
        }

        self.subs.persist_write(writer);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        todo!()
    }
}
