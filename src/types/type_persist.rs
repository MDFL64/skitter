use std::{borrow::Cow, rc::Rc};

use crate::{
    items::FunctionSig,
    lazy_collections::LazyItem,
    persist::{Persist, PersistReader, PersistWriteContext, PersistWriter},
    types::{IntSign, IntWidth},
};

use super::{ArraySize, FloatWidth, ItemWithSubs, Mutability, Sub, SubList, Type, TypeKind};

impl<'vm> LazyItem<'vm> for Type<'vm> {
    type Input = TypeKind<'vm>;

    fn build(kind: Self::Input, vm: &'vm crate::vm::VM<'vm>) -> Self {
        vm.types.intern(kind, vm)
    }
}

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
            TypeKind::Ptr(child, Mutability::Const) => {
                writer.write_byte(22);
                child.persist_write(writer);
            }
            TypeKind::Ptr(child, Mutability::Mut) => {
                writer.write_byte(23);
                child.persist_write(writer);
            }

            TypeKind::Tuple(children) => {
                writer.write_byte(24);
                children.persist_write(writer);
            }

            TypeKind::Array(child, size) => {
                writer.write_byte(25);
                child.persist_write(writer);
                size.persist_write(writer);
            }
            TypeKind::Slice(child) => {
                writer.write_byte(26);
                child.persist_write(writer);
            }
            TypeKind::StringSlice => {
                writer.write_byte(27);
            }

            TypeKind::FunctionDef(item_with_subs) => {
                writer.write_byte(30);
                item_with_subs.persist_write(writer);
            }
            TypeKind::Adt(item_with_subs) => {
                writer.write_byte(31);
                item_with_subs.persist_write(writer);
            }
            TypeKind::AssociatedType(item_with_subs) => {
                writer.write_byte(32);
                item_with_subs.persist_write(writer);
            }
            TypeKind::FunctionPointer(sig) => {
                writer.write_byte(33);
                sig.persist_write(writer);
            }
            TypeKind::Closure(closure, sig, subs) => {
                writer.write_byte(34);

                // TODO: a lot of this is likely redundant, and could be pulled from the closure def and subs
                // for now we just write everything

                // write closure ref
                closure.def_crate.persist_write(writer);
                closure.def_item.persist_write(writer);
                writer.write_str(closure.def_full_path);

                // write sig
                sig.kind.persist_write(writer);
                sig.fn_ptr_ty.persist_write(writer);
                sig.env_ty.persist_write(writer);

                // write subs
                subs.persist_write(writer);
            }
            TypeKind::Foreign(crate_id, path) => {
                writer.write_byte(35);
                crate_id.persist_write(writer);
                writer.write_str(path);
            }
            TypeKind::Opaque(item_with_subs, full_path) => {
                writer.write_byte(36);
                item_with_subs.persist_write(writer);
                writer.write_str(full_path);
            }
            TypeKind::Dynamic {
                primary_trait,
                auto_traits,
                is_dyn_star,
            } => {
                writer.write_byte(37);
                primary_trait.persist_write(writer);
                auto_traits.persist_write(writer);
                is_dyn_star.persist_write(writer);
            }

            TypeKind::Param(n) => {
                writer.write_byte(40);
                n.persist_write(writer);
            }

            _ => panic!("write type {:?}", self),
        }
    }

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let n = reader.read_byte();
        match n {
            0 => TypeKind::Int(IntWidth::I8, IntSign::Signed),
            1 => TypeKind::Int(IntWidth::I16, IntSign::Signed),
            2 => TypeKind::Int(IntWidth::I32, IntSign::Signed),
            3 => TypeKind::Int(IntWidth::I64, IntSign::Signed),
            4 => TypeKind::Int(IntWidth::I128, IntSign::Signed),
            5 => TypeKind::Int(IntWidth::ISize, IntSign::Signed),

            6 => TypeKind::Int(IntWidth::I8, IntSign::Unsigned),
            7 => TypeKind::Int(IntWidth::I16, IntSign::Unsigned),
            8 => TypeKind::Int(IntWidth::I32, IntSign::Unsigned),
            9 => TypeKind::Int(IntWidth::I64, IntSign::Unsigned),
            10 => TypeKind::Int(IntWidth::I128, IntSign::Unsigned),
            11 => TypeKind::Int(IntWidth::ISize, IntSign::Unsigned),

            12 => TypeKind::Float(FloatWidth::F32),
            13 => TypeKind::Float(FloatWidth::F64),

            14 => TypeKind::Bool,
            15 => TypeKind::Char,
            16 => TypeKind::Never,

            20 => {
                let child = Persist::persist_read(reader);
                TypeKind::Ref(child, Mutability::Const)
            }
            21 => {
                let child = Persist::persist_read(reader);
                TypeKind::Ref(child, Mutability::Mut)
            }
            22 => {
                let child = Persist::persist_read(reader);
                TypeKind::Ptr(child, Mutability::Const)
            }
            23 => {
                let child = Persist::persist_read(reader);
                TypeKind::Ptr(child, Mutability::Mut)
            }
            24 => {
                let item_with_subs = Persist::persist_read(reader);
                TypeKind::Tuple(item_with_subs)
            }
            25 => {
                let child = Persist::persist_read(reader);
                let size = Persist::persist_read(reader);
                TypeKind::Array(child, size)
            }
            26 => {
                let child = Persist::persist_read(reader);
                TypeKind::Slice(child)
            }
            27 => TypeKind::StringSlice,

            30 => {
                let item = Persist::persist_read(reader);
                TypeKind::FunctionDef(item)
            }
            31 => {
                let item_with_subs = Persist::persist_read(reader);
                TypeKind::Adt(item_with_subs)
            }
            32 => {
                let item_with_subs = Persist::persist_read(reader);
                TypeKind::AssociatedType(item_with_subs)
            }
            33 => {
                let sig = Persist::persist_read(reader);
                TypeKind::FunctionPointer(sig)
            }
            34 => {
                panic!("read closure");
            }
            35 => {
                let crate_id = Persist::persist_read(reader);
                let path = reader.read_str();
                TypeKind::Foreign(crate_id, path)
            }
            37 => {
                let primary_trait = Persist::persist_read(reader);
                let auto_traits = Persist::persist_read(reader);
                let is_dyn_star = Persist::persist_read(reader);

                TypeKind::Dynamic {
                    primary_trait,
                    auto_traits,
                    is_dyn_star,
                }
            }

            40 => {
                let n = Persist::persist_read(reader);
                TypeKind::Param(n)
            }

            _ => panic!("read type {:?}", n),
        }
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

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let c = reader.read_byte() as char;
        match c {
            'T' => Sub::Type(Type::persist_read(reader)),
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

    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        SubList {
            list: Vec::<Sub>::persist_read(reader),
        }
    }
}

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

impl<'vm> Persist<'vm> for ArraySize {
    fn persist_read(reader: &mut PersistReader<'vm>) -> Self {
        let b = reader.read_byte();
        let n = u32::persist_read(reader);
        match b {
            0 => ArraySize::Static(n),
            1 => ArraySize::ConstParam(n),
            _ => panic!(),
        }
    }

    fn persist_write(&self, writer: &mut PersistWriter<'vm>) {
        match self {
            ArraySize::Static(n) => {
                writer.write_byte(0);
                (*n).persist_write(writer);
            }
            ArraySize::ConstParam(n) => {
                writer.write_byte(1);
                (*n).persist_write(writer);
            }
        }
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
