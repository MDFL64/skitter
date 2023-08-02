use crate::persist::{Persist, PersistReadContext, PersistWriteContext};

use super::{
    ArraySize, FloatWidth, IntSign, IntWidth, ItemWithSubs, Mutability, Sub, SubList, Type,
    TypeKind,
};

impl<'vm> Persist<'vm> for Type<'vm> {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        let type_id = self.0.persist_id.get_or_init(|| {
            prepare_child_types(self.kind(), write_ctx);

            let i = write_ctx.types.len();
            write_ctx.types.push(*self);
            i as u32
        });

        type_id.persist_write(write_ctx);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        todo!()
    }
}

/// Child types must live at lower indices when serialized.
///
/// This does not include types in ADTs, which are not needed
/// to construct types while reading and could introduce cycles.
fn prepare_child_types<'vm>(ty_kind: &TypeKind<'vm>, write_ctx: &mut PersistWriteContext<'vm>) {
    match ty_kind {
        // No children.
        TypeKind::Int(..)
        | TypeKind::Float(_)
        | TypeKind::Bool
        | TypeKind::Char
        | TypeKind::StringSlice
        | TypeKind::Param(_)
        | TypeKind::Foreign
        | TypeKind::Dynamic
        | TypeKind::FunctionPointer => (),

        // Single child
        TypeKind::Array(child, _)
        | TypeKind::Slice(child)
        | TypeKind::Ref(child, _)
        | TypeKind::Ptr(child, _) => {
            child.persist_write(write_ctx);
        }

        // Multi-child
        TypeKind::Tuple(children) => {
            for ty in children {
                ty.persist_write(write_ctx);
            }
        }

        // Subs
        TypeKind::Adt(item_with_subs) | TypeKind::AssociatedType(item_with_subs) => {
            for sub in &item_with_subs.subs.list {
                if let Sub::Type(ty) = sub {
                    ty.persist_write(write_ctx);
                }
            }
        }

        _ => panic!("todo children {:?}", ty_kind),
    }
}

impl<'vm> Persist<'vm> for TypeKind<'vm> {


    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        match self {
            TypeKind::Int(IntWidth::I8, IntSign::Signed) => write_ctx.write_byte(0),
            TypeKind::Int(IntWidth::I16, IntSign::Signed) => write_ctx.write_byte(1),
            TypeKind::Int(IntWidth::I32, IntSign::Signed) => write_ctx.write_byte(2),
            TypeKind::Int(IntWidth::I64, IntSign::Signed) => write_ctx.write_byte(3),
            TypeKind::Int(IntWidth::I128, IntSign::Signed) => write_ctx.write_byte(4),
            TypeKind::Int(IntWidth::ISize, IntSign::Signed) => write_ctx.write_byte(5),

            TypeKind::Int(IntWidth::I8, IntSign::Unsigned) => write_ctx.write_byte(6),
            TypeKind::Int(IntWidth::I16, IntSign::Unsigned) => write_ctx.write_byte(7),
            TypeKind::Int(IntWidth::I32, IntSign::Unsigned) => write_ctx.write_byte(8),
            TypeKind::Int(IntWidth::I64, IntSign::Unsigned) => write_ctx.write_byte(9),
            TypeKind::Int(IntWidth::I128, IntSign::Unsigned) => write_ctx.write_byte(10),
            TypeKind::Int(IntWidth::ISize, IntSign::Unsigned) => write_ctx.write_byte(11),

            TypeKind::Float(FloatWidth::F32) => write_ctx.write_byte(12),
            TypeKind::Float(FloatWidth::F64) => write_ctx.write_byte(13),
            TypeKind::Bool => write_ctx.write_byte(14),
            TypeKind::Char => write_ctx.write_byte(15),

            TypeKind::Ref(child, Mutability::Const) => {
                write_ctx.write_byte(16);
                child.persist_write(write_ctx);
            }
            TypeKind::Ref(child, Mutability::Mut) => {
                write_ctx.write_byte(17);
                child.persist_write(write_ctx);
            }
            TypeKind::Ptr(child, Mutability::Const) => {
                write_ctx.write_byte(18);
                child.persist_write(write_ctx);
            }
            TypeKind::Ptr(child, Mutability::Mut) => {
                write_ctx.write_byte(19);
                child.persist_write(write_ctx);
            }
            TypeKind::Array(child, ArraySize::Static(n)) => {
                write_ctx.write_byte(20);
                child.persist_write(write_ctx);
                n.persist_write(write_ctx);
            }
            TypeKind::Array(child, ArraySize::ConstParam(n)) => {
                write_ctx.write_byte(21);
                child.persist_write(write_ctx);
                // TODO
            }
            TypeKind::Slice(child) => {
                write_ctx.write_byte(22);
                child.persist_write(write_ctx);
            }
            TypeKind::StringSlice => {
                write_ctx.write_byte(23);
            }
            TypeKind::Tuple(children) => {
                write_ctx.write_byte(24);
                children.persist_write(write_ctx);
            }
            TypeKind::Adt(item_with_subs) => {
                write_ctx.write_byte(25);
                item_with_subs.persist_write(write_ctx);
            }
            TypeKind::FunctionDef(item_with_subs) => {
                write_ctx.write_byte(26);
                item_with_subs.persist_write(write_ctx);
            }
            TypeKind::AssociatedType(item_with_subs) => {
                write_ctx.write_byte(27);
                item_with_subs.persist_write(write_ctx);
            }
            TypeKind::Param(n) => {
                write_ctx.write_byte(28);
                n.persist_write(write_ctx);
            }
            TypeKind::Never => {
                write_ctx.write_byte(29);
            }
            TypeKind::Foreign => {
                write_ctx.write_byte(30);
            }
            TypeKind::Dynamic => {
                write_ctx.write_byte(31);
            }
            TypeKind::FunctionPointer => {
                write_ctx.write_byte(32);
            }

            _ => panic!("write type {:?}", self),
        }
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        todo!()
    }
}

impl<'vm> Persist<'vm> for Sub<'vm> {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        match self {
            Sub::Type(ty) => {
                write_ctx.write_byte('T' as _);
                ty.persist_write(write_ctx);
            }
            Sub::Const => {
                write_ctx.write_byte('C' as _);
            }
            Sub::Lifetime => {
                write_ctx.write_byte('L' as _);
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
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        self.list.persist_write(write_ctx);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        SubList {
            list: Vec::<Sub>::persist_read(read_ctx),
        }
    }
}

impl<'vm> Persist<'vm> for ItemWithSubs<'vm> {
    fn persist_write(&self, write_ctx: &mut PersistWriteContext<'vm>) {
        if self.item.crate_id == write_ctx.this_crate {
            write_ctx.write_byte(0);
            self.item.item_id.index().persist_write(write_ctx);
        } else {
            // todo foreign items
            todo!()
        }

        self.subs.persist_write(write_ctx);
    }

    fn persist_read(read_ctx: &mut PersistReadContext<'vm>) -> Self {
        todo!()
    }
}
