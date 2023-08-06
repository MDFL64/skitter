use crate::vm::VM;

use super::{Type, TypeKind, IntWidth, IntSign, FloatWidth};

pub struct CommonTypes<'vm> {
    pub unknown: Type<'vm>,
    pub never: Type<'vm>,
    pub bool: Type<'vm>,
    pub char: Type<'vm>,

    pub u8: Type<'vm>,
    pub u16: Type<'vm>,
    pub u32: Type<'vm>,
    pub u64: Type<'vm>,
    pub u128: Type<'vm>,
    pub usize: Type<'vm>,

    pub i8: Type<'vm>,
    pub i16: Type<'vm>,
    pub i32: Type<'vm>,
    pub i64: Type<'vm>,
    pub i128: Type<'vm>,
    pub isize: Type<'vm>,

    pub f32: Type<'vm>,
    pub f64: Type<'vm>,
}

impl<'vm> CommonTypes<'vm> {
    pub fn new(vm: &'vm VM<'vm>) -> Self {
        Self {
            unknown: vm.types.intern(TypeKind::Unknown, vm),

            never: vm.types.intern_with_persist_id(TypeKind::Never, vm, 0),
            bool: vm.types.intern_with_persist_id(TypeKind::Bool, vm, 1),
            char: vm.types.intern_with_persist_id(TypeKind::Char, vm, 2),

            u8: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I8, IntSign::Unsigned), vm, 3),
            u16: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I16, IntSign::Unsigned), vm, 4),
            u32: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I32, IntSign::Unsigned), vm, 5),
            u64: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I64, IntSign::Unsigned), vm, 6),
            u128: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I128, IntSign::Unsigned), vm, 7),
            usize: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::ISize, IntSign::Unsigned), vm, 8),

            i8: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I8, IntSign::Signed), vm, 9),
            i16: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I16, IntSign::Signed), vm, 10),
            i32: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I32, IntSign::Signed), vm, 11),
            i64: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I64, IntSign::Signed), vm, 12),
            i128: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::I128, IntSign::Signed), vm, 13),
            isize: vm.types
                .intern_with_persist_id(TypeKind::Int(IntWidth::ISize, IntSign::Signed), vm, 14),

            f32: vm.types
                .intern_with_persist_id(TypeKind::Float(FloatWidth::F32), vm, 15),
            f64: vm.types
                .intern_with_persist_id(TypeKind::Float(FloatWidth::F64), vm, 16),
        }
    }
}

