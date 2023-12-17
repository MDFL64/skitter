use crate::vm::VM;

use super::{FloatWidth, IntSign, IntWidth, Type, TypeKind};

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

    pub void: Type<'vm>,
}

impl<'vm> CommonTypes<'vm> {
    pub fn new(vm: &'vm VM<'vm>) -> Self {
        Self {
            unknown: vm.types.intern(TypeKind::Unknown, vm),

            never: vm.types.intern(TypeKind::Never, vm),
            bool: vm.types.intern(TypeKind::Bool, vm),
            char: vm.types.intern(TypeKind::Char, vm),

            u8: vm
                .types
                .intern(TypeKind::Int(IntWidth::I8, IntSign::Unsigned), vm),
            u16: vm
                .types
                .intern(TypeKind::Int(IntWidth::I16, IntSign::Unsigned), vm),
            u32: vm
                .types
                .intern(TypeKind::Int(IntWidth::I32, IntSign::Unsigned), vm),
            u64: vm
                .types
                .intern(TypeKind::Int(IntWidth::I64, IntSign::Unsigned), vm),
            u128: vm
                .types
                .intern(TypeKind::Int(IntWidth::I128, IntSign::Unsigned), vm),
            usize: vm
                .types
                .intern(TypeKind::Int(IntWidth::ISize, IntSign::Unsigned), vm),

            i8: vm
                .types
                .intern(TypeKind::Int(IntWidth::I8, IntSign::Signed), vm),
            i16: vm
                .types
                .intern(TypeKind::Int(IntWidth::I16, IntSign::Signed), vm),
            i32: vm
                .types
                .intern(TypeKind::Int(IntWidth::I32, IntSign::Signed), vm),
            i64: vm
                .types
                .intern(TypeKind::Int(IntWidth::I64, IntSign::Signed), vm),
            i128: vm
                .types
                .intern(TypeKind::Int(IntWidth::I128, IntSign::Signed), vm),
            isize: vm
                .types
                .intern(TypeKind::Int(IntWidth::ISize, IntSign::Signed), vm),

            f32: vm.types.intern(TypeKind::Float(FloatWidth::F32), vm),
            f64: vm.types.intern(TypeKind::Float(FloatWidth::F64), vm),

            void: vm.types.intern(TypeKind::Tuple(vec![]), vm),
        }
    }
}
