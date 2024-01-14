use crate::{
    abi::POINTER_SIZE,
    builtins::BuiltinAdt,
    bytecode_compiler::FunctionBytecode,
    bytecode_select,
    ir::BinaryOp,
    items::{AdtInfo, AdtKind, Item},
    types::{Mutability, SubList, Type},
    variants::{Discriminant, VariantIndex, Variants},
    vm::{
        instr::{Instr, Slot},
        Function, FunctionSource, VM,
    },
};

use super::TypeKind;

// TODO, this is just a function. This function:
// 1. Calls drop, if applicable.
// 2. Drops all fields, if applicable.
#[derive(Clone, Copy)]
pub struct DropGlue<'vm>(&'vm Function<'vm>);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct DropBit(u32);

// Jargon:
// Drop "glue" refers to all the code required to drop a type, which may include a Drop impl and code to drop fields.
//
// A drop "leaf" is a type that must be either fully live or fully dead, either because it implements Drop or for some other reason.
//
// A drop "branch" is a type that contains fields that are Drop, but is not Drop itself. Some of its fields may be dead, and some may be live.

/// Contains some metadata about drops, which is used both to compile functions, and to
/// generate drop "glue". Glue is stored separately in the type.
#[derive(Clone)]
pub enum DropInfo<'vm> {
    None,
    Leaf(DropGlue<'vm>),
    Branch {
        glue: DropGlue<'vm>,
        fields: Vec<DropField<'vm>>,
    },
    Enum {
        glue: DropGlue<'vm>,
        fields: Vec<DropField<'vm>>,
    }
}

#[derive(Clone, Debug)]
pub struct DropField<'vm> {
    pub variant: VariantIndex,
    pub field: u32,
    pub offset: u32,
    pub ty: Type<'vm>,
}

impl<'vm> DropInfo<'vm> {
    fn new(
        vm: &'vm VM<'vm>,
        drop_fn: Option<&'vm Item<'vm>>,
        field_types: &[Vec<Type<'vm>>],
        field_offsets: &Variants<Vec<u32>>,
        subs: &SubList<'vm>,
        adt_info: Option<&AdtInfo<'vm>>,
        boxed_ty: Option<Type<'vm>>,
        debug_name: &str,
    ) -> Self {
        let drop_fn = drop_fn.map(|drop_fn| drop_fn.func_mono(subs));

        let mut fields = Vec::new();

        for (variant, field_tys) in field_types.iter().enumerate() {
            let variant = VariantIndex::new(variant as u32);

            for (field_index, ty) in field_tys.iter().enumerate() {
                let ty = ty.sub(subs);
                if ty.drop_info().is_drop() {
                    let offset = field_offsets.get(variant)[field_index as usize];
                    fields.push(DropField {
                        variant,
                        field: field_index as u32,
                        offset,
                        ty,
                    })
                }
            }
        }

        if fields.len() > 0 || drop_fn.is_some() {
            let disc_info = if let Some(adt_info) = adt_info {
                match &adt_info.kind {
                    AdtKind::Enum(enum_info) => {
                        let disc_ty = enum_info.discriminant_internal;
                        let discs = adt_info.variant_discriminants(vm);
                        Some((disc_ty, discs))
                    }
                    AdtKind::Struct => None,
                    AdtKind::Union => {
                        // unions are skipped
                        return DropInfo::None;
                    }
                }
            } else {
                None
            };

            let is_enum = adt_info.map(|info| info.is_enum()).unwrap_or(false);

            let glue = DropGlue::new(vm, drop_fn, &fields, disc_info, boxed_ty, debug_name);

            if drop_fn.is_some() {
                DropInfo::Leaf(glue)
            } else if is_enum {
                DropInfo::Enum { glue, fields }
            } else {
                DropInfo::Branch { glue, fields }
            }
        } else {
            DropInfo::None
        }
    }

    /// Returns true if ANY drop-related actions are required.
    pub fn is_drop(&self) -> bool {
        !self.is_none()
    }

    pub fn is_none(&self) -> bool {
        match self {
            Self::None => true,
            _ => false,
        }
    }

    pub fn glue(&self) -> Option<&DropGlue<'vm>> {
        match self {
            Self::Leaf(glue) => Some(glue),
            Self::Branch { glue, fields: _ } => Some(glue),
            Self::Enum { glue, fields: _ } => Some(glue),
            Self::None => None,
        }
    }
}

impl<'vm> Type<'vm> {
    pub fn drop_info(&self) -> &DropInfo<'vm> {
        let vm = self.1;

        self.0.drop_info.get_or_init(|| {
            // gather fields, bail for types we can't drop
            match self.kind() {
                TypeKind::Bool
                | TypeKind::Char
                | TypeKind::Int(..)
                | TypeKind::Float(_)
                | TypeKind::Ref(..)
                | TypeKind::Ptr(..)
                | TypeKind::FunctionPointer(..)
                | TypeKind::FunctionDef(..)
                | TypeKind::Never => DropInfo::None,

                TypeKind::Array(child, len) => {
                    if child.drop_info().is_drop() {
                        let len = len.get_value() as u32;
                        DropInfo::Leaf(DropGlue::for_array(vm, *child, Some(len)))
                    } else {
                        DropInfo::None
                    }
                }
                TypeKind::Slice(child) => {
                    if child.drop_info().is_drop() {
                        DropInfo::Leaf(DropGlue::for_array(vm, *child, None))
                    } else {
                        DropInfo::None
                    }
                }

                TypeKind::Tuple(fields) => {
                    let offsets = &self.layout().field_offsets;

                    DropInfo::new(
                        vm,
                        None,
                        std::slice::from_ref(fields),
                        offsets,
                        &SubList::empty(),
                        None,
                        None,
                        "tuple",
                    )
                }
                TypeKind::Closure(closure, subs) => {
                    let env = closure.env(subs);
                    env.drop_info().clone()
                }
                TypeKind::Adt(info) => {
                    if info.item.adt_is_builtin(BuiltinAdt::ManuallyDrop) {
                        return DropInfo::None;
                    }

                    let boxed_ty = if info.item.adt_is_builtin(BuiltinAdt::Box) {
                        Some(info.subs.list[0].assert_ty())
                    } else {
                        None
                    };

                    let adt_info = info.item.adt_info();
                    let drop_fn = self.1.find_drop(*self);
                    let offsets = &self.layout().field_offsets;

                    DropInfo::new(
                        vm,
                        drop_fn,
                        adt_info.variant_fields.as_slice(),
                        offsets,
                        &info.subs,
                        Some(adt_info),
                        boxed_ty,
                        info.item.path.as_string(),
                    )
                }
                _ => panic!("drop info: {:?}", self),
            }
        })
    }
}

impl<'vm> DropGlue<'vm> {
    pub fn new(
        vm: &'vm VM<'vm>,
        drop_fn: Option<&'vm Function<'vm>>,
        fields: &[DropField<'vm>],
        disc_info: Option<(Type<'vm>, &Variants<Discriminant>)>,
        boxed_ty: Option<Type<'vm>>,
        debug_name: &str,
    ) -> Self {
        assert!(drop_fn.is_some() || fields.len() > 0);

        // we go straight to bytecode here for several reasons:
        // 1. the code required may depend on the drop info of generics, trying to generate generic IR would probably not go well
        // 2. handling drops in functions may involve generating similar bytecode (currently it does not) there may be some opportunity for re-use
        // 3. performance -- i vaguely remember hearing that dealing with drop glue is expensive in rustc (i may be wrong but this makes sense)

        let self_slot = Slot::new(0);
        // slot used to store enum discriminants
        let disc_slot = Slot::new(16);
        // slot used to store field offsets and other temporaries
        let member_slot = Slot::new(32);
        assert!(member_slot.has_call_align());

        let mut code = Vec::new();

        // box drops require some special handling
        if let Some(boxed_ty) = boxed_ty {
            if let Some(boxed_ty_glue) = boxed_ty.drop_info().glue() {
                let ref_ty = boxed_ty.ref_to(Mutability::Mut);
                code.push(
                    bytecode_select::copy_from_ptr(member_slot, self_slot, ref_ty, 0).unwrap(),
                );
                code.push(Instr::Call(member_slot, boxed_ty_glue.function()));
            }
        }

        if let Some(drop_fn) = drop_fn {
            code.push(Instr::Call(self_slot, drop_fn));
        }

        if let Some((disc_ty, disc_values)) = disc_info {
            // load discriminant
            code.push(bytecode_select::copy_from_ptr(disc_slot, self_slot, disc_ty, 0).unwrap());

            let (eq, _) = bytecode_select::binary(BinaryOp::Eq, disc_ty);
            let mut last_variant = None;
            let mut last_jump = None;

            for field in fields {
                if last_variant != Some(field.variant) {
                    last_variant = Some(field.variant);

                    // finish previous case
                    if let Some(last_jump) = last_jump {
                        code.push(Instr::Return);
                        let dist = code.len() - last_jump;
                        code[last_jump] = Instr::JumpF(dist as i32, member_slot);
                    }

                    // start next case
                    let disc_value = disc_values
                        .get(field.variant)
                        .value()
                        .expect("todo nonzero discriminant");

                    code.push(bytecode_select::literal(
                        disc_value,
                        disc_ty.layout().assert_size(),
                        member_slot,
                    ));
                    code.push(eq(member_slot, member_slot, disc_slot));
                    last_jump = Some(code.len());
                    code.push(Instr::Skipped);
                }

                code.push(Instr::PointerOffset2(
                    member_slot,
                    self_slot,
                    field.offset as i32,
                ));

                let field_glue = field
                    .ty
                    .drop_info()
                    .glue()
                    .expect("drop field missing glue");
                code.push(Instr::Call(member_slot, field_glue.function()));
            }

            // finish previous case
            if let Some(last_jump) = last_jump {
                code.push(Instr::Return);
                let dist = code.len() - last_jump;
                code[last_jump] = Instr::JumpF(dist as i32, member_slot);
            }
        } else {
            for field in fields {
                assert!(field.variant == VariantIndex::new(0));
                code.push(Instr::PointerOffset2(
                    member_slot,
                    self_slot,
                    field.offset as i32,
                ));

                let field_glue = field
                    .ty
                    .drop_info()
                    .glue()
                    .expect("drop field missing glue");
                code.push(Instr::Call(member_slot, field_glue.function()));
            }
        }
        code.push(Instr::Return);

        let bc = FunctionBytecode {
            code,
            drops: Vec::new(),
        };

        let bc = vm.alloc_bytecode(bc);

        let name = vm.alloc_path(&format!("<drop {}>", debug_name));

        Self(vm.alloc_function(FunctionSource::RawBytecode(bc, name), SubList::empty()))
    }

    /// Builds drop glue for an array **OR** a slice, depending on whether the len is provided.
    pub fn for_array(vm: &'vm VM<'vm>, elem_ty: Type<'vm>, len: Option<u32>) -> Self {
        let self_slot = Slot::new(0);
        let self_meta_slot = Slot::new(POINTER_SIZE.bytes() * 1);
        let i_slot = Slot::new(POINTER_SIZE.bytes() * 2);
        let inc_slot = Slot::new(POINTER_SIZE.bytes() * 3);
        let max_slot = Slot::new(POINTER_SIZE.bytes() * 4);
        let test_slot = Slot::new(POINTER_SIZE.bytes() * 5);
        let elem_slot = Slot::new(POINTER_SIZE.bytes() * 6);
        assert!(elem_slot.has_call_align());

        let elem_glue = elem_ty
            .drop_info()
            .glue()
            .expect("array element missing glue");
        let elem_size = elem_ty.layout().assert_size();

        let usize_ty = vm.common_types().usize;

        let mut code = Vec::new();
        code.push(bytecode_select::literal(0, POINTER_SIZE.bytes(), i_slot));
        code.push(bytecode_select::literal(
            elem_size as _,
            POINTER_SIZE.bytes(),
            inc_slot,
        ));
        if let Some(len) = len {
            code.push(bytecode_select::literal(
                (elem_size * len) as _,
                POINTER_SIZE.bytes(),
                max_slot,
            ));
        } else {
            let (mul, _) = bytecode_select::binary(BinaryOp::Mul, usize_ty);
            code.push(mul(max_slot, inc_slot, self_meta_slot));
        }

        // loop body
        code.push(bytecode_select::binary(BinaryOp::Lt, usize_ty).0(
            test_slot, i_slot, max_slot,
        ));
        let loop_top = code.len();
        code.push(Instr::Skipped);

        let (add, _) = bytecode_select::binary(BinaryOp::Add, usize_ty);

        code.push(add(elem_slot, self_slot, i_slot));

        code.push(Instr::Call(elem_slot, elem_glue.function()));

        code.push(add(i_slot, i_slot, inc_slot));

        let loop_end = code.len();
        let distance = (loop_end - loop_top + 1) as i32;
        code.push(Instr::Jump(-distance));
        code[loop_top] = Instr::JumpF(distance, test_slot);

        code.push(Instr::Return);

        let bc = FunctionBytecode {
            code,
            drops: Vec::new(),
        };

        let bc = vm.alloc_bytecode(bc);

        let name = vm.alloc_path("<drop array>");

        Self(vm.alloc_function(FunctionSource::RawBytecode(bc, name), SubList::empty()))
    }

    pub fn function(&self) -> &'vm Function<'vm> {
        self.0
    }
}

impl DropBit {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(&self) -> u32 {
        self.0
    }
}
