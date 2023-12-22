use std::sync::Arc;

use skitter_macro::Persist;

use crate::{
    abi::POINTER_SIZE,
    bytecode_compiler::{CompilerStack, Local, BytecodeCompiler},
    bytecode_select,
    closure::FnTrait,
    crate_provider::TraitImpl,
    ir::{glue_builder::glue_for_fn_trait, BinaryOp},
    items::{AssocValue, CrateId, IRFlag, Item, ItemPath},
    types::{IntSign, Sub, SubList, Type, TypeKind},
    vm::{
        instr::{Instr, Slot},
        VM,
    },
};

#[derive(Copy, Clone, Debug, Persist)]
pub enum BuiltinTrait {
    Sized,
    Tuple,
    DiscriminantKind,
    FnOnce,
    FnMut,
    Fn,
    Pointee,
}

/// Used for marker trait impls.
fn trait_impl_empty<'vm>() -> TraitImpl<'vm> {
    TraitImpl {
        crate_id: CrateId::new(0),
        impl_subs: SubList { list: vec![] },
        assoc_values: Arc::new([]),
    }
}

fn trait_impl<'vm>(
    impl_subs: SubList<'vm>,
    trait_item: &Item<'vm>,
    pairs: &[(ItemPath, AssocValue<'vm>)],
) -> TraitImpl<'vm> {
    let assoc_values = trait_item.trait_build_assoc_values_for_impl(pairs);

    TraitImpl {
        crate_id: CrateId::new(0),
        impl_subs,
        assoc_values: assoc_values.into(),
    }
}

impl BuiltinTrait {
    // currently we try to generate impls which are entirely concrete
    // TODO try and commit to generating the most generic impl possible?
    // then this might be more usable for actual trait resolution, and we can try caching it?
    pub fn find_impl<'vm>(
        &self,
        for_tys: &SubList<'vm>,
        vm: &'vm VM<'vm>,
        trait_item: &Item<'vm>,
    ) -> Option<TraitImpl<'vm>> {
        match self {
            BuiltinTrait::Sized => {
                assert!(for_tys.list.len() == 1);
                let ty = for_tys.list[0].assert_ty();
                if ty.is_concrete() && ty.is_sized() {
                    Some(trait_impl_empty())
                } else {
                    None
                }
            }
            BuiltinTrait::Tuple => {
                assert!(for_tys.list.len() == 1);
                let ty = for_tys.list[0].assert_ty();
                if let TypeKind::Tuple(_) = ty.kind() {
                    Some(trait_impl_empty())
                } else {
                    None
                }
            }
            BuiltinTrait::DiscriminantKind => {
                // todo this should handle things like Option<Box<T>>
                assert!(for_tys.list.len() == 1);
                let ty = for_tys.list[0].assert_ty();
                if ty.is_concrete() {
                    if let Some(enum_info) = ty.adt_info().enum_info() {
                        return Some(trait_impl(
                            for_tys.clone(),
                            trait_item,
                            &[(
                                ItemPath::for_type("Discriminant"),
                                AssocValue::Type(enum_info.discriminant_external),
                            )],
                        ));
                    }
                }
                None
            }
            BuiltinTrait::Pointee => {
                assert!(for_tys.list.len() == 1);
                let ty = for_tys.list[0].assert_ty();
                let metadata_ty = match ty.kind() {
                    TypeKind::Slice(_) | TypeKind::StringSlice => vm.common_types().usize,
                    TypeKind::Dynamic { is_dyn_star, .. } => {
                        assert!(!is_dyn_star);
                        panic!("pointee dynamic");
                    }
                    _ => {
                        panic!("pointee none");
                    }
                };

                return Some(trait_impl(
                    for_tys.clone(),
                    trait_item,
                    &[(
                        ItemPath::for_type("Metadata"),
                        AssocValue::Type(metadata_ty),
                    )],
                ));
            }
            BuiltinTrait::FnOnce | BuiltinTrait::FnMut | BuiltinTrait::Fn => {
                let func_ty = for_tys.list[0].assert_ty();

                match func_ty.kind() {
                    TypeKind::FunctionDef(fun) => {
                        let sig = fun.item.func_sig(&fun.subs);

                        // todo how to handle generics?
                        // make sure the sig is concrete. will see if this poses issues down the line
                        {
                            for in_ty in sig.inputs.iter() {
                                assert!(in_ty.is_concrete());
                            }
                            assert!(sig.output.is_concrete());
                        }

                        let fn_args_ty = vm.ty_tuple(sig.inputs);
                        let for_tys = SubList {
                            list: vec![Sub::Type(func_ty), Sub::Type(fn_args_ty)],
                        };

                        // BAD:?
                        match self {
                            BuiltinTrait::FnOnce => {
                                let call_ir = glue_for_fn_trait(
                                    FnTrait::FnOnce,
                                    func_ty,
                                    fn_args_ty,
                                    sig.output,
                                );

                                return Some(trait_impl(
                                    for_tys.clone(),
                                    trait_item,
                                    &[
                                        (
                                            ItemPath::for_value("call_once"),
                                            AssocValue::RawFunctionIR(
                                                Arc::new(call_ir),
                                                IRFlag::None,
                                            ),
                                        ),
                                        (
                                            ItemPath::for_type("Output"),
                                            AssocValue::Type(sig.output),
                                        ),
                                    ],
                                ));
                            }
                            BuiltinTrait::FnMut => {
                                let call_ir = glue_for_fn_trait(
                                    FnTrait::FnMut,
                                    func_ty,
                                    fn_args_ty,
                                    sig.output,
                                );

                                return Some(trait_impl(
                                    for_tys.clone(),
                                    trait_item,
                                    &[(
                                        ItemPath::for_value("call_mut"),
                                        AssocValue::RawFunctionIR(Arc::new(call_ir), IRFlag::None),
                                    )],
                                ));
                            }
                            BuiltinTrait::Fn => {
                                let call_ir =
                                    glue_for_fn_trait(FnTrait::Fn, func_ty, fn_args_ty, sig.output);

                                return Some(trait_impl(
                                    for_tys.clone(),
                                    trait_item,
                                    &[(
                                        ItemPath::for_value("call"),
                                        AssocValue::RawFunctionIR(Arc::new(call_ir), IRFlag::None),
                                    )],
                                ));
                            }
                            _ => panic!(),
                        }
                    }
                    TypeKind::FunctionPointer(sig) => {
                        // todo how to handle generics?
                        // make sure the sig is concrete. will see if this poses issues down the line
                        {
                            for in_ty in sig.inputs.iter() {
                                assert!(in_ty.is_concrete());
                            }
                            assert!(sig.output.is_concrete());
                        }

                        let fn_args_ty = vm.ty_tuple(sig.inputs.clone());
                        let for_tys = SubList {
                            list: vec![Sub::Type(func_ty), Sub::Type(fn_args_ty)],
                        };

                        // BAD:?
                        match self {
                            BuiltinTrait::FnOnce => {
                                let call_ir = glue_for_fn_trait(
                                    FnTrait::FnOnce,
                                    func_ty,
                                    fn_args_ty,
                                    sig.output,
                                );

                                return Some(trait_impl(
                                    for_tys.clone(),
                                    trait_item,
                                    &[
                                        (
                                            ItemPath::for_value("call_once"),
                                            AssocValue::RawFunctionIR(
                                                Arc::new(call_ir),
                                                IRFlag::None,
                                            ),
                                        ),
                                        (
                                            ItemPath::for_type("Output"),
                                            AssocValue::Type(sig.output),
                                        ),
                                    ],
                                ));
                            }
                            BuiltinTrait::FnMut => {
                                let call_ir = glue_for_fn_trait(
                                    FnTrait::FnMut,
                                    func_ty,
                                    fn_args_ty,
                                    sig.output,
                                );

                                return Some(trait_impl(
                                    for_tys.clone(),
                                    trait_item,
                                    &[(
                                        ItemPath::for_value("call_mut"),
                                        AssocValue::RawFunctionIR(Arc::new(call_ir), IRFlag::None),
                                    )],
                                ));
                            }
                            BuiltinTrait::Fn => {
                                let call_ir =
                                    glue_for_fn_trait(FnTrait::Fn, func_ty, fn_args_ty, sig.output);

                                return Some(trait_impl(
                                    for_tys.clone(),
                                    trait_item,
                                    &[(
                                        ItemPath::for_value("call"),
                                        AssocValue::RawFunctionIR(Arc::new(call_ir), IRFlag::None),
                                    )],
                                ));
                            }
                            _ => panic!(),
                        }
                    }
                    TypeKind::Closure(closure, closure_subs) => {
                        let fn_trait = match self {
                            BuiltinTrait::FnOnce => FnTrait::FnOnce,
                            BuiltinTrait::FnMut => FnTrait::FnMut,
                            BuiltinTrait::Fn => FnTrait::Fn,
                            _ => panic!(),
                        };

                        let abstract_sig = closure.abstract_sig();

                        let ir = closure.ir_for_trait(fn_trait, abstract_sig.env_ty);

                        let fn_args_ty = ir.sig.inputs[1].sub(closure_subs);
                        let for_tys = SubList {
                            list: vec![Sub::Type(func_ty), Sub::Type(fn_args_ty)],
                        };

                        match self {
                            BuiltinTrait::FnOnce => {
                                let output = ir.sig.output;

                                return Some(trait_impl(
                                    for_tys.clone(),
                                    trait_item,
                                    &[
                                        (
                                            ItemPath::for_value("call_once"),
                                            AssocValue::RawFunctionIR(ir, IRFlag::UseClosureSubs),
                                        ),
                                        (ItemPath::for_type("Output"), AssocValue::Type(output)),
                                    ],
                                ));
                            }
                            BuiltinTrait::FnMut => {
                                return Some(trait_impl(
                                    for_tys.clone(),
                                    trait_item,
                                    &[(
                                        ItemPath::for_value("call_mut"),
                                        AssocValue::RawFunctionIR(ir, IRFlag::UseClosureSubs),
                                    )],
                                ));
                            }
                            BuiltinTrait::Fn => {
                                return Some(trait_impl(
                                    for_tys.clone(),
                                    trait_item,
                                    &[(
                                        ItemPath::for_value("call"),
                                        AssocValue::RawFunctionIR(ir, IRFlag::UseClosureSubs),
                                    )],
                                ));
                            }
                            _ => panic!(),
                        }
                    }
                    _ => (),
                }
                None
            }
        }
    }
}

/// Writes bytecode for a rust intrinsic. Should probably be integrated into the bytecode compiler more closely.
pub fn compile_rust_intrinsic<'vm>(
    name: &str,
    subs: &SubList<'vm>,
    
    args: Vec<Local<'vm>>,
    out: Local<'vm>,

    compiler: &mut BytecodeCompiler<'vm,'_>
) {
    match name {
        /*"transmute" | "transmute_unchecked" => {
            assert!(subs.list.len() == 2);
            assert!(arg_slots.len() == 1);

            let arg = subs.list[0].assert_ty();
            let res = subs.list[1].assert_ty();

            assert_eq!(arg.layout().assert_size(), res.layout().assert_size());

            let min_align_ty = std::cmp::min_by_key(arg, res, |ty: &Type| ty.layout().align);

            if let Some(copy) = bytecode_select::copy(out_slot, arg_slots[0], min_align_ty) {
                out_bc.push(copy);
            }
        }
        "bswap" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 1);

            let arg_res = subs.list[0].assert_ty();
            let size = arg_res.layout().assert_size();

            let arg_slot = arg_slots[0];

            for i in 0..size {
                out_bc.push(Instr::MovSS1(
                    out_slot.offset_by(i as i32),
                    arg_slot.offset_by((size - i - 1) as i32),
                ));
            }
        }
        "offset" => {
            assert!(subs.list.len() == 2);
            assert!(arg_slots.len() == 2);

            let ptr_ty = subs.list[0].assert_ty();
            let offset_ty = subs.list[1].assert_ty();

            // todo wide pointers?
            assert!(ptr_ty.layout().assert_size() == POINTER_SIZE.bytes());

            let elem_size = if let TypeKind::Ptr(child, _) = ptr_ty.kind() {
                child.layout().assert_size()
            } else {
                panic!("offset intrinsic used on non-ptr");
            };

            if elem_size == 1 {
                let (offset_ctor, _) = bytecode_select::binary(BinaryOp::Add, offset_ty);

                out_bc.push(offset_ctor(out_slot, arg_slots[0], arg_slots[1]));
            } else {
                // out = size
                out_bc.push(bytecode_select::literal(
                    elem_size as _,
                    POINTER_SIZE.bytes(),
                    out_slot,
                ));

                // out = size * n
                let (mul_ctor, _) = bytecode_select::binary(BinaryOp::Mul, offset_ty);
                out_bc.push(mul_ctor(out_slot, out_slot, arg_slots[1]));

                // out = base + size * n
                let (offset_ctor, _) = bytecode_select::binary(BinaryOp::Add, offset_ty);
                out_bc.push(offset_ctor(out_slot, out_slot, arg_slots[0]));
            }
        }
        "ptr_offset_from_unsigned" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let ty = subs.list[0].assert_ty();
            let size = ty.layout().assert_size();

            let usize_ty = vm.common_types().usize;

            let (sub_ctor, _) = bytecode_select::binary(BinaryOp::Sub, usize_ty);

            if size == 1 {
                out_bc.push(sub_ctor(out_slot, arg_slots[0], arg_slots[1]));
            } else {
                panic!("non-trivial offset");
            }
        }
        "min_align_of" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 0);

            let arg_ty = subs.list[0].assert_ty();

            let res = arg_ty.layout().align;

            out_bc.push(bytecode_select::literal(
                res as _,
                POINTER_SIZE.bytes(),
                out_slot,
            ));
        }
        "size_of" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 0);

            let arg_ty = subs.list[0].assert_ty();

            let res = arg_ty.layout().assert_size();

            out_bc.push(bytecode_select::literal(
                res as _,
                POINTER_SIZE.bytes(),
                out_slot,
            ));
        }
        "min_align_of_val" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 1);

            let arg_ty = subs.list[0].assert_ty();

            if let TypeKind::Dynamic { .. } = arg_ty.kind() {
                // it may be best to just force alignment to the max for these cases and use that compile-time value
                panic!("todo, trait objects may have alignment only decidable at run-time");
            } else {
                // everything else should be decidable at compile-time
                let res = arg_ty.layout().align;

                out_bc.push(bytecode_select::literal(
                    res as _,
                    POINTER_SIZE.bytes(),
                    out_slot,
                ));
            }
        }
        "size_of_val" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 1);

            let arg_ty = subs.list[0].assert_ty();

            if let Some(size) = arg_ty.layout().maybe_size {
                out_bc.push(bytecode_select::literal(
                    size as _,
                    POINTER_SIZE.bytes(),
                    out_slot,
                ));
            } else {
                let arg_slot = arg_slots[0];

                let ptr_size = POINTER_SIZE.bytes();

                let ty_usize = vm.common_types().usize;

                let byte_slice_size = || {
                    bytecode_select::copy(out_slot, arg_slot.offset_by(ptr_size as i32), ty_usize)
                        .unwrap()
                };

                match arg_ty.kind() {
                    TypeKind::Slice(child_ty) => {
                        let (mul_ctor, _) =
                            bytecode_select::binary(crate::ir::BinaryOp::Mul, ty_usize);

                        let elem_size = child_ty.layout().assert_size();

                        if elem_size == 1 {
                            out_bc.push(byte_slice_size());
                        } else {
                            out_bc.push(bytecode_select::literal(
                                elem_size as _,
                                ptr_size,
                                out_slot,
                            ));
                            out_bc.push(mul_ctor(
                                out_slot,
                                out_slot,
                                arg_slot.offset_by(ptr_size as i32),
                            ));
                        }
                    }
                    TypeKind::StringSlice => out_bc.push(byte_slice_size()),
                    _ => panic!("todo unsized value size"),
                }
            };
        }
        "variant_count" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 0);

            let arg_ty = subs.list[0].assert_ty();

            // get the number of variants
            let res = arg_ty.layout().field_offsets.len();

            // we should maybe panic if not checking an enum, but this is probably okay

            out_bc.push(bytecode_select::literal(
                res as _,
                POINTER_SIZE.bytes(),
                out_slot,
            ));
        }
        "discriminant_value" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let enum_info = arg_ty
                .adt_info()
                .enum_info()
                .expect("attempt to get discriminant of non-enum");

            let layout_external = enum_info.discriminant_external.layout();
            let layout_internal = enum_info.discriminant_internal.layout();

            if layout_internal.assert_size() > layout_external.assert_size() {
                panic!("internal layout may not be larger than external");
            }

            out_bc.push(
                bytecode_select::copy_from_ptr(
                    out_slot,
                    arg_slots[0],
                    enum_info.discriminant_internal,
                    0,
                )
                .unwrap(),
            );

            if enum_info.discriminant_external != enum_info.discriminant_internal {
                out_bc.push(bytecode_select::cast(
                    enum_info.discriminant_internal,
                    enum_info.discriminant_external,
                )(out_slot, out_slot));
            }
        }
        //""
        "const_eval_select" => {
            // we always select the runtime impl
            // it is unsound to make the two impls behave differently, so this should hopefully be okay
            // it might cause some tests to fail though?
            assert!(subs.list.len() == 4);
            assert!(arg_slots.len() == 3);

            let source_slot = arg_slots[0];
            let args_ty = subs.list[0].assert_ty();
            let res_ty = subs.list[3].assert_ty();
            let func = subs.list[2].assert_ty().func_item().unwrap();
            let func = func.item.func_mono(&func.subs);

            let TypeKind::Tuple(arg_tys) = args_ty.kind() else {
                panic!("const_eval_select: bad args ty");
            };

            stack.align_for_call();
            let call_slot = stack.alloc(res_ty);
            for (arg_ty, arg_offset) in arg_tys
                .iter()
                .zip(args_ty.layout().field_offsets.assert_single())
            {
                let arg_slot = stack.alloc(*arg_ty);

                if let Some(copy) = bytecode_select::copy(
                    arg_slot,
                    source_slot.offset_by(*arg_offset as i32),
                    *arg_ty,
                ) {
                    out_bc.push(copy);
                }
            }
            out_bc.push(Instr::Call(call_slot, func));

            if let Some(copy) = bytecode_select::copy(out_slot, call_slot, res_ty) {
                out_bc.push(copy);
            }
        }
        "read_via_copy" | "volatile_load" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let arg_slot = arg_slots[0];

            if let Some(copy) = bytecode_select::copy_from_ptr(out_slot, arg_slot, arg_ty, 0) {
                out_bc.push(copy);
            }
        }
        "write_via_move" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let ptr_slot = arg_slots[0];
            let val_slot = arg_slots[1];

            if let Some(copy) = bytecode_select::copy_to_ptr(ptr_slot, val_slot, arg_ty, 0) {
                out_bc.push(copy);
            }
        }
        "copy_nonoverlapping" => {
            // generally rust will check that the regions are not overlapping
            // no need to assert it here
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 3);

            let arg_ty = subs.list[0].assert_ty();
            let arg_size = arg_ty.layout().assert_size();

            if arg_size == 1 {
                out_bc.push(Instr::MemCopy(arg_slots[0], arg_slots[1], arg_slots[2]));
            } else {
                let usize_ty = vm.common_types().usize;
                let size_slot = stack.alloc(usize_ty);

                let (mul_op, _) = bytecode_select::binary(BinaryOp::Mul, usize_ty);

                out_bc.push(bytecode_select::literal(
                    arg_size as _,
                    usize_ty.layout().assert_size(),
                    size_slot,
                ));
                out_bc.push(mul_op(size_slot, size_slot, arg_slots[2]));
                out_bc.push(Instr::MemCopy(arg_slots[0], arg_slots[1], size_slot));
                //panic!("non-trivial copy {}", arg_size);
            }
        }
        "ctpop" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let arg_slot = arg_slots[0];

            match arg_ty.layout().assert_size() {
                1 => out_bc.push(Instr::I8_PopCount(out_slot, arg_slot)),
                2 => out_bc.push(Instr::I16_PopCount(out_slot, arg_slot)),
                4 => out_bc.push(Instr::I32_PopCount(out_slot, arg_slot)),
                8 => out_bc.push(Instr::I64_PopCount(out_slot, arg_slot)),
                16 => out_bc.push(Instr::I128_PopCount(out_slot, arg_slot)),
                _ => panic!("can't ctpop {}", arg_ty),
            }
        }
        "cttz_nonzero" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let arg_slot = arg_slots[0];

            match arg_ty.layout().assert_size() {
                1 => out_bc.push(Instr::I8_TrailingZeros(out_slot, arg_slot)),
                2 => out_bc.push(Instr::I16_TrailingZeros(out_slot, arg_slot)),
                4 => out_bc.push(Instr::I32_TrailingZeros(out_slot, arg_slot)),
                8 => out_bc.push(Instr::I64_TrailingZeros(out_slot, arg_slot)),
                16 => out_bc.push(Instr::I128_TrailingZeros(out_slot, arg_slot)),
                _ => panic!("can't cttz {}", arg_ty),
            }
        }
        "bitreverse" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let arg_slot = arg_slots[0];

            match arg_ty.layout().assert_size() {
                1 => out_bc.push(Instr::I8_ReverseBits(out_slot, arg_slot)),
                2 => out_bc.push(Instr::I16_ReverseBits(out_slot, arg_slot)),
                4 => out_bc.push(Instr::I32_ReverseBits(out_slot, arg_slot)),
                8 => out_bc.push(Instr::I64_ReverseBits(out_slot, arg_slot)),
                16 => out_bc.push(Instr::I128_ReverseBits(out_slot, arg_slot)),
                _ => panic!("can't ctpop {}", arg_ty),
            }
        }
        "rotate_left" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let arg1 = arg_slots[0];
            let arg2 = arg_slots[1];

            match arg_ty.layout().assert_size() {
                1 => out_bc.push(Instr::I8_RotateLeft(out_slot, arg1, arg2)),
                2 => out_bc.push(Instr::I16_RotateLeft(out_slot, arg1, arg2)),
                4 => out_bc.push(Instr::I32_RotateLeft(out_slot, arg1, arg2)),
                8 => out_bc.push(Instr::I64_RotateLeft(out_slot, arg1, arg2)),
                16 => out_bc.push(Instr::I128_RotateLeft(out_slot, arg1, arg2)),
                _ => panic!("can't ctpop {}", arg_ty),
            }
        }
        "rotate_right" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let arg1 = arg_slots[0];
            let arg2 = arg_slots[1];

            match arg_ty.layout().assert_size() {
                1 => out_bc.push(Instr::I8_RotateRight(out_slot, arg1, arg2)),
                2 => out_bc.push(Instr::I16_RotateRight(out_slot, arg1, arg2)),
                4 => out_bc.push(Instr::I32_RotateRight(out_slot, arg1, arg2)),
                8 => out_bc.push(Instr::I64_RotateRight(out_slot, arg1, arg2)),
                16 => out_bc.push(Instr::I128_RotateRight(out_slot, arg1, arg2)),
                _ => panic!("can't ctpop {}", arg_ty),
            }
        }
        "saturating_add" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let arg1 = arg_slots[0];
            let arg2 = arg_slots[1];

            match (arg_ty.layout().assert_size(), arg_ty.sign()) {
                (8, IntSign::Unsigned) => out_bc.push(Instr::I64_U_SatAdd(out_slot, arg1, arg2)),
                _ => panic!("can't saturating_add {}", arg_ty),
            }
        }
        "abort" => {
            out_bc.push(Instr::Error(Box::new("abort".to_owned())));
        }
        "unreachable" => {
            out_bc.push(Instr::Error(Box::new("unreachable".to_owned())));
        }
        "caller_location" => {
            out_bc.push(Instr::Error(Box::new(
                "caller_location not implemented".to_owned(),
            )));
        }
        "assume" | "assert_zero_valid" | "assert_inhabited" => {
            // do nothing yeehaw
        }
        "unlikely" => {
            assert!(subs.list.len() == 0);
            assert!(arg_slots.len() == 1);

            let ty_bool = vm.common_types().bool;

            // copying here isn't so great, ideally we could make the res slot optional like in the main compiler
            let copy = bytecode_select::copy(out_slot, arg_slots[0], ty_bool).unwrap();
            out_bc.push(copy);
        }
        "write_bytes" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 3);

            let arg_ty = subs.list[0].assert_ty();
            let size = arg_ty.layout().assert_size();
            // todo it might be cleaner to multiply the size in bytecode, instead of having a limit attached to the instruction
            let small_size: u16 = size.try_into().expect("size too large!");

            out_bc.push(Instr::WriteBytes {
                size: small_size,
                dst: arg_slots[0],
                val: arg_slots[1],
                count: arg_slots[2],
            });
        }
        // ugh
        "add_with_overflow" => {
            // TODO make this actually work -- I'd rather not add more arithmetic instructions
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let carry_slot = out_slot.offset_by(arg_ty.layout().assert_size() as i32);

            let (ctor, _) = bytecode_select::binary(BinaryOp::Add, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
            out_bc.push(bytecode_select::literal(0, 1, carry_slot));
            // signed: same signs on inputs, differ from output
            // unsigned: (a + b < a)
        }
        "sub_with_overflow" => {
            // TODO make this actually work -- I'd rather not add more arithmetic instructions
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let carry_slot = out_slot.offset_by(arg_ty.layout().assert_size() as i32);

            let (ctor, _) = bytecode_select::binary(BinaryOp::Sub, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
            out_bc.push(bytecode_select::literal(0, 1, carry_slot));
        }
        "mul_with_overflow" => {
            // TODO make this actually work -- I'd rather not add more arithmetic instructions
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let carry_slot = out_slot.offset_by(arg_ty.layout().assert_size() as i32);

            let (ctor, _) = bytecode_select::binary(BinaryOp::Mul, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
            out_bc.push(bytecode_select::literal(0, 1, carry_slot));
        }
        "unchecked_add" | "wrapping_add" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Add, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
        }
        "unchecked_sub" | "wrapping_sub" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Sub, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
        }
        "unchecked_mul" | "wrapping_mul" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Mul, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
        }
        "exact_div" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Div, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
        }
        "unchecked_rem" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Rem, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
        }
        "unchecked_shl" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::ShiftL, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
        }
        "unchecked_shr" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::ShiftR, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
        }
        // icky float intrinsics which aren't generic
        "minnumf32" => {
            assert!(arg_slots.len() == 2);
            out_bc.push(Instr::F32_Min(out_slot, arg_slots[0], arg_slots[1]));
        }
        "maxnumf32" => {
            assert!(arg_slots.len() == 2);
            out_bc.push(Instr::F32_Max(out_slot, arg_slots[0], arg_slots[1]));
        }
        "minnumf64" => {
            assert!(arg_slots.len() == 2);
            out_bc.push(Instr::F64_Min(out_slot, arg_slots[0], arg_slots[1]));
        }
        "maxnumf64" => {
            assert!(arg_slots.len() == 2);
            out_bc.push(Instr::F64_Max(out_slot, arg_slots[0], arg_slots[1]));
        }*/

        // special skitter-specific intrinsics
        "skitter_box_new" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let layout = arg_ty.layout();

            let size = layout.assert_size();
            if size == 0 {
                // dangling pointer
                compiler.out_bc.push(bytecode_select::literal(1, POINTER_SIZE.bytes(), out.slot));
            } else {
                compiler.out_bc.push(Instr::Alloc {
                    out: out.slot,
                    size: layout.assert_size(),
                    align: layout.align,
                });
                compiler.local_move_to_ptr(out.slot, args[0], 0);
                
                //out_bc
                //    .push(bytecode_select::copy_to_ptr(out_slot, arg_slots[0], arg_ty, 0).unwrap());
            }
        }

        _ => {
            panic!("attempt compile intrinsic: {}{}", name, subs);
        }
    }
}
