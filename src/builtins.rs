use paste::paste;
use std::sync::Arc;

use skitter_macro::Persist;

use crate::{
    abi::POINTER_SIZE,
    bytecode_compiler::{BytecodeCompiler, CompilerStack, Local},
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

#[derive(Copy, Clone, Debug, Persist, PartialEq, Eq)]
pub enum BuiltinAdt {
    Box,
    ManuallyDrop,
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

// I8_S_CheckedAdd

/// Writes bytecode for a rust intrinsic. Should probably be integrated into the bytecode compiler more closely.
/// NOTE: local init/moving is handled by the caller!
pub fn compile_rust_intrinsic<'vm>(
    name: &str,
    subs: &SubList<'vm>,

    args: Vec<Local<'vm>>,
    out: Local<'vm>,

    compiler: &mut BytecodeCompiler<'vm, '_>,
) {
    macro_rules! select_binary_signed {
        ($instr_name:ident) => {
            {
                assert!(subs.list.len() == 1);
                assert!(args.len() == 2);

                let arg_ty = subs.list[0].assert_ty();
                let arg1 = args[0].slot;
                let arg2 = args[1].slot;

                paste! {
                    match (arg_ty.layout().assert_size(), arg_ty.sign()) {

                        (1, IntSign::Unsigned) => compiler.out_bc.push( Instr:: [< I8_U_ $instr_name>] (out.slot, arg1, arg2)),
                        (2, IntSign::Unsigned) => compiler.out_bc.push( Instr:: [< I16_U_ $instr_name>] (out.slot, arg1, arg2)),
                        (4, IntSign::Unsigned) => compiler.out_bc.push( Instr:: [< I32_U_ $instr_name>] (out.slot, arg1, arg2)),
                        (8, IntSign::Unsigned) => compiler.out_bc.push( Instr:: [< I64_U_ $instr_name>] (out.slot, arg1, arg2)),
                        (16, IntSign::Unsigned) => compiler.out_bc.push( Instr:: [< I128_U_ $instr_name>] (out.slot, arg1, arg2)),

                        (1, IntSign::Signed) => compiler.out_bc.push( Instr:: [< I8_S_ $instr_name>] (out.slot, arg1, arg2)),
                        (2, IntSign::Signed) => compiler.out_bc.push( Instr:: [< I16_S_ $instr_name>] (out.slot, arg1, arg2)),
                        (4, IntSign::Signed) => compiler.out_bc.push( Instr:: [< I32_S_ $instr_name>] (out.slot, arg1, arg2)),
                        (8, IntSign::Signed) => compiler.out_bc.push( Instr:: [< I64_S_ $instr_name>] (out.slot, arg1, arg2)),
                        (16, IntSign::Signed) => compiler.out_bc.push( Instr:: [< I128_S_ $instr_name>] (out.slot, arg1, arg2)),

                        _ => panic!("can't {} {}", stringify!(instr_name), arg_ty),
                    }
                }
            }
        }
    }

    macro_rules! select_binary_int {
        ($instr_name:ident) => {
            {
                assert!(subs.list.len() == 1);
                assert!(args.len() == 2);

                let arg_ty = subs.list[0].assert_ty();
                let arg1 = args[0].slot;
                let arg2 = args[1].slot;

                paste! {
                    match arg_ty.layout().assert_size() {

                        1 => compiler.out_bc.push( Instr:: [< I8_ $instr_name>] (out.slot, arg1, arg2)),
                        2 => compiler.out_bc.push( Instr:: [< I16_ $instr_name>] (out.slot, arg1, arg2)),
                        4 => compiler.out_bc.push( Instr:: [< I32_ $instr_name>] (out.slot, arg1, arg2)),
                        8 => compiler.out_bc.push( Instr:: [< I64_ $instr_name>] (out.slot, arg1, arg2)),
                        16 => compiler.out_bc.push( Instr:: [< I128_ $instr_name>] (out.slot, arg1, arg2)),

                        _ => panic!("can't {} {}", stringify!(instr_name), arg_ty),
                    }
                }
            }
        }
    }

    macro_rules! select_unary_int {
        ($instr_name:ident) => {{
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let arg = args[0].slot;

            paste! {
                match arg_ty.layout().assert_size() {
                    1 => compiler.out_bc.push( Instr:: [< I8_ $instr_name>] (out.slot, arg)),
                    2 => compiler.out_bc.push( Instr:: [< I16_ $instr_name>] (out.slot, arg)),
                    4 => compiler.out_bc.push( Instr:: [< I32_ $instr_name>] (out.slot, arg)),
                    8 => compiler.out_bc.push( Instr:: [< I64_ $instr_name>] (out.slot, arg)),
                    16 => compiler.out_bc.push( Instr:: [< I128_ $instr_name>] (out.slot, arg)),

                    _ => panic!("can't {} {}", stringify!(instr_name), arg_ty),
                }
            }
        }};
    }

    match name {
        "transmute" | "transmute_unchecked" => {
            assert!(subs.list.len() == 2);
            assert!(args.len() == 1);

            let arg = subs.list[0].assert_ty();
            let res = subs.list[1].assert_ty();

            assert_eq!(arg.layout().assert_size(), res.layout().assert_size());

            let min_align_ty = std::cmp::min_by_key(arg, res, |ty: &Type| ty.layout().align);

            // do not use normal local handling
            if let Some(copy) = bytecode_select::copy(out.slot, args[0].slot, min_align_ty) {
                compiler.out_bc.push(copy);
            }
        }
        "bswap" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_res = subs.list[0].assert_ty();
            let size = arg_res.layout().assert_size();

            let arg = args[0];

            for i in 0..size {
                compiler.out_bc.push(Instr::MovSS1(
                    out.slot.offset_by(i as i32),
                    arg.slot.offset_by((size - i - 1) as i32),
                ));
            }
        }
        "offset" => {
            assert!(subs.list.len() == 2);
            assert!(args.len() == 2);

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

                compiler
                    .out_bc
                    .push(offset_ctor(out.slot, args[0].slot, args[1].slot));
            } else {
                // out = size
                compiler.out_bc.push(bytecode_select::literal(
                    elem_size as _,
                    POINTER_SIZE.bytes(),
                    out.slot,
                ));

                // out = size * n
                let (mul_ctor, _) = bytecode_select::binary(BinaryOp::Mul, offset_ty);
                compiler
                    .out_bc
                    .push(mul_ctor(out.slot, out.slot, args[1].slot));

                // out = base + size * n
                let (offset_ctor, _) = bytecode_select::binary(BinaryOp::Add, offset_ty);
                compiler
                    .out_bc
                    .push(offset_ctor(out.slot, out.slot, args[0].slot));
            }
        }
        "ptr_offset_from_unsigned" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 2);

            let ty = subs.list[0].assert_ty();
            let size = ty.layout().assert_size();

            let usize_ty = compiler.vm.common_types().usize;

            let (sub_ctor, _) = bytecode_select::binary(BinaryOp::Sub, usize_ty);

            compiler
                .out_bc
                .push(sub_ctor(out.slot, args[0].slot, args[1].slot));

            assert!(size > 0);

            if size != 1 {
                let divisor_slot = compiler.stack.alloc_no_drop(usize_ty);

                compiler.out_bc.push(bytecode_select::literal(
                    size as _,
                    POINTER_SIZE.bytes(),
                    divisor_slot,
                ));

                let (div_ctor, _) = bytecode_select::binary(BinaryOp::Div, usize_ty);
                compiler
                    .out_bc
                    .push(div_ctor(out.slot, out.slot, divisor_slot));
            }
        }
        "min_align_of" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 0);

            let arg_ty = subs.list[0].assert_ty();

            let res = arg_ty.layout().align;

            compiler.out_bc.push(bytecode_select::literal(
                res as _,
                POINTER_SIZE.bytes(),
                out.slot,
            ));
        }
        "size_of" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 0);

            let arg_ty = subs.list[0].assert_ty();

            let res = arg_ty.layout().assert_size();

            compiler.out_bc.push(bytecode_select::literal(
                res as _,
                POINTER_SIZE.bytes(),
                out.slot,
            ));
        }
        "min_align_of_val" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_ty = subs.list[0].assert_ty();

            if let TypeKind::Dynamic { .. } = arg_ty.kind() {
                // it may be best to just force alignment to the max for these cases and use that compile-time value
                panic!("todo, trait objects may have alignment only decidable at run-time");
            } else {
                // everything else should be decidable at compile-time
                let res = arg_ty.layout().align;

                compiler.out_bc.push(bytecode_select::literal(
                    res as _,
                    POINTER_SIZE.bytes(),
                    out.slot,
                ));
            }
        }
        "size_of_val" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_ty = subs.list[0].assert_ty();

            if let Some(size) = arg_ty.layout().maybe_size {
                compiler.out_bc.push(bytecode_select::literal(
                    size as _,
                    POINTER_SIZE.bytes(),
                    out.slot,
                ));
            } else {
                let arg_slot = args[0].slot;

                let ptr_size = POINTER_SIZE.bytes();

                let ty_usize = compiler.vm.common_types().usize;

                let byte_slice_size = || {
                    bytecode_select::copy(out.slot, arg_slot.offset_by(ptr_size as i32), ty_usize)
                        .unwrap()
                };

                match arg_ty.kind() {
                    TypeKind::Slice(child_ty) => {
                        let (mul_ctor, _) =
                            bytecode_select::binary(crate::ir::BinaryOp::Mul, ty_usize);

                        let elem_size = child_ty.layout().assert_size();

                        if elem_size == 1 {
                            compiler.out_bc.push(byte_slice_size());
                        } else {
                            compiler.out_bc.push(bytecode_select::literal(
                                elem_size as _,
                                ptr_size,
                                out.slot,
                            ));
                            compiler.out_bc.push(mul_ctor(
                                out.slot,
                                out.slot,
                                arg_slot.offset_by(ptr_size as i32),
                            ));
                        }
                    }
                    TypeKind::StringSlice => compiler.out_bc.push(byte_slice_size()),
                    _ => panic!("todo unsized value size"),
                }
            };
        }
        "variant_count" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 0);

            let arg_ty = subs.list[0].assert_ty();

            // get the number of variants
            let res = arg_ty.layout().field_offsets.len();

            // we should maybe panic if not checking an enum, but this is probably okay

            compiler.out_bc.push(bytecode_select::literal(
                res as _,
                POINTER_SIZE.bytes(),
                out.slot,
            ));
        }
        "discriminant_value" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

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

            compiler.out_bc.push(
                bytecode_select::copy_from_ptr(
                    out.slot,
                    args[0].slot,
                    enum_info.discriminant_internal,
                    0,
                )
                .unwrap(),
            );

            if enum_info.discriminant_external != enum_info.discriminant_internal {
                compiler.out_bc.push(bytecode_select::cast(
                    enum_info.discriminant_internal,
                    enum_info.discriminant_external,
                )(out.slot, out.slot));
            }
        }

        "const_eval_select" => {
            // we always select the runtime impl
            // it is unsound to make the two impls behave differently, so this should hopefully be okay
            // it might cause some tests to fail though?
            assert!(subs.list.len() == 4);
            assert!(args.len() == 3);

            let source_slot = args[0].slot;
            let args_ty = subs.list[0].assert_ty();
            let res_ty = subs.list[3].assert_ty();
            let func = subs.list[2].assert_ty().func_item().unwrap();
            let func = func.item.func_mono(&func.subs);

            let TypeKind::Tuple(arg_tys) = args_ty.kind() else {
                panic!("const_eval_select: bad args ty");
            };

            // what we're left with is a pretty normal function call setup
            compiler.stack.align_for_call();
            let call_slot = compiler.stack.alloc_no_drop(res_ty);
            for (arg_ty, arg_offset) in arg_tys
                .iter()
                .zip(args_ty.layout().field_offsets.assert_single())
            {
                let arg_slot = compiler.stack.alloc_no_drop(*arg_ty);

                if let Some(copy) = bytecode_select::copy(
                    arg_slot,
                    source_slot.offset_by(*arg_offset as i32),
                    *arg_ty,
                ) {
                    compiler.out_bc.push(copy);
                }
            }
            compiler.out_bc.push(Instr::Call(call_slot, func));

            if let Some(copy) = bytecode_select::copy(out.slot, call_slot, res_ty) {
                compiler.out_bc.push(copy);
            }
        }
        "read_via_copy" | "volatile_load" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let arg_slot = args[0].slot;

            if let Some(copy) = bytecode_select::copy_from_ptr(out.slot, arg_slot, arg_ty, 0) {
                compiler.out_bc.push(copy);
            }
        }
        "write_via_move" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let ptr_slot = args[0].slot;
            let val_slot = args[1].slot;

            if let Some(copy) = bytecode_select::copy_to_ptr(ptr_slot, val_slot, arg_ty, 0) {
                compiler.out_bc.push(copy);
            }
        }
        "copy_nonoverlapping" => {
            // generally rust will check that the regions are not overlapping
            // no need to assert it here
            assert!(subs.list.len() == 1);
            assert!(args.len() == 3);

            let arg_ty = subs.list[0].assert_ty();
            let arg_size = arg_ty.layout().assert_size();

            if arg_size == 1 {
                compiler
                    .out_bc
                    .push(Instr::MemCopy(args[0].slot, args[1].slot, args[2].slot));
            } else {
                let usize_ty = compiler.vm.common_types().usize;
                let size_slot = compiler.stack.alloc_no_drop(usize_ty);

                let (mul_op, _) = bytecode_select::binary(BinaryOp::Mul, usize_ty);

                compiler.out_bc.push(bytecode_select::literal(
                    arg_size as _,
                    usize_ty.layout().assert_size(),
                    size_slot,
                ));
                compiler
                    .out_bc
                    .push(mul_op(size_slot, size_slot, args[2].slot));
                compiler
                    .out_bc
                    .push(Instr::MemCopy(args[0].slot, args[1].slot, size_slot));
                //panic!("non-trivial copy {}", arg_size);
            }
        }
        "write_bytes" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 3);

            let arg_ty = subs.list[0].assert_ty();
            let size = arg_ty.layout().assert_size();
            // todo it might be cleaner to multiply the size in bytecode, instead of having a limit attached to the instruction
            let small_size: u16 = size.try_into().expect("size too large!");

            compiler.out_bc.push(Instr::WriteBytes {
                size: small_size,
                dst: args[0].slot,
                val: args[1].slot,
                count: args[2].slot,
            });
        }
        "ctpop" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let arg_slot = args[0].slot;

            match arg_ty.layout().assert_size() {
                1 => compiler.out_bc.push(Instr::I8_PopCount(out.slot, arg_slot)),
                2 => compiler
                    .out_bc
                    .push(Instr::I16_PopCount(out.slot, arg_slot)),
                4 => compiler
                    .out_bc
                    .push(Instr::I32_PopCount(out.slot, arg_slot)),
                8 => compiler
                    .out_bc
                    .push(Instr::I64_PopCount(out.slot, arg_slot)),
                16 => compiler
                    .out_bc
                    .push(Instr::I128_PopCount(out.slot, arg_slot)),
                _ => panic!("can't ctpop {}", arg_ty),
            }
        }
        "cttz_nonzero" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let arg_slot = args[0].slot;

            match arg_ty.layout().assert_size() {
                1 => compiler
                    .out_bc
                    .push(Instr::I8_TrailingZeros(out.slot, arg_slot)),
                2 => compiler
                    .out_bc
                    .push(Instr::I16_TrailingZeros(out.slot, arg_slot)),
                4 => compiler
                    .out_bc
                    .push(Instr::I32_TrailingZeros(out.slot, arg_slot)),
                8 => compiler
                    .out_bc
                    .push(Instr::I64_TrailingZeros(out.slot, arg_slot)),
                16 => compiler
                    .out_bc
                    .push(Instr::I128_TrailingZeros(out.slot, arg_slot)),
                _ => panic!("can't cttz {}", arg_ty),
            }
        }
        "bitreverse" => select_unary_int!(ReverseBits),

        "rotate_left" => select_binary_int!(RotateLeft),
        "rotate_right" => select_binary_int!(RotateRight),

        //""
        "add_with_overflow" => select_binary_signed!(OverflowingAdd),
        "sub_with_overflow" => select_binary_signed!(OverflowingSub),

        "mul_with_overflow" => select_binary_signed!(OverflowingMul),

        "saturating_add" => select_binary_signed!(SatAdd),

        "abort" => {
            compiler
                .out_bc
                .push(Instr::Error(Box::new("abort".to_owned())));
        }
        "unreachable" => {
            compiler
                .out_bc
                .push(Instr::Error(Box::new("unreachable".to_owned())));
        } /*
        "caller_location" => {
        out_bc.push(Instr::Error(Box::new(
        "caller_location not implemented".to_owned(),
        )));
        }*/
        "assume" | "assert_zero_valid" | "assert_inhabited" => {
            // do nothing yeehaw
        }
        "unlikely" => {
            assert!(subs.list.len() == 0);
            assert!(args.len() == 1);

            let ty_bool = compiler.vm.common_types().bool;

            // copying here isn't so great, ideally we could make the res slot optional like in the main compiler
            let copy = bytecode_select::copy(out.slot, args[0].slot, ty_bool).unwrap();
            compiler.out_bc.push(copy);
        }
        "unchecked_add" | "wrapping_add" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Add, arg_ty);
            compiler
                .out_bc
                .push(ctor(out.slot, args[0].slot, args[1].slot));
        }
        "unchecked_sub" | "wrapping_sub" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Sub, arg_ty);
            compiler
                .out_bc
                .push(ctor(out.slot, args[0].slot, args[1].slot));
        }
        "unchecked_mul" | "wrapping_mul" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Mul, arg_ty);
            compiler
                .out_bc
                .push(ctor(out.slot, args[0].slot, args[1].slot));
        }
        "exact_div" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Div, arg_ty);
            compiler
                .out_bc
                .push(ctor(out.slot, args[0].slot, args[1].slot));
        }
        "unchecked_rem" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Rem, arg_ty);
            compiler
                .out_bc
                .push(ctor(out.slot, args[0].slot, args[1].slot));
        }
        "unchecked_shl" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::ShiftL, arg_ty);
            compiler
                .out_bc
                .push(ctor(out.slot, args[0].slot, args[1].slot));
        }
        "unchecked_shr" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::ShiftR, arg_ty);
            compiler
                .out_bc
                .push(ctor(out.slot, args[0].slot, args[1].slot));
        }
        // icky float intrinsics which aren't generic
        "minnumf32" => {
            assert!(args.len() == 2);
            compiler
                .out_bc
                .push(Instr::F32_Min(out.slot, args[0].slot, args[1].slot));
        }
        "maxnumf32" => {
            assert!(args.len() == 2);
            compiler
                .out_bc
                .push(Instr::F32_Max(out.slot, args[0].slot, args[1].slot));
        }
        "minnumf64" => {
            assert!(args.len() == 2);
            compiler
                .out_bc
                .push(Instr::F64_Min(out.slot, args[0].slot, args[1].slot));
        }
        "maxnumf64" => {
            assert!(args.len() == 2);
            compiler
                .out_bc
                .push(Instr::F64_Max(out.slot, args[0].slot, args[1].slot));
        }
        // we redirect `std::ptr::drop_in_place` to this intrinsic
        "drop_in_place" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let ref_ty = arg_ty.ref_to(crate::types::Mutability::Mut);

            if let Some(glue) = arg_ty.drop_info().glue() {
                // arg is not guaranteed to be at top of stack, correct this
                let call_slot = compiler
                    .stack
                    .alloc_no_drop(arg_ty.ref_to(crate::types::Mutability::Mut));
                compiler
                    .out_bc
                    .push(bytecode_select::copy(call_slot, args[0].slot, ref_ty).unwrap());
                compiler
                    .out_bc
                    .push(Instr::Call(call_slot, glue.function()));
            }
        }
        // special skitter-specific intrinsics
        "skitter_box_new" => {
            assert!(subs.list.len() == 1);
            assert!(args.len() == 1);

            let arg_ty = subs.list[0].assert_ty();
            let layout = arg_ty.layout();

            let size = layout.assert_size();
            if size == 0 {
                // dangling pointer
                compiler
                    .out_bc
                    .push(bytecode_select::literal(1, POINTER_SIZE.bytes(), out.slot));
            } else {
                compiler.out_bc.push(Instr::Alloc {
                    out: out.slot,
                    size: layout.assert_size(),
                    align: layout.align,
                });
                // do not move, drops are handled by caller
                compiler
                    .out_bc
                    .push(bytecode_select::copy_to_ptr(out.slot, args[0].slot, arg_ty, 0).unwrap());
            }
        }
        _ => {
            panic!("attempt compile intrinsic: {}{}", name, subs);
        }
    }
}
