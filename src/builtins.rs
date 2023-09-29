use std::sync::Arc;

use crate::{
    abi::POINTER_SIZE,
    bytecode_compiler::CompilerStack,
    bytecode_select,
    closure::FnTrait,
    crate_provider::TraitImplResult,
    ir::{glue_builder::glue_for_fn_trait, BinaryOp},
    items::{AssocValue, CrateId, GenericCounts, IRFlag, Item, ItemPath, TraitImpl},
    persist::Persist,
    types::{Mutability, Sub, SubList, Type, TypeKind},
    vm::{
        instr::{Instr, Slot},
        VM,
    },
};

#[derive(Copy, Clone, Debug)]
pub enum BuiltinTrait {
    Sized,
    Tuple,
    DiscriminantKind,
    FnOnce,
    FnMut,
    Fn,
}

impl<'vm> Persist<'vm> for BuiltinTrait {
    fn persist_write(&self, writer: &mut crate::persist::PersistWriter<'vm>) {
        let b = match self {
            Self::Sized => 0,
            Self::Tuple => 1,
            Self::DiscriminantKind => 2,
            Self::FnOnce => 3,
            Self::FnMut => 4,
            Self::Fn => 5,
        };
        writer.write_byte(b);
    }

    fn persist_read(reader: &mut crate::persist::PersistReader<'vm>) -> Self {
        let b = reader.read_byte();
        match b {
            0 => Self::Sized,
            1 => Self::Tuple,
            2 => Self::DiscriminantKind,
            3 => Self::FnOnce,
            4 => Self::FnMut,
            5 => Self::Fn,
            _ => panic!(),
        }
    }
}

/// Used for marker trait impls.
fn trait_impl_empty<'vm>() -> TraitImplResult<'vm> {
    TraitImplResult {
        crate_id: CrateId::new(0),
        impl_subs: SubList { list: vec![] },
        assoc_values: Arc::new([]),
    }
}

fn trait_impl<'vm>(
    impl_subs: SubList<'vm>,
    trait_item: &Item<'vm>,
    pairs: &[(ItemPath, AssocValue<'vm>)],
) -> TraitImplResult<'vm> {
    let assoc_values = trait_item.trait_build_assoc_values_for_impl(pairs);

    TraitImplResult {
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
    ) -> Option<TraitImplResult<'vm>> {
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
                    if let Some(discrim_ty) = ty.adt_info().discriminant_ty() {
                        return Some(trait_impl(
                            for_tys.clone(),
                            trait_item,
                            &[(
                                ItemPath::for_type("Discriminant"),
                                AssocValue::Type(discrim_ty),
                            )],
                        ));
                    }
                }
                None
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
                                    vm,
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
                                    vm,
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
                                let call_ir = glue_for_fn_trait(
                                    FnTrait::Fn,
                                    func_ty,
                                    fn_args_ty,
                                    sig.output,
                                    vm,
                                );

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
                                    vm,
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
                                    vm,
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
                                let call_ir = glue_for_fn_trait(
                                    FnTrait::Fn,
                                    func_ty,
                                    fn_args_ty,
                                    sig.output,
                                    vm,
                                );

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
                    TypeKind::Closure(closure, closure_sig, closure_subs) => {
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
    vm: &'vm VM<'vm>,
    out_bc: &mut Vec<Instr<'vm>>,
    stack: &mut CompilerStack,
    arg_slots: Vec<Slot>,
    out_slot: Slot,
) {
    match name {
        "transmute" => {
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
                    out_slot.offset_by(i),
                    arg_slot.offset_by(size - i - 1),
                ));
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

            if let TypeKind::Dynamic = arg_ty.kind() {
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
                    bytecode_select::copy(out_slot, arg_slot.offset_by(ptr_size), ty_usize).unwrap()
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
                            out_bc.push(mul_ctor(out_slot, out_slot, arg_slot.offset_by(ptr_size)));
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
            let discriminant_ty = arg_ty
                .adt_info()
                .discriminant_ty()
                .expect("missing discriminator type");

            out_bc.push(
                bytecode_select::copy_from_ptr(out_slot, arg_slots[0], discriminant_ty, 0).unwrap(),
            );
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
            for (arg_ty, arg_offset) in arg_tys.iter().zip(&args_ty.layout().field_offsets[0]) {
                let arg_slot = stack.alloc(*arg_ty);

                if let Some(copy) =
                    bytecode_select::copy(arg_slot, source_slot.offset_by(*arg_offset), *arg_ty)
                {
                    out_bc.push(copy);
                }
            }
            out_bc.push(Instr::Call(call_slot, func));

            if let Some(copy) = bytecode_select::copy(out_slot, call_slot, res_ty) {
                out_bc.push(copy);
            }
        }
        "read_via_copy" => {
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
        "assert_zero_valid" | "assert_inhabited" => {
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
            // TODO make this actually work, probably just implement more arithmetic instructions :(
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let carry_slot = out_slot.offset_by(arg_ty.layout().assert_size());

            let (ctor, _) = bytecode_select::binary(BinaryOp::Add, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
            out_bc.push(bytecode_select::literal(0, 1, carry_slot));
            // signed: same signs on inputs, differ from output
            // unsigned: (a + b < a)
        }
        "wrapping_add" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 2);

            let arg_ty = subs.list[0].assert_ty();
            let (ctor, _) = bytecode_select::binary(BinaryOp::Add, arg_ty);
            out_bc.push(ctor(out_slot, arg_slots[0], arg_slots[1]));
        }
        _ => {
            panic!("attempt compile intrinsic: {}{}", name, subs);
        }
    }
}
