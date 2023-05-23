use crate::{items::{TraitImpl, GenericCounts, CrateId}, types::{SubList, Sub, TypeKind, Type}, vm::{Function, VM, instr::{Instr, Slot}}, bytecode_compiler::CompilerStack, bytecode_select, abi::POINTER_SIZE};

#[derive(Copy,Clone)]
pub enum BuiltinTrait {
    Sized
}

impl BuiltinTrait {
    pub fn find<'vm>(&self, subs: &SubList<'vm>) -> Option<(TraitImpl<'vm>,SubList<'vm>)> {
        let dummy = || {
            let trait_impl = TraitImpl{
                bounds: Default::default(),
                child_fn_items: Default::default(),
                child_tys: Default::default(),
                crate_id: CrateId::new(0),
                for_types: subs.clone(),
                generics: GenericCounts{ lifetimes: 0, types: 0, consts: 0 }
            };
            (trait_impl,SubList{list:vec!()})
        };
    
        match self {
            BuiltinTrait::Sized => {
                assert!(subs.list.len() == 1);
                let Sub::Type(ty) = subs.list[0] else {
                    panic!("bad lookup for core::marker::Sized");
                };
                if ty.is_sized() {
                    Some(dummy())
                } else {
                    None
                }
            }
        }
    }
}

/// Initializes a rust intrinsic function, by either setting bytecode or setting a native function to call
pub fn compile_rust_intrinsic<'vm>(name: &str, subs: &SubList<'vm>, vm: &'vm VM<'vm>, out_bc: &mut Vec<Instr<'vm>>, stack: &mut CompilerStack, arg_slots: Vec<Slot>, out_slot: Slot) {
    match name {
        "transmute" => {
            assert!(subs.list.len() == 2);
            assert!(arg_slots.len() == 1);

            let arg = subs.list[0].assert_ty();
            let res = subs.list[1].assert_ty();

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
                out_bc.push(Instr::MovSS1(out_slot.offset_by(i), arg_slot.offset_by(size - i - 1)));
            }
        }
        "min_align_of" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 0);

            let arg_ty = subs.list[0].assert_ty();

            let res = arg_ty.layout().align;

            out_bc.push(bytecode_select::literal(res as _, POINTER_SIZE.bytes(), out_slot));
        }
        "size_of" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 0);

            let arg_ty = subs.list[0].assert_ty();

            let res = arg_ty.layout().assert_size();

            out_bc.push(bytecode_select::literal(res as _, POINTER_SIZE.bytes(), out_slot));
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
    
                out_bc.push(bytecode_select::literal(res as _, POINTER_SIZE.bytes(), out_slot));
            }
        }
        "size_of_val" => {
            assert!(subs.list.len() == 1);
            assert!(arg_slots.len() == 1);

            let arg_ty = subs.list[0].assert_ty();

            if let Some(size) = arg_ty.layout().maybe_size {
                out_bc.push(bytecode_select::literal(size as _, POINTER_SIZE.bytes(), out_slot));
            } else {
                let arg_slot = arg_slots[0];

                let ptr_size = POINTER_SIZE.bytes();

                let byte_slice_size = || {
                    bytecode_select::copy(out_slot, arg_slot.offset_by(ptr_size), vm.ty_usize()).unwrap()
                };
                
                match arg_ty.kind() {
                    TypeKind::Slice(child_ty) => {
                        let (mul_ctor,_) = bytecode_select::binary(crate::ir::BinaryOp::Mul, vm.ty_usize());

                        let elem_size = child_ty.layout().assert_size();

                        if elem_size == 1 {
                            out_bc.push(byte_slice_size());
                        } else {
                            out_bc.push(bytecode_select::literal(elem_size as _, ptr_size, out_slot));
                            out_bc.push(mul_ctor(out_slot,out_slot,arg_slot.offset_by(ptr_size)));
                        }
                    }
                    TypeKind::StringSlice => out_bc.push(byte_slice_size()),
                    _ => panic!("todo unsized value size")
                }
            };
        }
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
            let func = func.item.func_get(&func.subs);

            let TypeKind::Tuple(arg_tys) = args_ty.kind() else {
                panic!("const_eval_select: bad args ty");
            };

            stack.align_for_call();
            let call_slot = stack.alloc(res_ty);
            for (arg_ty,arg_offset) in arg_tys.iter().zip(&args_ty.layout().field_offsets[0]) {
                let arg_slot = stack.alloc(*arg_ty);
                
                if let Some(copy) = bytecode_select::copy(arg_slot, source_slot.offset_by(*arg_offset), *arg_ty) {
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
                _ => panic!("can't ctpop {}",arg_ty)
            }
        }
        _ => {
            panic!("attempt compile intrinsic: {}{}",name,subs);
        }
    }
}
