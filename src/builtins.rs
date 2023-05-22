use crate::{items::{TraitImpl, GenericCounts, CrateId}, types::{SubList, Sub, TypeKind}, vm::{Function, VM, instr::{Instr, Slot}}, bytecode_compiler::CompilerStack, bytecode_select, abi::POINTER_SIZE};

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
pub fn setup_rust_intrinsic<'vm>(vm: &'vm VM<'vm>, func: &Function<'vm>, name: &str, subs: &SubList<'vm>) {
    match name {
        "transmute" => {
            // this is pretty nasty and is filled with a bunch of possibly un-needed sanity checks
            // the best thing we could do is detect it while compiling the caller and convert it straight to a copy, which would remove a redundant copy
            assert!(subs.list.len() == 2);
            let arg = subs.list[0].assert_ty();
            let res = subs.list[1].assert_ty();

            let size = arg.layout().assert_size();

            assert_eq!(size,res.layout().assert_size());
            
            // must be at 0
            let res_slot = Slot::new(0);
            // must be here, since the sizes are equal and alignments are factors of the size
            let arg_slot = Slot::new(size);
            
            let mut bc = Vec::new();

            if let Some(copy) = bytecode_select::copy(res_slot, arg_slot, arg) {
                bc.push(copy);
            }
            bc.push(Instr::Return);

            let bc = vm.alloc_bytecode(bc);
            func.set_bytecode(bc);
        }
        "bswap" => {
            // also nasty, do we just want a specialized instruction for this? is it common enough to matter?
            assert!(subs.list.len() == 1);
            let arg_res = subs.list[0].assert_ty();

            let size = arg_res.layout().assert_size();

            // must be at 0
            let res_slot = Slot::new(0);
            // must be here, since the sizes are equal and alignments are factors of the size
            let arg_slot = Slot::new(size);

            let mut bc = Vec::new();

            for i in 0..size {
                bc.push(Instr::MovSS1(res_slot.offset_by(i), arg_slot.offset_by(size - i - 1)));
            }
            bc.push(Instr::Return);

            let bc = vm.alloc_bytecode(bc);
            func.set_bytecode(bc);
        }
        "min_align_of" => {
            // not super offensive
            assert!(subs.list.len() == 1);
            let arg = subs.list[0].assert_ty();

            let res = arg.layout().align;

            let bc = vec!(
                bytecode_select::literal(res as _, POINTER_SIZE.bytes(), Slot::new(0)),
                Instr::Return
            );

            let bc = vm.alloc_bytecode(bc);
            func.set_bytecode(bc);
        }
        "size_of" => {
            // not super offensive
            assert!(subs.list.len() == 1);
            let arg = subs.list[0].assert_ty();

            let res = arg.layout().assert_size();

            let bc = vec!(
                bytecode_select::literal(res as _, POINTER_SIZE.bytes(), Slot::new(0)),
                Instr::Return
            );

            let bc = vm.alloc_bytecode(bc);
            func.set_bytecode(bc);
        }
        "min_align_of_val" => {
            assert!(subs.list.len() == 1);
            let arg = subs.list[0].assert_ty();

            if let TypeKind::Dynamic = arg.kind() {
                panic!("todo, trait objects may have alignment only decidable at run-time");
            }

            let res = arg.layout().align;

            let bc = vec!(
                bytecode_select::literal(res as _, POINTER_SIZE.bytes(), Slot::new(0)),
                Instr::Return
            );

            let bc = vm.alloc_bytecode(bc);
            func.set_bytecode(bc);
        }
        "size_of_val" => {
            assert!(subs.list.len() == 1);
            let arg = subs.list[0].assert_ty();

            let bc = if let Some(size) = arg.layout().maybe_size {
                vec!(
                    bytecode_select::literal(size as _, POINTER_SIZE.bytes(), Slot::new(0)),
                    Instr::Return
                )
            } else {
                
                fn byte_slice_sizer<'vm>(vm: &'vm VM<'vm>) -> Vec<Instr<'vm>> {
                    let ptr_size = POINTER_SIZE.bytes();
                    vec!(
                        bytecode_select::copy(Slot::new(0), Slot::new(ptr_size*2), vm.ty_usize()).unwrap(),
                        Instr::Return
                    )
                }

                let ptr_size = POINTER_SIZE.bytes();
                
                match arg.kind() {
                    TypeKind::Slice(child_ty) => {
                        let (mul_ctor,_) = bytecode_select::binary(crate::ir::BinaryOp::Mul, vm.ty_usize());

                        let elem_size = child_ty.layout().assert_size();

                        if elem_size == 1 {
                            byte_slice_sizer(vm)
                        } else {
                            vec!(
                                bytecode_select::literal(elem_size as _, ptr_size, Slot::new(ptr_size*3)),
                                mul_ctor(Slot::new(0),Slot::new(ptr_size*2),Slot::new(ptr_size*3)),
                                Instr::Return
                            )
                        }
                    }
                    TypeKind::StringSlice => byte_slice_sizer(vm),
                    _ => panic!("todo unsized value size")
                }
            };

            let bc = vm.alloc_bytecode(bc);
            func.set_bytecode(bc);
        }
        _ => {
            println!("{}{}",name,subs);
            panic!("attempt setup intrinsic");
        }
    }
}
