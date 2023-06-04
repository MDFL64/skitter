use crate::abi::POINTER_SIZE;
use crate::builtins::compile_rust_intrinsic;
use crate::bytecode_select;
use crate::ir::{
    BinaryOp, BindingMode, ExprId, ExprKind, IRFunction, LogicOp, Pattern, PatternKind,
    PointerCast, Stmt, LoopId, PatternId, Block,
};
use crate::items::FunctionAbi;
use crate::types::{Mutability, SubList, Type, TypeKind};
use crate::vm::instr::Instr;
use crate::vm::{self, instr::Slot};

pub struct BytecodeCompiler<'vm, 'f> {
    in_func_subs: &'f SubList<'vm>,
    in_func: &'f IRFunction<'vm>,
    out_bc: Vec<Instr<'vm>>,
    vm: &'vm vm::VM<'vm>,
    stack: CompilerStack,
    locals: Vec<(u32, Slot)>,
    loops: Vec<LoopInfo>,
    loop_breaks: Vec<BreakInfo>,
}

struct LoopInfo {
    loop_id: LoopId,
    result_slot: Slot,
    start_index: usize,
}

struct BreakInfo {
    loop_id: LoopId,
    break_index: usize,
}

impl<'vm, 'f> BytecodeCompiler<'vm, 'f> {
    pub fn compile(
        vm: &'vm vm::VM<'vm>,
        ir: &'f IRFunction<'vm>,
        subs: &'f SubList<'vm>,
        path: &str,
    ) -> Vec<Instr<'vm>> {
        if vm.is_verbose {
            println!("compiling {:?} for {}", path, subs);
        }

        let mut compiler = BytecodeCompiler {
            in_func_subs: subs,
            in_func: &ir,
            out_bc: Vec::new(),
            vm,
            stack: Default::default(),
            locals: Vec::new(),
            loops: Vec::new(),
            loop_breaks: Vec::new(),
        };

        let out_ty = compiler.apply_subs(ir.sig.output);

        let ret_slot = compiler.stack.alloc(out_ty);
        assert_eq!(ret_slot.index(), 0);
        let param_slots: Vec<Slot> = ir
            .params
            .iter()
            .map(|pat| compiler.alloc_pattern(*pat))
            .collect();

        for (pat, slot) in ir.params.iter().zip(param_slots) {
            compiler.match_pattern(*pat, slot, None);
        }

        compiler.lower_expr(ir.root_expr, Some(Slot::new(0)));
        compiler.out_bc.push(Instr::Return);

        if compiler.vm.is_verbose {
            for (i, bc) in compiler.out_bc.iter().enumerate() {
                println!("  {} {:?}", i, bc);
            }
        }

        //println!("{:?}",compiler.out_bc);
        compiler.out_bc
    }

    fn debug<S: Into<String>>(&mut self, f: impl Fn() -> S) {
        if self.vm.is_verbose {
            let bc = Instr::Debug(Box::new(f().into()));
            self.out_bc.push(bc);
        }
    }

    fn lower_block(&mut self, block: &Block, dst_slot: Option<Slot>) -> Slot {
        for stmt in block.stmts.iter() {
            self.lower_stmt(stmt);
        }
        if let Some(expr) = block.result {
            // ALWAYS have a slot allocated for the final expression
            // prevents wrongly aliasing locals - TODO we may want to allocate this up-front for saner stack layout
            let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(self.expr_ty(expr)));

            self.lower_expr(expr, Some(dst_slot))
        } else {
            dst_slot.unwrap_or(Slot::DUMMY)
        }
    }

    fn lower_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                pattern,
                init,
                else_block,
                ..
            } => {
                assert!(else_block.is_none());
                let dst_slot = self.alloc_pattern(*pattern);
                if let Some(init) = init {
                    self.lower_expr(*init, Some(dst_slot));
                }
                self.match_pattern(*pattern, dst_slot, None);
            }
            Stmt::Expr(expr) => {
                self.lower_expr(*expr, None);
            }
        }
    }

    fn lower_expr(&mut self, id: ExprId, dst_slot: Option<Slot>) -> Slot {
        let expr = self.in_func.expr(id);
        let expr_ty = self.expr_ty(id);
        //println!("lower {:?} :: {}",expr.kind,expr.ty);

        match &expr.kind {
            ExprKind::Dummy(value) => {
                self.lower_expr(*value, dst_slot)
            }
            ExprKind::Block(block) => self.lower_block(block, dst_slot),
            // PLACES:
            ExprKind::NamedConst(_)
            | ExprKind::VarRef { .. }
            | ExprKind::Field { .. }
            | ExprKind::Index { .. } => match self.expr_to_place(id) {
                Place::Local(local_slot) => {
                    if let Some(dst_slot) = dst_slot {
                        if let Some(instr) = bytecode_select::copy(dst_slot, local_slot, expr_ty) {
                            self.out_bc.push(instr);
                        }

                        dst_slot
                    } else {
                        local_slot
                    }
                }
                Place::Ptr(ptr_slot, offset) => {
                    let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                    if let Some(instr) =
                        bytecode_select::copy_from_ptr(dst_slot, ptr_slot, expr_ty, offset)
                    {
                        self.out_bc.push(instr);
                    }

                    dst_slot
                }
            },
            ExprKind::LiteralValue(n) => {
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                self.out_bc.push(bytecode_select::literal(
                    *n,
                    expr_ty.layout().assert_size(),
                    dst_slot,
                ));
                dst_slot
            }
            ExprKind::LiteralVoid => dst_slot.unwrap_or_else(|| Slot::DUMMY),
            ExprKind::LiteralBytes(bytes) => {
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                let ptr = bytes.as_ptr() as usize;
                let len = bytes.len();

                let ptr_size = POINTER_SIZE.bytes();

                self.out_bc
                    .push(bytecode_select::literal(ptr as i128, ptr_size, dst_slot));
                self.out_bc.push(bytecode_select::literal(
                    len as i128,
                    ptr_size,
                    dst_slot.offset_by(ptr_size),
                ));

                dst_slot
            }
            ExprKind::Unary(op, arg) => {
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                let arg_slot = self.lower_expr(*arg, None);

                let arg_ty = self.expr_ty(*arg);

                let ctor = bytecode_select::unary(*op, arg_ty);
                self.out_bc.push(ctor(dst_slot, arg_slot));
                dst_slot
            }
            ExprKind::Cast(source) => {
                let arg_ty = self.expr_ty(*source);
                let dst_ty = expr_ty;

                // Sometimes we will encounter a no-op cast.
                if arg_ty == dst_ty {
                    let arg_slot = self.lower_expr(*source, None);

                    if let Some(dst_slot) = dst_slot {
                        self.out_bc.push(bytecode_select::copy(dst_slot, arg_slot, arg_ty).unwrap());
                        dst_slot
                    } else {
                        arg_slot
                    }
                } else {
                    let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));
    
                    let arg_slot = self.lower_expr(*source, None);
    
                    let ctor = bytecode_select::cast(arg_ty, dst_ty);
                    self.out_bc.push(ctor(dst_slot, arg_slot));
                    dst_slot
                }

            }
            ExprKind::Binary(op, lhs, rhs) => {
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                let lhs_slot = self.lower_expr(*lhs, None);
                let rhs_slot = self.lower_expr(*rhs, None);

                let lhs_ty = self.expr_ty(*lhs);

                let (ctor, swap) = bytecode_select::binary(*op, lhs_ty);
                if swap {
                    self.out_bc.push(ctor(dst_slot, rhs_slot, lhs_slot));
                } else {
                    self.out_bc.push(ctor(dst_slot, lhs_slot, rhs_slot));
                }
                dst_slot
            }
            ExprKind::Assign(lhs, rhs) => {
                let rhs_slot = self.lower_expr(*rhs, None);

                let assign_ty = self.expr_ty(*lhs);

                match self.expr_to_place(*lhs) {
                    Place::Local(lhs_slot) => {
                        if let Some(instr) = bytecode_select::copy(lhs_slot, rhs_slot, assign_ty) {
                            self.out_bc.push(instr);
                        }
                    }
                    Place::Ptr(ptr_slot, offset) => {
                        if let Some(instr) =
                            bytecode_select::copy_to_ptr(ptr_slot, rhs_slot, assign_ty, offset)
                        {
                            self.out_bc.push(instr);
                        }
                    }
                }

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::AssignOp(op, lhs, rhs) => {
                let rhs_slot = self.lower_expr(*rhs, None);

                let lhs_ty = self.expr_ty(*lhs);

                let (ctor, swap) = bytecode_select::binary(*op, lhs_ty);
                assert!(!swap);

                match self.expr_to_place(*lhs) {
                    Place::Local(lhs_slot) => {
                        self.out_bc.push(ctor(lhs_slot, lhs_slot, rhs_slot));
                    }
                    Place::Ptr(ptr_slot, offset) => {
                        let assign_ty = self.expr_ty(*lhs);

                        let tmp_slot = self.stack.alloc(assign_ty);

                        self.out_bc.push(
                            bytecode_select::copy_from_ptr(tmp_slot, ptr_slot, assign_ty, offset)
                                .unwrap(),
                        );
                        self.out_bc.push(ctor(tmp_slot, tmp_slot, rhs_slot));
                        self.out_bc.push(
                            bytecode_select::copy_to_ptr(ptr_slot, tmp_slot, assign_ty, offset)
                                .unwrap(),
                        );
                    }
                }

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Call {
                func,
                args,
            } => {
                let ty = self.expr_ty(*func);
                let func_ref = ty.func_item().expect("can't find function");

                if let Some((FunctionAbi::RustIntrinsic, extern_name)) = func_ref.item.func_extern()
                {
                    let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                    let arg_slots = args.iter().map(|arg| self.lower_expr(*arg, None)).collect();

                    compile_rust_intrinsic(
                        extern_name,
                        &func_ref.subs,
                        self.vm,
                        &mut self.out_bc,
                        &mut self.stack,
                        arg_slots,
                        dst_slot,
                    );

                    dst_slot
                } else {
                    let func = func_ref.item.func_mono(&func_ref.subs);
                    let call_start_instr = self.out_bc.len();

                    // evaluate arguments and write them to a virtual call frame
                    let mut call_frame = CallFrame::default();
                    call_frame.alloc(expr_ty);
                    for arg in args.iter() {
                        let arg_ty = self.expr_ty(*arg);
                        let arg_slot = call_frame.alloc(arg_ty);
                        self.lower_expr(*arg, Some(arg_slot));
                    }

                    // set up real call frame
                    let base_slot = self.stack.align_for_call();
                    let ret_slot = self.stack.alloc(expr_ty);
                    assert_eq!(ret_slot, base_slot);

                    for arg in args.iter() {
                        let arg_ty = self.expr_ty(*arg);
                        self.stack.alloc(arg_ty);
                    }

                    // amend the previous code to write args into the correct slots
                    for index in call_start_instr..self.out_bc.len() {
                        self.out_bc[index].replace_arg_sub(base_slot);
                    }

                    self.out_bc.push(Instr::Call(ret_slot, func));
                    if let Some(dst_slot) = dst_slot {
                        if let Some(instr) = bytecode_select::copy(dst_slot, ret_slot, expr_ty) {
                            self.out_bc.push(instr);
                        }
                        dst_slot
                    } else {
                        ret_slot
                    }
                }
            }
            ExprKind::If {
                cond,
                then,
                else_opt,
                ..
            } => {
                let cond_slot = self.lower_expr(*cond, None);

                let jump_index_1 = self.skip_instr();

                if let Some(else_expr) = else_opt {
                    let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                    self.lower_expr(*then, Some(dst_slot));

                    let jump_index_2 = self.skip_instr();

                    let jump_offset_1 = -self.get_jump_offset(jump_index_1);
                    self.out_bc[jump_index_1] = Instr::JumpF(jump_offset_1, cond_slot);

                    self.lower_expr(*else_expr, Some(dst_slot));

                    let jump_offset_2 = -self.get_jump_offset(jump_index_2);
                    self.out_bc[jump_index_2] = Instr::Jump(jump_offset_2);

                    dst_slot
                } else {
                    // note: dst_slot should be void
                    let res = self.lower_expr(*then, dst_slot);

                    let jump_offset_1 = -self.get_jump_offset(jump_index_1);
                    self.out_bc[jump_index_1] = Instr::JumpF(jump_offset_1, cond_slot);

                    res
                }
            }
            ExprKind::LogicOp(op, lhs, rhs) => {
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                self.lower_expr(*lhs, Some(dst_slot));

                let jump_index_1 = self.skip_instr();

                self.lower_expr(*rhs, Some(dst_slot));

                let jump_offset_1 = -self.get_jump_offset(jump_index_1);

                match op {
                    LogicOp::And => {
                        self.out_bc[jump_index_1] = Instr::JumpF(jump_offset_1, dst_slot);
                    }
                    LogicOp::Or => {
                        self.out_bc[jump_index_1] = Instr::JumpT(jump_offset_1, dst_slot);
                    }
                }

                dst_slot
            }
            ExprKind::Loop(body,loop_id) => {
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                let loop_index = self.out_bc.len();

                self.loops.push(LoopInfo {
                    loop_id: *loop_id,
                    result_slot: dst_slot,
                    start_index: loop_index,
                });
                let loop_count = self.loops.len();

                self.lower_block(body, Some(Slot::DUMMY));

                assert_eq!(loop_count, self.loops.len());
                self.loops.pop();

                // ending jump
                let offset = self.get_jump_offset(loop_index);
                self.out_bc.push(Instr::Jump(offset));

                // fix breaks
                let break_indices: Vec<_> = self
                    .loop_breaks
                    .drain_filter(|break_info| break_info.loop_id == *loop_id)
                    .map(|break_info| break_info.break_index)
                    .collect();

                for break_index in break_indices {
                    let offset = -self.get_jump_offset(break_index);
                    self.out_bc[break_index] = Instr::Jump(offset);
                }

                dst_slot
            }
            ExprKind::Break { loop_id, value } => {

                if let Some(value) = value {
                    let loop_slot = self
                        .loops
                        .iter()
                        .find(|x| x.loop_id == *loop_id)
                        .unwrap()
                        .result_slot;
                    self.lower_expr(*value, Some(loop_slot));
                }

                let break_index = self.skip_instr(); //self.out_bc.len();
                self.loop_breaks.push(BreakInfo {
                    loop_id: *loop_id,
                    break_index,
                });

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Continue { loop_id } => {

                let loop_start = self
                    .loops
                    .iter()
                    .find(|x| x.loop_id == *loop_id)
                    .unwrap()
                    .start_index;

                let offset = self.get_jump_offset(loop_start) + 1;
                self.out_bc.push(Instr::Jump(offset));

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Return(value) => {
                if let Some(value) = value {
                    self.lower_expr(*value, Some(Slot::new(0)));
                }
                self.out_bc.push(Instr::Return);

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Ref(arg, ..) => {
                let place = self.expr_to_place(*arg);

                match place {
                    Place::Local(src_slot) => {
                        let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                        self.out_bc.push(Instr::SlotAddr(dst_slot, src_slot));
                        dst_slot
                    }
                    Place::Ptr(src_slot, offset) => {
                        //println!("? {:?}",expr_ty);

                        let ref_size = expr_ty.layout().assert_size();

                        if offset != 0 {
                            // todo fat pointers
                            assert!(ref_size == POINTER_SIZE.bytes());

                            let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                            self.out_bc
                                .push(Instr::PointerOffset2(dst_slot, src_slot, offset));
                            dst_slot
                        } else {
                            if let Some(dst_slot) = dst_slot {
                                self.out_bc.push(
                                    bytecode_select::copy(dst_slot, src_slot, expr_ty).unwrap(),
                                );

                                dst_slot
                            } else {
                                src_slot
                            }
                        }
                    }
                }
            }
            ExprKind::DeRef(arg) => {
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                let ptr = self.lower_expr(*arg, None);

                if let Some(instr) = bytecode_select::copy_from_ptr(dst_slot, ptr, expr_ty, 0) {
                    self.out_bc.push(instr);
                }

                dst_slot
            }
            ExprKind::PointerCast(source, cast) => {
                match cast {
                    PointerCast::UnSize => {
                        assert_eq!(expr_ty.layout().assert_size(), POINTER_SIZE.bytes() * 2);

                        let src_ty = self.expr_ty(*source);

                        let src_kind = match src_ty.kind() {
                            TypeKind::Ptr(ref_ty, _) | TypeKind::Ref(ref_ty, _) => ref_ty.kind(),
                            _ => panic!(),
                        };

                        let meta = match src_kind {
                            TypeKind::Array(_, elem_count) => elem_count.assert_static() as usize,
                            _ => panic!("unsized cast ptr to {:?}", src_kind),
                        };

                        let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                        let src_slot = self.lower_expr(*source, None);

                        // copy base
                        let ptr_size = POINTER_SIZE.bytes();
                        self.out_bc.push(
                            bytecode_select::copy(dst_slot, src_slot, self.vm.ty_usize()).unwrap(),
                        );
                        self.out_bc.push(bytecode_select::literal(
                            meta as i128,
                            ptr_size,
                            dst_slot.offset_by(ptr_size),
                        ));
                    }
                    PointerCast::MutToConstPointer => {
                        // a simple copy
                        let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                        let src_slot = self.lower_expr(*source, None);

                        self.out_bc
                            .push(bytecode_select::copy(dst_slot, src_slot, expr_ty).unwrap());
                    }
                    _ => panic!("todo ptr cast {:?}", cast),
                }

                self.lower_expr(*source, dst_slot)
            }
            ExprKind::Tuple(fields) => {
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                for (field, offset) in fields.iter().zip(expr_ty.layout().field_offsets[0].iter()) {
                    let field_slot = dst_slot.offset_by(*offset);
                    self.lower_expr(*field, Some(field_slot));
                }

                dst_slot
            }
            ExprKind::Array(fields) => {
                let TypeKind::Array(elem_ty,_) = expr_ty.kind() else {
                    panic!("bad array layout");
                };

                let elem_size = elem_ty.layout().assert_size();

                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                for (i, field) in fields.iter().enumerate() {
                    let field_slot = dst_slot.offset_by(i as u32 * elem_size);
                    self.lower_expr(*field, Some(field_slot));
                }

                dst_slot
            }
            ExprKind::Adt { variant, fields } => {
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                if let Some(discriminator_ty) = expr_ty.get_adt_discriminator_ty() {
                    let dl = discriminator_ty.layout();
                    self.out_bc.push(bytecode_select::literal(
                        *variant as i128,
                        dl.assert_size(),
                        dst_slot,
                    ));
                }

                let expr_layout = expr_ty.layout();

                for (field_index, expr) in fields.iter() {
                    let offset =
                        expr_layout.field_offsets[*variant as usize][*field_index as usize];
                    let field_slot = dst_slot.offset_by(offset);
                    self.lower_expr(*expr, Some(field_slot));
                }

                dst_slot
            }
            ExprKind::Let { pattern, init } => {
                // the bool result
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                // allocate space for our value, to avoid aliasing other variables
                let value_slot = self.stack.alloc(self.expr_ty(*init));
                self.lower_expr(*init, Some(value_slot));

                self.match_pattern(*pattern, value_slot, Some(dst_slot));

                dst_slot
            }
            ExprKind::Match { arg, arms } => {
                // the result value
                let dst_slot = dst_slot.unwrap_or_else(|| self.stack.alloc(expr_ty));

                // allocate space for our value, to avoid aliasing other variables
                let arg_slot = self.stack.alloc(self.expr_ty(*arg));
                self.lower_expr(*arg, Some(arg_slot));

                // the bool pattern match result slot
                let bool_ty = self.vm.ty_bool();
                let match_result_slot = self.stack.alloc(bool_ty);

                let mut jump_gaps = Vec::new();

                for arm_id in arms {
                    panic!("todo fix match");
                    /*let arm = self.in_func.arm(*arm_id);
                    self.match_pattern(&arm.pattern, arg_slot, Some(match_result_slot));
                    let check_end_index = self.skip_instr();
                    self.lower_expr(arm.expr, Some(dst_slot));
                    jump_gaps.push(self.skip_instr());

                    self.out_bc[check_end_index] =
                        Instr::JumpF(-self.get_jump_offset(check_end_index), match_result_slot);*/
                }

                for gap_index in jump_gaps {
                    self.out_bc[gap_index] = Instr::Jump(-self.get_jump_offset(gap_index));
                }

                dst_slot
            }
            _ => panic!("todo lower expr {:?}", expr.kind),
        }
    }

    fn expr_to_place(&mut self, id: ExprId) -> Place {
        let expr = self.in_func.expr(id);

        match &expr.kind {
            ExprKind::Dummy(value) => self.expr_to_place(*value),
            ExprKind::DeRef(arg) => {
                let addr_slot = self.lower_expr(*arg, None);
                Place::Ptr(addr_slot, 0)
            }
            ExprKind::VarRef(local_id) => {
                let local_slot = self.find_local(*local_id);
                Place::Local(local_slot)
            }
            ExprKind::Field {
                lhs,
                variant,
                field,
            } => {
                assert!(*variant == 0);

                let layout = self.expr_ty(*lhs).layout();
                let field_offset = layout.field_offsets[*variant as usize][*field as usize];

                match self.expr_to_place(*lhs) {
                    Place::Local(base_slot) => Place::Local(base_slot.offset_by(field_offset)),
                    Place::Ptr(ptr_slot, offset) => Place::Ptr(ptr_slot, offset + field_offset),
                }
            }
            ExprKind::Index { lhs, index } => {
                let index_ty = self.expr_ty(*index);
                assert!(index_ty.layout().assert_size() == POINTER_SIZE.bytes());

                // this index slot is re-used for the resulting pointer
                let index_slot = self.stack.alloc(index_ty);
                self.lower_expr(*index, Some(index_slot));

                let lhs_ty = self.expr_ty(*lhs);
                let lhs_kind = lhs_ty.kind();
                match lhs_kind {
                    TypeKind::Array(elem_ty, elem_count) => {
                        let elem_size = elem_ty.layout().assert_size();
                        let elem_count = elem_count.assert_static();

                        self.out_bc.push(Instr::IndexCalc {
                            arg_out: index_slot,
                            elem_size,
                            elem_count,
                        });

                        match self.expr_to_place(*lhs) {
                            Place::Local(base_slot) => {
                                self.out_bc.push(Instr::SlotAddrOffset {
                                    out: index_slot,
                                    arg: base_slot,
                                    offset: index_slot,
                                });
                            }
                            Place::Ptr(ptr_slot, offset) => {
                                self.out_bc
                                    .push(Instr::PointerOffset3(index_slot, ptr_slot, offset));
                            }
                        }
                    }
                    TypeKind::Slice(elem_ty) => {
                        let elem_size = elem_ty.layout().assert_size();

                        let lhs_place = self.expr_to_place(*lhs);
                        if let Place::Ptr(ptr_slot, offset) = lhs_place {
                            assert_eq!(offset, 0);
                            let elem_count = ptr_slot.offset_by(POINTER_SIZE.bytes());
                            self.out_bc.push(Instr::IndexCalcDyn {
                                arg_out: index_slot,
                                elem_size,
                                elem_count,
                            });
                            self.out_bc
                                .push(Instr::PointerOffset3(index_slot, ptr_slot, 0));
                        } else {
                            panic!("invalid local(?!) slice index");
                        }
                    }
                    _ => panic!("cannot index {:?}", lhs_kind),
                }
                Place::Ptr(index_slot, 0)
            }
            ExprKind::NamedConst(const_ref) => {
                let ty = self.expr_ty(id);
                let ptr_ty = ty.ref_to(Mutability::Const);
                let ptr_size = ptr_ty.layout().assert_size();

                let const_ptr = const_ref.item.const_value(&const_ref.subs).as_ptr() as usize;

                let ptr_slot = self.stack.alloc(ty);
                self.out_bc.push(bytecode_select::literal(
                    const_ptr as i128,
                    ptr_size,
                    ptr_slot,
                ));

                Place::Ptr(ptr_slot, 0)
            }
            // (todo) All other expressions without special handling:
            ExprKind::Block { .. }
            | ExprKind::Tuple { .. }
            | ExprKind::Binary { .. }
            | ExprKind::Ref { .. }
            | ExprKind::LiteralVoid
            | ExprKind::Array(_)
            | ExprKind::Adt { .. }
            | ExprKind::LiteralValue { .. } => {
                // TODO it may be necessary to pre-allocate a slot here to avoid aliasing locals
                let res = self.lower_expr(id, None);
                Place::Local(res)
            }
            _ => panic!("expr_to_place {:?}", expr.kind),
        }
    }

    fn apply_subs(&self, ty: Type<'vm>) -> Type<'vm> {
        let res = ty.sub(self.in_func_subs);
        //println!("sub {} with {} => {}",ty,self.in_func_subs,res);
        res
    }

    fn expr_ty(&self, id: ExprId) -> Type<'vm> {
        let ty = self.in_func.expr(id).ty;
        self.apply_subs(ty)
    }

    fn alloc_pattern(&mut self, pat_id: PatternId) -> Slot {
        let pat = self.in_func.pattern(pat_id);

        let ty = self.apply_subs(pat.ty);
        let slot = self.stack.alloc(ty);
        slot
    }

    /// Use to both setup variable bindings AND generate pattern matching code.
    fn match_pattern(&mut self, pat_id: PatternId, source: Slot, match_result_slot: Option<Slot>) {

        let pat = self.in_func.pattern(pat_id);

        let refutable =
            self.match_pattern_internal(pat, Place::Local(source), false, match_result_slot);

        if let Some(match_result_slot) = match_result_slot {
            if !refutable {
                self.out_bc.push(Instr::I8_Const(match_result_slot, 1));
            }
        }
    }

    /// If must_copy is true, we must copy values from the source. Otherwise we are free to re-use locals.
    /// The returned value indicates whether the pattern is refutable.
    /// If not, match_result_slot will NOT be written to, and the caller must assume the match was successful
    fn match_pattern_internal(
        &mut self,
        pat: &Pattern<'vm>,
        source: Place,
        must_copy: bool,
        match_result_slot: Option<Slot>,
    ) -> bool {

        match &pat.kind {
            PatternKind::LocalBinding {
                local_id,
                mode,
                sub_pattern,
            } => {
                match (mode, source) {
                    (BindingMode::Value, Place::Local(source_slot)) => {
                        // copy values if there are possible aliases
                        if must_copy || sub_pattern.is_some() {
                            let ty = self.apply_subs(pat.ty);
                            let var_slot = self.find_or_alloc_local(*local_id, ty);
                            if let Some(instr) = bytecode_select::copy(var_slot, source_slot, ty) {
                                self.out_bc.push(instr);
                            }
                        } else {
                            self.assert_local_undef(*local_id);
                            self.locals.push((*local_id, source_slot));
                        }
                    }
                    (BindingMode::Value, Place::Ptr(ref_slot, ref_offset)) => {
                        // copy value from pointer
                        let ty = self.apply_subs(pat.ty);
                        let var_slot = self.find_or_alloc_local(*local_id, ty);
                        if let Some(instr) =
                            bytecode_select::copy_from_ptr(var_slot, ref_slot, ty, ref_offset)
                        {
                            self.out_bc.push(instr);
                        }
                    }
                    (BindingMode::Ref, Place::Local(source_slot)) => {
                        // ref to local
                        let ref_ty = self.apply_subs(pat.ty);
                        let var_slot = self.find_or_alloc_local(*local_id, ref_ty);

                        self.out_bc.push(Instr::SlotAddr(var_slot, source_slot));
                    }
                    (BindingMode::Ref, Place::Ptr(ref_slot, ref_offset)) => {
                        // offset pointer
                        let ref_ty = self.apply_subs(pat.ty);
                        let var_slot = self.find_or_alloc_local(*local_id, ref_ty);

                        self.out_bc
                            .push(Instr::PointerOffset2(var_slot, ref_slot, ref_offset));
                    }
                }
                if let Some(sub_pattern) = sub_pattern {
                    // copy values if there are possible aliases
                    let sub_pattern = self.in_func.pattern(*sub_pattern);
                    self.match_pattern_internal(sub_pattern, source, true, match_result_slot)
                } else {
                    false
                }
            }
            PatternKind::Struct { fields } => {
                let layout = self.apply_subs(pat.ty).layout();

                let mut jump_gaps = Vec::new();
                let mut result = false;

                for field in fields {
                    let field_pattern = self.in_func.pattern(field.pattern);

                    let offset = layout.field_offsets[0][field.field as usize];
                    let refutable = self.match_pattern_internal(
                        field_pattern,
                        source.offset_by(offset),
                        must_copy,
                        match_result_slot,
                    );
                    result |= refutable;

                    if refutable && match_result_slot.is_some() {
                        jump_gaps.push(self.skip_instr());
                    }
                }

                // cut out unnecessary jumps
                if let Some(last_gap) = jump_gaps.last() {
                    if *last_gap == self.out_bc.len() - 1 {
                        jump_gaps.pop();
                        self.out_bc.pop();
                    }
                }

                if let Some(match_result_slot) = match_result_slot {
                    for gap_index in jump_gaps.iter() {
                        let gap_offset = -self.get_jump_offset(*gap_index);
                        self.out_bc[*gap_index] = Instr::JumpF(gap_offset, match_result_slot);
                    }
                }

                result
            }
            PatternKind::Enum {
                fields,
                variant_index,
            } => {
                let layout = self.apply_subs(pat.ty).layout();

                let mut jump_gaps = Vec::new();

                // test the discriminator by building a fake literal pattern
                {
                    let discriminator_ty = pat
                        .ty
                        .get_adt_discriminator_ty()
                        .expect("no discriminator type");
                    let fake_pattern = Pattern {
                        kind: PatternKind::LiteralValue(*variant_index as i128),
                        ty: discriminator_ty,
                    };
                    self.match_pattern_internal(
                        &fake_pattern,
                        source,
                        must_copy,
                        match_result_slot,
                    );
                    jump_gaps.push(self.skip_instr());
                }

                for field in fields {
                    let field_pattern = self.in_func.pattern(field.pattern);

                    let offset =
                        layout.field_offsets[*variant_index as usize][field.field as usize];
                    
                    let refutable = self.match_pattern_internal(
                        field_pattern,
                        source.offset_by(offset),
                        must_copy,
                        match_result_slot,
                    );

                    if refutable && match_result_slot.is_some() {
                        jump_gaps.push(self.skip_instr());
                    }
                }

                // cut out unnecessary jumps
                if let Some(last_gap) = jump_gaps.last() {
                    if *last_gap == self.out_bc.len() - 1 {
                        jump_gaps.pop();
                        self.out_bc.pop();
                    }
                }

                if let Some(match_result_slot) = match_result_slot {
                    for gap_index in jump_gaps.iter() {
                        let gap_offset = -self.get_jump_offset(*gap_index);
                        self.out_bc[*gap_index] = Instr::JumpF(gap_offset, match_result_slot);
                    }
                }

                // we could technically optimize for the case of single-variant enums, but why?
                true
            }
            PatternKind::Or { options } => {
                let mut jump_gaps = Vec::new();
                let mut result = true;

                assert!(options.len() >= 2);

                for option in options {
                    let option = self.in_func.pattern(*option);

                    // always copy values since a local may appear multiple times
                    let refutable =
                        self.match_pattern_internal(option, source, true, match_result_slot);
                    if !refutable {
                        // irrefutable pattern, no point in testing anything else
                        result = false;
                        break;
                    } else {
                        jump_gaps.push(self.skip_instr());
                    }
                }

                // cut out unnecessary jumps
                if let Some(last_gap) = jump_gaps.last() {
                    if *last_gap == self.out_bc.len() - 1 {
                        jump_gaps.pop();
                        self.out_bc.pop();
                    }
                }

                if let Some(match_result_slot) = match_result_slot {
                    for gap_index in jump_gaps.iter() {
                        let gap_offset = -self.get_jump_offset(*gap_index);
                        self.out_bc[*gap_index] = Instr::JumpT(gap_offset, match_result_slot);
                    }
                } else {
                    panic!("or-pattern should always have a result slot");
                }

                result
            }
            PatternKind::DeRef { sub_pattern } => {
                let sub_pattern = self.in_func.pattern(*sub_pattern);
                match source {
                    // must_copy is probably irrelevant at this stage
                    Place::Local(source_slot) => {
                        self.match_pattern_internal(
                            sub_pattern,
                            Place::Ptr(source_slot, 0),
                            true,
                            match_result_slot,
                        )
                    }
                    Place::Ptr(ptr_slot,ptr_offset) => {
                        let new_slot = self.stack.alloc(pat.ty);
                        if let Some(copy) = bytecode_select::copy_from_ptr(new_slot, ptr_slot, pat.ty, ptr_offset) {
                            self.out_bc.push(copy);
                        }

                        self.match_pattern_internal(
                            sub_pattern,
                            Place::Ptr(new_slot, 0),
                            true,
                            match_result_slot,
                        )
                    }
                }
            }
            PatternKind::LiteralValue(n) => {
                // rufutable pattern, should have a result
                let match_result_slot = match_result_slot.unwrap();

                let val_slot = match source {
                    Place::Local(slot) => slot,
                    Place::Ptr(ref_slot, ref_offset) => {
                        let ty = self.apply_subs(pat.ty);
                        let slot = self.stack.alloc(ty);
                        if let Some(instr) =
                            bytecode_select::copy_from_ptr(slot, ref_slot, ty, ref_offset)
                        {
                            self.out_bc.push(instr);
                        }
                        slot
                    }
                };

                let size = pat.ty.layout().assert_size();
                let lit_slot = self.stack.alloc(pat.ty);
                self.out_bc
                    .push(bytecode_select::literal(*n, size, lit_slot));

                let (cmp_ctor, _) = bytecode_select::binary(BinaryOp::Eq, pat.ty);
                self.out_bc
                    .push(cmp_ctor(match_result_slot, val_slot, lit_slot));
                true
            }
            PatternKind::Hole => false,
            PatternKind::Error => todo!()
        }
    }

    fn find_local(&self, local: u32) -> Slot {
        for (id, slot) in &self.locals {
            if *id == local {
                return *slot;
            }
        }
        panic!("failed to find local");
    }

    fn find_or_alloc_local(&mut self, local: u32, ty: Type) -> Slot {
        for (id, slot) in &self.locals {
            if *id == local {
                return *slot;
            }
        }
        let var_slot = self.stack.alloc(ty);
        self.locals.push((local, var_slot));
        var_slot
    }

    fn assert_local_undef(&self, local: u32) {
        for (id, _) in &self.locals {
            if *id == local {
                panic!("assert_local_undef failed");
            }
        }
    }

    fn skip_instr(&mut self) -> usize {
        let index = self.out_bc.len();
        self.out_bc.push(Instr::Bad);
        index
    }

    fn get_jump_offset(&mut self, other: usize) -> i32 {
        other as i32 - self.out_bc.len() as i32
    }
}

#[derive(Debug, Clone, Copy)]
enum Place {
    Local(Slot),
    /// Pointer with offset
    Ptr(Slot, u32),
}

impl Place {
    pub fn offset_by(&self, n: u32) -> Self {
        match self {
            Place::Local(slot) => Place::Local(slot.offset_by(n)),
            Place::Ptr(slot, offset) => Place::Ptr(*slot, offset + n),
        }
    }
}

#[derive(Default)]
pub struct CompilerStack {
    entries: Vec<StackEntry>,
    top: u32,
}

impl CompilerStack {
    pub fn alloc(&mut self, ty: Type) -> Slot {
        let layout = ty.layout();

        self.align(layout.align);

        let base = self.top;
        let size = layout.assert_size();

        self.entries.push(StackEntry { base, size });
        self.top += size;
        Slot::new(base)
    }

    fn align(&mut self, align: u32) {
        self.top = crate::abi::align(self.top, align);
    }

    pub fn align_for_call(&mut self) -> Slot {
        self.align(16);
        Slot::new(self.top)
    }
}

struct StackEntry {
    base: u32,
    size: u32,
}

// A simplified callstack for
#[derive(Default)]
struct CallFrame {
    top: u32,
}

impl CallFrame {
    fn alloc(&mut self, ty: Type) -> Slot {
        let layout = ty.layout();

        self.align(layout.align);

        let base = self.top;
        let size = layout.assert_size();

        self.top += size;
        Slot::new_frame_sub(base)
    }

    fn align(&mut self, align: u32) {
        self.top = crate::abi::align(self.top, align);
    }
}
