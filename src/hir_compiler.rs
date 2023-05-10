use crate::abi::POINTER_SIZE;
use crate::bytecode_select;
use crate::ir::{IRFunction, BlockId, StmtId, ExprId, ExprKind, LogicOp, Pattern, PatternKind, PointerCast, Stmt};
use crate::types::{Type, TypeKind, Sub};
use crate::vm::Function;
use crate::vm::instr::Instr;
use crate::vm::{self, instr::Slot};

pub struct HirCompiler<'vm,'f> {
    in_func_subs: &'f [Sub<'vm>],
    in_func: &'f IRFunction<'vm>,
    out_bc: Vec<Instr<'vm>>,
    vm: &'vm vm::VM<'vm>,
    stack: CompilerStack,
    locals: Vec<(u32,Slot)>,
    last_scope: u32,
    loops: Vec<LoopInfo>,
    loop_breaks: Vec<BreakInfo>
}

struct LoopInfo {
    loop_id: u32,
    result_slot: Slot,
    start_index: usize
}

struct BreakInfo {
    loop_id: u32,
    break_index: usize
}

impl<'vm,'f> HirCompiler<'vm,'f> {
    pub fn compile(vm: &'vm vm::VM<'vm>, ir: &'f IRFunction<'vm>, subs: &[Sub<'vm>], path: &str) -> Vec<Instr<'vm>> {

        if vm.is_verbose {
            println!("compiling {:?} for {:?}",path,subs);
        }

        let mut compiler = HirCompiler {
            in_func_subs: subs,
            in_func: &ir,
            out_bc: Vec::new(),
            vm,
            stack: Default::default(),
            locals: Vec::new(),
            last_scope: 0xFFFFFFFF,
            loops: Vec::new(),
            loop_breaks: Vec::new()
        };

        let out_ty = compiler.apply_subs(ir.return_ty);

        let ret_slot = compiler.stack.alloc(out_ty);
        assert_eq!(ret_slot.index(),0);
        for param in &ir.params {
            compiler.alloc_pattern(param);
        }

        compiler.lower_expr(ir.root_expr,Some(Slot::new(0)));
        compiler.out_bc.push(Instr::Return);

        if compiler.vm.is_verbose {
            println!("compiled {:?} for {:?}","(todo)",compiler.in_func_subs);
            for (i,bc) in compiler.out_bc.iter().enumerate() {
                println!("  {} {:?}",i,bc);
            }
        }

        //println!("{:?}",compiler.out_bc);
        compiler.out_bc
    }

    fn debug<S: Into<String>>(&mut self, f: impl Fn()->S) {
        if self.vm.is_verbose {
            let bc = Instr::Debug(Box::new(f().into()));
            self.out_bc.push(bc);
        }
    }

    fn lower_block(&mut self, id: BlockId, dst_slot: Option<Slot>) -> Slot {
        let block = self.in_func.block(id);
        for stmt_id in block.stmts.iter() {
            self.lower_stmt(*stmt_id);
        }
        if let Some(expr) = block.result {
            // ALWAYS have a slot allocated for the final expression
            // prevents wrongly aliasing locals - TODO we may want to allocate this up-front for saner stack layout
            let dst_slot = dst_slot.unwrap_or_else(|| {
                self.stack.alloc(self.expr_ty(expr))
            });

            self.lower_expr(expr, Some(dst_slot))
        } else {
            dst_slot.unwrap_or(Slot::DUMMY)
        }
    }

    fn lower_stmt(&mut self, id: StmtId) {
        let stmt = self.in_func.stmt(id);
        match stmt {
            Stmt::Let{pattern,init,else_block,..} => {
                assert!(else_block.is_none());
                let dst_slot = self.alloc_pattern(pattern);
                if let Some(init) = init {
                    self.lower_expr(*init, Some(dst_slot));
                }
            }
            Stmt::Expr(expr) => {
                self.lower_expr(*expr, None);
            }
        }
    }

    fn lower_expr(&mut self, id: ExprId, dst_slot: Option<Slot>) -> Slot {
        let expr = self.in_func.expr(id);
        let expr_ty = self.expr_ty(id);

        match &expr.kind {
            ExprKind::Dummy(value,scope_id) => {
                if let Some(scope_id) = scope_id {
                    self.last_scope = *scope_id;
                }
                self.lower_expr(*value, dst_slot)
            }
            ExprKind::Block(block) => {
                self.lower_block(*block, dst_slot)
            }
            // PLACES:
            ExprKind::VarRef{..} |
            ExprKind::Field{..} |
            ExprKind::Index{..} => {
                let size = expr_ty.layout().assert_size();

                match self.expr_to_place(id) {
                    Place::Local(local_slot) => {
                        if let Some(dst_slot) = dst_slot {
                            if let Some(instr) = bytecode_select::copy(dst_slot, local_slot, size) {
                                self.out_bc.push(instr);
                            }
                            
                            dst_slot
                        } else {
                            local_slot
                        }
                    }
                    Place::Ptr(ptr_slot, offset) => {
                        let dst_slot = dst_slot.unwrap_or_else(|| {
                            self.stack.alloc(expr_ty)
                        });

                        if let Some(instr) = bytecode_select::copy_from_ptr(dst_slot, ptr_slot, size, offset) {
                            self.out_bc.push(instr);
                        }

                        dst_slot
                    }
                }
            }
            ExprKind::LiteralValue(n) => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

                self.out_bc.push(bytecode_select::literal(*n, expr_ty.layout().assert_size(), dst_slot));
                dst_slot
            }
            ExprKind::Unary(op, arg) => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

                let arg_slot = self.lower_expr(*arg, None);

                let arg_ty = self.expr_ty(*arg);

                let ctor = bytecode_select::unary(*op, arg_ty);
                self.out_bc.push(ctor(dst_slot,arg_slot));
                dst_slot
            }
            ExprKind::Cast(source) => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

                let arg_slot = self.lower_expr(*source, None);
                
                let arg_ty = self.expr_ty(*source);
                let dst_ty = expr_ty;

                let ctor = bytecode_select::cast(arg_ty,dst_ty);
                self.out_bc.push(ctor(dst_slot,arg_slot));
                dst_slot
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

                let lhs_slot = self.lower_expr(*lhs, None);
                let rhs_slot = self.lower_expr(*rhs, None);

                let lhs_ty = self.expr_ty(*lhs);

                let (ctor,swap) = bytecode_select::binary(*op, lhs_ty);
                if swap {
                    self.out_bc.push(ctor(dst_slot,rhs_slot,lhs_slot));
                } else {
                    self.out_bc.push(ctor(dst_slot,lhs_slot,rhs_slot));
                }
                dst_slot
            }
            ExprKind::Assign(lhs, rhs) => {
                let rhs_slot = self.lower_expr(*rhs, None);

                let assign_ty = self.expr_ty(*lhs);

                match self.expr_to_place(*lhs) {
                    Place::Local(lhs_slot) => {
                        if let Some(instr) = bytecode_select::copy(lhs_slot, rhs_slot, assign_ty.layout().assert_size()) {
                            self.out_bc.push(instr);
                        }
                    }
                    Place::Ptr(ptr_slot,offset) => {
                        if let Some(instr) = bytecode_select::copy_to_ptr(ptr_slot, rhs_slot, assign_ty.layout().assert_size(), offset) {
                            self.out_bc.push(instr);
                        }
                    }
                }

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::AssignOp(op,lhs,rhs) => {
                let rhs_slot = self.lower_expr(*rhs, None);

                let lhs_ty = self.expr_ty(*lhs);
                
                let (ctor,swap) = bytecode_select::binary(*op, lhs_ty);
                assert!(!swap);

                match self.expr_to_place(*lhs) {
                    Place::Local(lhs_slot) => {
                        self.out_bc.push(ctor(lhs_slot,lhs_slot,rhs_slot));
                    }
                    Place::Ptr(ptr_slot,offset) => {
                        let assign_ty = self.expr_ty(*lhs);
                        let assign_size = assign_ty.layout().assert_size();
                        
                        let tmp_slot = self.stack.alloc(assign_ty);
                        
                        self.out_bc.push(bytecode_select::copy_from_ptr(tmp_slot, ptr_slot, assign_size, offset).unwrap());
                        self.out_bc.push(ctor(tmp_slot,tmp_slot,rhs_slot));
                        self.out_bc.push(bytecode_select::copy_to_ptr(ptr_slot, tmp_slot, assign_size, offset).unwrap());
                    }
                }

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Call{func_ty,func_expr,args} => {
                let ty = self.apply_subs(*func_ty);
                let func = self.ty_to_func(ty).expect("can't find function");

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
                    if let Some(instr) = bytecode_select::copy(dst_slot, ret_slot, expr_ty.layout().assert_size()) {
                        self.out_bc.push(instr);
                    }
                    dst_slot
                } else {
                    ret_slot
                }
            }
            ExprKind::If{cond,then,else_opt,..} => {
                let cond_slot = self.lower_expr(*cond, None);
                
                let jump_index_1 = self.skip_instr();
                
                if let Some(else_expr) = else_opt {
                    let dst_slot = dst_slot.unwrap_or_else(|| {
                        self.stack.alloc(expr_ty)
                    });

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
            ExprKind::LogicOp(op,lhs,rhs) => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

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
            ExprKind::Loop(body) => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

                let loop_id = self.last_scope;

                let loop_index = self.out_bc.len();

                self.loops.push(LoopInfo{
                    loop_id,
                    result_slot: dst_slot,
                    start_index: loop_index
                });
                let loop_count = self.loops.len();

                self.lower_expr(*body, Some(Slot::DUMMY));

                assert_eq!(loop_count,self.loops.len());
                self.loops.pop();

                // ending jump
                let offset = self.get_jump_offset(loop_index);
                self.out_bc.push(Instr::Jump(offset));

                // fix breaks
                let break_indices: Vec<_> = self.loop_breaks.drain_filter(|break_info| break_info.loop_id == loop_id)
                    .map(|break_info| break_info.break_index).collect();
                
                for break_index in break_indices {
                    let offset = -self.get_jump_offset(break_index);
                    self.out_bc[break_index] = Instr::Jump(offset);
                }

                dst_slot
            }
            ExprKind::Break{scope_id, value} => {
                let loop_id = *scope_id;

                if let Some(value) = value {
                    let loop_slot = self.loops.iter().find(|x| x.loop_id == loop_id).unwrap().result_slot;
                    self.lower_expr(*value, Some(loop_slot));
                }

                let break_index = self.out_bc.len();
                self.loop_breaks.push(BreakInfo{loop_id,break_index});

                self.out_bc.push(Instr::Bad);

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Continue{scope_id} => {
                let loop_id = *scope_id;

                let loop_start = self.loops.iter().find(|x| x.loop_id == loop_id).unwrap().start_index;

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
            ExprKind::Ref(arg,..) => {

                let place = self.expr_to_place(*arg);

                match place {
                    Place::Local(src_slot) => {
                        let dst_slot = dst_slot.unwrap_or_else(|| {
                            self.stack.alloc(expr_ty)
                        });

                        self.out_bc.push(Instr::SlotAddr(dst_slot,src_slot));
                        dst_slot
                    }
                    Place::Ptr(src_slot, offset) => {
                        assert_eq!(offset,0);

                        if let Some(dst_slot) = dst_slot {
                            self.out_bc.push(bytecode_select::copy(dst_slot, src_slot, expr_ty.layout().assert_size()).unwrap());

                            dst_slot
                        } else {
                            src_slot
                        }
                    }
                }
            }
            ExprKind::DeRef(arg) => {

                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

                let ptr = self.lower_expr(*arg, None);

                let size = expr_ty.layout().assert_size();

                if let Some(instr) = bytecode_select::copy_from_ptr(dst_slot, ptr, size, 0) {
                    self.out_bc.push(instr);
                }

                dst_slot
            }
            ExprKind::PointerCast(source,cast) => {

                match cast {
                    PointerCast::UnSize => {
                        assert_eq!(expr_ty.layout().assert_size(), POINTER_SIZE.bytes()*2);

                        let src_ty = self.expr_ty(*source);
                        
                        let src_kind = match src_ty.kind() {
                            TypeKind::Ptr(ref_ty) |
                            TypeKind::Ref(ref_ty) => ref_ty.kind(),
                            _ => panic!()
                        };

                        let meta = match src_kind {
                            TypeKind::Array(_,elem_count) => {
                                *elem_count as usize
                            }
                            _ => panic!("unsized cast ptr to {:?}",src_kind)
                        };

                        let dst_slot = dst_slot.unwrap_or_else(|| {
                            self.stack.alloc(expr_ty)
                        });

                        let src_slot = self.lower_expr(*source, None);

                        // copy base
                        let ptr_size = POINTER_SIZE.bytes();
                        self.out_bc.push(bytecode_select::copy(dst_slot, src_slot, ptr_size).unwrap());
                        self.out_bc.push(bytecode_select::literal(meta as i128,ptr_size,dst_slot.offset_by(ptr_size)));
                    }
                    _ => panic!("todo ptr cast {:?}",cast)
                }

                self.lower_expr(*source, dst_slot)
            }
            ExprKind::Tuple(fields) => {

                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

                for (field,offset) in fields.iter().zip(expr_ty.layout().field_offsets.iter()) {
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

                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

                for (i,field) in fields.iter().enumerate() {
                    let field_slot = dst_slot.offset_by(i as u32 * elem_size);
                    self.lower_expr(*field, Some(field_slot));
                }

                dst_slot
            }
            ExprKind::Adt{variant,fields} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr_ty)
                });

                assert!(*variant == 0);

                for (field_index,expr) in fields.iter() {
                    let offset = expr_ty.layout().field_offsets[*field_index as usize];
                    let field_slot = dst_slot.offset_by(offset);
                    self.lower_expr(*expr, Some(field_slot));
                }

                dst_slot
            }
            _ => panic!("expr {:?}",expr.kind)
        }
    }

    fn expr_to_place(&mut self, id: ExprId) -> Place {
        let expr = self.in_func.expr(id);

        match &expr.kind {
            ExprKind::Dummy(value,_) => {
                self.expr_to_place(*value)
            }
            ExprKind::DeRef(arg) => {
                let addr_slot = self.lower_expr(*arg, None);
                Place::Ptr(addr_slot, 0)
            }
            ExprKind::VarRef(local_id) => {
                let local_slot = self.find_local(*local_id);
                Place::Local(local_slot)
            }
            ExprKind::Field{lhs,variant,field} => {

                assert!(*variant == 0);

                let field_offset = self.expr_ty(*lhs).layout().field_offsets[*field as usize];

                match self.expr_to_place(*lhs) {
                    Place::Local(base_slot) => {
                        Place::Local(base_slot.offset_by(field_offset))
                    }
                    Place::Ptr(ptr_slot, offset) => {
                        Place::Ptr(ptr_slot, offset + field_offset as i32)
                    }
                }
            }
            ExprKind::Index{lhs,index} => {

                let index_ty = self.expr_ty(*index);
                assert!(index_ty.layout().assert_size() == POINTER_SIZE.bytes());

                // this index slot is re-used for the resulting pointer
                let index_slot = self.stack.alloc(index_ty);
                self.lower_expr(*index, Some(index_slot));
                
                let lhs_ty = self.expr_ty(*lhs);
                let lhs_kind = lhs_ty.kind();
                match lhs_kind {
                    TypeKind::Array(elem_ty,elem_count) => {

                        let elem_size = elem_ty.layout().assert_size();
                        let elem_count = *elem_count;

                        self.out_bc.push(Instr::IndexCalc {
                            arg_out: index_slot,
                            elem_size,
                            elem_count
                        });
        
                        match self.expr_to_place(*lhs) {
                            Place::Local(base_slot) => {
                                self.out_bc.push(Instr::SlotAddrOffset{ out: index_slot, arg: base_slot, offset: index_slot });
                            }
                            Place::Ptr(ptr_slot, offset) => {
                                self.out_bc.push(Instr::PointerOffset(index_slot, ptr_slot, offset));
                            }
                        }
                    }
                    TypeKind::Slice(elem_ty) => {

                        let elem_size = elem_ty.layout().assert_size();

                        let lhs_place = self.expr_to_place(*lhs);
                        if let Place::Ptr(ptr_slot, offset) = lhs_place {
                            assert_eq!(offset,0);
                            let elem_count = ptr_slot.offset_by(POINTER_SIZE.bytes());
                            self.out_bc.push(Instr::IndexCalcDyn{ arg_out: index_slot, elem_size, elem_count });
                            self.out_bc.push(Instr::PointerOffset(index_slot, ptr_slot, 0));
                        } else {
                            panic!("invalid local(?!) slice index");
                        }
                    }
                    _ => panic!("cannot index {:?}",lhs_kind)
                }
                Place::Ptr(index_slot,0)
            }
            // (todo) All other expressions without special handling:
            ExprKind::Block{..} |
            ExprKind::Tuple{..} |
            ExprKind::Binary{..} |
            ExprKind::Ref{..} | // TODO special case? is *& a no-op or does it create a temporary?
            ExprKind::LiteralValue{..} => {
                let res = self.lower_expr(id, None);
                Place::Local(res)
            }
            _ => panic!("expr_to_place {:?}",expr.kind)
        }
    }

    fn apply_subs(&self, ty: Type<'vm>) -> Type<'vm> {
        ty.sub(self.in_func_subs)
    }

    fn expr_ty(&self, id: ExprId) -> Type<'vm> {
        let ty = self.in_func.expr(id).ty;
        self.apply_subs(ty)
    }

    fn ty_to_func(&self, ty: Type<'vm>) -> Option<&'vm Function<'vm>> {
        match ty.kind() {
            TypeKind::FunctionDef(def) => {
                Some(def.item.get_function(&def.subs))
            }
            _ => None
        }
    }

    fn alloc_pattern(&mut self, pat: &Pattern<'vm>) -> Slot {
        match &pat.kind {
            PatternKind::LocalBinding(var_id) => {
                // todo mode may pose issues?
                
                let ty = self.apply_subs(pat.ty);

                let slot = self.stack.alloc(ty);
                self.locals.push((*var_id,slot));
                slot
            }
            _ => panic!("pat {:?}",pat.kind)
        }
    }

    fn find_local(&self, local: u32) -> Slot {
        for (id,slot) in &self.locals {
            if *id == local {
                return *slot;
            }
        }
        panic!("failed to find local");
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

#[derive(Debug)]
enum Place {
    Local(Slot),
    /// Pointer with offset
    Ptr(Slot,i32)
}

#[derive(Default)]
struct CompilerStack {
    entries: Vec<StackEntry>,
    top: u32
}

impl CompilerStack {
    fn alloc(&mut self, ty: Type) -> Slot {
        let layout = ty.layout();

        self.align(layout.align);
        
        let base = self.top;
        let size = layout.assert_size();

        self.entries.push(StackEntry { base, size });
        self.top += size;
        Slot::new(base)
    }

    fn align(&mut self, align: u32) {
        self.top = crate::abi::align(self.top,align);
    }

    fn align_for_call(&mut self) -> Slot {
        self.align(16);
        Slot::new(self.top)
    }
}

struct StackEntry {
    base: u32,
    size: u32
}

// A simplified callstack for 
#[derive(Default)]
struct CallFrame {
    top: u32
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
        self.top = crate::abi::align(self.top,align);
    }
}
