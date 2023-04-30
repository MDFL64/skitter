use std::sync::Arc;

use crate::abi::POINTER_SIZE;
use crate::bytecode_select;
use crate::vm::Function;
use crate::vm::instr::Instr;
use crate::vm::{self, instr::Slot};

use rustc_middle::ty::{Ty, TyKind};
use rustc_middle::ty::SubstsRef;
use rustc_middle::thir::{self, Thir, ExprId, ExprKind, StmtId, StmtKind, BlockId, Pat, PatKind, LogicalOp};

use rustc_hir::hir_id::ItemLocalId;
use rustc_hir::def_id::LocalDefId;

pub struct HirCompiler<'a,'tcx> {
    in_func_id: LocalDefId,
    in_func_subs: SubstsRef<'tcx>,
    in_func: &'a Thir<'tcx>,
    out_bc: Vec<vm::instr::Instr<'tcx>>,
    vm: &'a vm::VM<'tcx>,
    stack: CompilerStack,
    locals: Vec<(ItemLocalId,Slot)>,
    last_scope: ItemLocalId,
    loops: Vec<(ItemLocalId,Slot,usize)>,
    loop_breaks: Vec<(ItemLocalId,usize)>
}

impl<'a,'tcx> HirCompiler<'a,'tcx> {
    pub fn compile(vm: &'a vm::VM<'tcx>, in_func_id: LocalDefId, in_func_subs: SubstsRef<'tcx>, in_func: &'a Thir<'tcx>, root_expr: ExprId) -> Vec<vm::instr::Instr<'tcx>> {

        let mut compiler = HirCompiler {
            in_func_id,
            in_func_subs,
            in_func,
            out_bc: Vec::new(),
            vm,
            stack: Default::default(),
            locals: Vec::new(),
            last_scope: ItemLocalId::MAX,
            loops: Vec::new(),
            loop_breaks: Vec::new()
        };

        let out_ty = match in_func.body_type {
            thir::BodyTy::Fn(sig) => {
                sig.output()
            }
            _ => panic!("const not supported")
        };

        let out_ty = compiler.apply_subs(out_ty);
        let out_layout = crate::layout::Layout::from(out_ty,vm);

        let ret_slot = compiler.stack.alloc(&out_layout);
        assert_eq!(ret_slot.index(),0);
        for param in &in_func.params {
            compiler.alloc_pattern(param.pat.as_ref().unwrap());
        }

        compiler.lower_expr(root_expr,Some(Slot::new(0)));
        compiler.out_bc.push(Instr::Return);

        if compiler.vm.is_verbose {
            println!("compiled {:?}",in_func_id);
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
        let block = &self.in_func.blocks[id];
        for stmt_id in block.stmts.iter() {
            self.lower_stmt(*stmt_id);
        }
        if let Some(expr) = block.expr {
            // ALWAYS have a slot allocated for the final expression
            // prevents wrongly aliasing locals
            let dst_slot = dst_slot.unwrap_or_else(|| {
                self.stack.alloc(&self.expr_layout(expr))
            });

            self.lower_expr(expr, Some(dst_slot))
        } else {
            dst_slot.unwrap_or(Slot::DUMMY)
        }
    }

    fn lower_stmt(&mut self, id: StmtId) {
        let stmt = &self.in_func.stmts[id];
        match &stmt.kind {
            StmtKind::Let{pattern,initializer,init_scope,remainder_scope,else_block,..} => {
                assert!(else_block.is_none());
                let dst_slot = self.alloc_pattern(pattern);
                if let Some(init) = initializer {
                    self.lower_expr(*init, Some(dst_slot));
                }
            }
            StmtKind::Expr{scope, expr} => {
                self.lower_expr(*expr, None);
            }
        }
    }

    fn lower_expr(&mut self, id: ExprId, dst_slot: Option<Slot>) -> Slot {
        let expr = &self.in_func.exprs[id];
        //println!("{:?}",expr.kind);
        let expr_layout = self.expr_layout(id);

        match &expr.kind {
            ExprKind::Scope{region_scope,value,..} => {
                self.last_scope = region_scope.id;
                self.lower_expr(*value, dst_slot)
            }
            ExprKind::Block{block} => {
                self.lower_block(*block, dst_slot)
            }
            ExprKind::ValueTypeAscription{source,..} => {
                self.lower_expr(*source, dst_slot)
            }
            ExprKind::Use{source} => {
                self.lower_expr(*source, dst_slot)
            }
            ExprKind::NeverToAny{source} => {
                self.lower_expr(*source, dst_slot)
            }
            // PLACES:
            ExprKind::VarRef{..} |
            ExprKind::Field{..} => {
                let size = expr_layout.size;

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
                            self.stack.alloc(&expr_layout)
                        });

                        if let Some(instr) = bytecode_select::copy_from_ptr(dst_slot, ptr_slot, size, offset) {
                            self.out_bc.push(instr);
                        }

                        dst_slot
                    }
                }
            }
            /*ExprKind::VarRef{id} => {
                let hir_id = id.0;
                assert_eq!(hir_id.owner.def_id,self.in_func_id);

                let local_slot = self.find_local(hir_id.local_id);

                if let Some(dst_slot) = dst_slot {
                    let size = Layout::from(expr.ty).size;

                    if let Some(instr) = bytecode_select::copy(dst_slot, local_slot, size) {
                        self.out_bc.push(instr);
                    }
                    
                    dst_slot
                } else {
                    local_slot
                }
            }
            ExprKind::Field{lhs,variant_index,name} => {

                let lhs_ty = self.expr_ty(*lhs);
                let layout = Layout::from(lhs_ty);
                let field_offset = layout.field_offsets[name.index()];

                match self.expr_to_place(*lhs) {
                    Place::Local(base_slot) => {
                        let slot = base_slot.offset_by(field_offset);
                        panic!("--- {:?}",slot);
                    }
                    Place::Ptr(ptr_slot, offset) => {
                        panic!("todo ptr");
                    }
                }

                //let name: () = name.index();
                //println!("-> {:?}",layout);
                //panic!("= stop");
            }*/
            ExprKind::Literal{lit,neg} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(&expr_layout)
                });
                
                self.out_bc.push(bytecode_select::literal(&lit.node, expr_layout.size, dst_slot, *neg));
                dst_slot
            }
            ExprKind::Unary{op, arg} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(&expr_layout)
                });

                let arg_slot = self.lower_expr(*arg, None);

                let layout = self.expr_layout(*arg);

                let ctor = bytecode_select::unary(*op, &layout);
                self.out_bc.push(ctor(dst_slot,arg_slot));
                dst_slot
            }
            ExprKind::Cast{source} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(&expr_layout)
                });

                let arg_slot = self.lower_expr(*source, None);
                
                let arg_ty = &self.expr_layout(*source);
                let dst_ty = &expr_layout;

                let ctor = bytecode_select::cast(arg_ty,dst_ty);
                self.out_bc.push(ctor(dst_slot,arg_slot));
                dst_slot
            }
            ExprKind::Binary{op, lhs, rhs} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(&expr_layout)
                });

                let lhs_slot = self.lower_expr(*lhs, None);
                let rhs_slot = self.lower_expr(*rhs, None);

                let layout = self.expr_layout(*lhs);

                let (ctor,swap) = bytecode_select::binary(*op, &layout);
                if swap {
                    self.out_bc.push(ctor(dst_slot,rhs_slot,lhs_slot));
                } else {
                    self.out_bc.push(ctor(dst_slot,lhs_slot,rhs_slot));
                }
                dst_slot
            }
            ExprKind::Assign{lhs, rhs} => {
                let rhs_slot = self.lower_expr(*rhs, None);

                let layout = self.expr_layout(*lhs);

                match self.expr_to_place(*lhs) {
                    Place::Local(lhs_slot) => {
                        if let Some(instr) = bytecode_select::copy(lhs_slot, rhs_slot, layout.size) {
                            self.out_bc.push(instr);
                        }
                    }
                    Place::Ptr(ptr_slot,offset) => {
                        if let Some(instr) = bytecode_select::copy_to_ptr(ptr_slot, rhs_slot, layout.size, offset) {
                            self.out_bc.push(instr);
                        }
                    }
                }

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::AssignOp{op,lhs,rhs} => {
                let rhs_slot = self.lower_expr(*rhs, None);

                let layout = self.expr_layout(*lhs);
                
                let (ctor,swap) = bytecode_select::binary(*op, &layout);
                assert!(!swap);

                match self.expr_to_place(*lhs) {
                    Place::Local(lhs_slot) => {
                        self.out_bc.push(ctor(lhs_slot,lhs_slot,rhs_slot));
                    }
                    Place::Ptr(ptr_slot,offset) => {
                        let tmp_slot = self.stack.alloc(&self.expr_layout(*lhs));
                        
                        self.out_bc.push(bytecode_select::copy_from_ptr(tmp_slot, ptr_slot, layout.size, offset).unwrap());
                        self.out_bc.push(ctor(tmp_slot,tmp_slot,rhs_slot));
                        self.out_bc.push(bytecode_select::copy_to_ptr(ptr_slot, tmp_slot, layout.size, offset).unwrap());
                    }
                }

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Call{ty,args,..} => {
                let ty = self.apply_subs(*ty);
                let func = self.ty_to_func(ty).expect("can't find function");

                let call_start_instr = self.out_bc.len();
                for (i,arg) in args.iter().enumerate() {
                    let arg_slot = Slot::new_arg_sub(i as u32);
                    self.lower_expr(*arg, Some(arg_slot));
                }

                let base_slot = self.stack.align_for_call();
                let ret_slot = self.stack.alloc(&expr_layout);
                assert_eq!(ret_slot, base_slot);

                // amend the previous code to write args into the correct slots
                for (i,arg) in args.iter().enumerate() {
                    let arg_layout = self.expr_layout(*arg);
                    let arg_slot = self.stack.alloc(&arg_layout);
                    for index in call_start_instr..self.out_bc.len() {
                        self.out_bc[index].replace_arg(i as u32, arg_slot);
                    }
                }

                self.out_bc.push(Instr::Call(ret_slot, func));
                if let Some(dst_slot) = dst_slot {
                    if let Some(instr) = bytecode_select::copy(dst_slot, ret_slot, expr_layout.size) {
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
                        self.stack.alloc(&expr_layout)
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
            ExprKind::LogicalOp{op,lhs,rhs} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(&expr_layout)
                });

                self.lower_expr(*lhs, Some(dst_slot));

                let jump_index_1 = self.skip_instr();

                self.lower_expr(*rhs, Some(dst_slot));

                let jump_offset_1 = -self.get_jump_offset(jump_index_1);
                
                match op {
                    LogicalOp::And => {
                        self.out_bc[jump_index_1] = Instr::JumpF(jump_offset_1, dst_slot);
                    }
                    LogicalOp::Or => {
                        self.out_bc[jump_index_1] = Instr::JumpT(jump_offset_1, dst_slot);
                    }
                }

                dst_slot
            }
            ExprKind::Loop{body} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(&expr_layout)
                });

                let loop_id = self.last_scope;

                let loop_index = self.out_bc.len();

                self.loops.push((loop_id,dst_slot,0));
                let loop_count = self.loops.len();

                self.lower_expr(*body, Some(Slot::DUMMY));

                assert_eq!(loop_count,self.loops.len());
                self.loops.pop();

                // ending jump
                let offset = self.get_jump_offset(loop_index);
                self.out_bc.push(Instr::Jump(offset));

                // fix breaks
                let break_indices: Vec<_> = self.loop_breaks.drain_filter(|(id,_)| *id == loop_id).map(|(_,x)| x).collect();
                for break_index in break_indices {
                    let offset = -self.get_jump_offset(break_index);
                    self.out_bc[break_index] = Instr::Jump(offset);
                }

                dst_slot
            }
            ExprKind::Break{label, value} => {

                let loop_id = label.id;

                let (_,loop_slot,_) = self.loops.iter().find(|x| x.0 == loop_id).unwrap();

                if let Some(value) = value {
                    self.lower_expr(*value, Some(*loop_slot));
                }

                let break_index = self.out_bc.len();
                self.loop_breaks.push((loop_id,break_index));

                self.out_bc.push(Instr::Bad);

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Continue{label} => {
                let loop_id = label.id;

                let (_,_,loop_start) = self.loops.iter().find(|x| x.0 == loop_id).unwrap();

                let offset = self.get_jump_offset(*loop_start) + 1;
                self.out_bc.push(Instr::Jump(offset));

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Return{value} => {
                if let Some(value) = value {
                    self.lower_expr(*value, Some(Slot::new(0)));
                }
                self.out_bc.push(Instr::Return);

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Borrow{arg,..} | ExprKind::AddressOf{arg,..} => {

                let place = self.expr_to_place(*arg);

                match place {
                    Place::Local(src_slot) => {
                        let dst_slot = dst_slot.unwrap_or_else(|| {
                            self.stack.alloc(&expr_layout)
                        });

                        self.out_bc.push(Instr::SlotAddr(dst_slot,src_slot));
                        dst_slot
                    }
                    Place::Ptr(src_slot, offset) => {
                        assert_eq!(offset,0);

                        if let Some(dst_slot) = dst_slot {
                            self.out_bc.push(bytecode_select::copy(dst_slot, src_slot, POINTER_SIZE.bytes()).unwrap());

                            dst_slot
                        } else {
                            src_slot
                        }
                    }
                }
            }
            ExprKind::Deref{arg} => {

                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(&expr_layout)
                });

                let ptr = self.lower_expr(*arg, None);

                let size = expr_layout.size;

                if let Some(instr) = bytecode_select::copy_from_ptr(dst_slot, ptr, size, 0) {
                    self.out_bc.push(instr);
                }

                dst_slot
            }
            ExprKind::Tuple{fields} => {

                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(&expr_layout)
                });

                for (field,offset) in fields.iter().zip(expr_layout.field_offsets.iter()) {
                    let field_slot = dst_slot.offset_by(*offset);
                    self.lower_expr(*field, Some(field_slot));
                }

                dst_slot
            }
            ExprKind::Adt(adt) => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(&expr_layout)
                });

                assert!(adt.variant_index.index() == 0);

                for field in adt.fields.iter() {
                    let offset = expr_layout.field_offsets[field.name.index()];
                    let field_slot = dst_slot.offset_by(offset);
                    self.lower_expr(field.expr, Some(field_slot));
                }

                dst_slot
            }
            _ => panic!("expr {:?}",expr.kind)
        }
    }

    fn expr_to_place(&mut self, id: ExprId) -> Place {
        let expr = &self.in_func.exprs[id];

        match &expr.kind {
            ExprKind::Scope{region_scope,value,..} => {
                self.expr_to_place(*value)
            }
            ExprKind::Deref{arg} => {
                let addr_slot = self.lower_expr(*arg, None);
                Place::Ptr(addr_slot, 0)
            }
            ExprKind::VarRef{id} => {
                let hir_id = id.0;
                assert_eq!(hir_id.owner.def_id,self.in_func_id);

                let local_slot = self.find_local(hir_id.local_id);
                Place::Local(local_slot)
            }
            ExprKind::Field{lhs,variant_index,name} => {

                assert!(variant_index.index() == 0);

                let layout = self.expr_layout(*lhs);
                let field_offset = layout.field_offsets[name.index()];

                match self.expr_to_place(*lhs) {
                    Place::Local(base_slot) => {
                        Place::Local(base_slot.offset_by(field_offset))
                    }
                    Place::Ptr(ptr_slot, offset) => {
                        Place::Ptr(ptr_slot, offset + field_offset as i32)
                    }
                }
            }
            // (todo) All other expressions without special handling:
            ExprKind::Block{..} |
            ExprKind::Tuple{..} |
            ExprKind::Binary{..} |
            ExprKind::Borrow{..} | // TODO special case? is *& a no-op or does it create a temporary?
            ExprKind::Literal{..} => {
                let res = self.lower_expr(id, None);
                Place::Local(res)
            }
            _ => panic!("expr_to_place {:?}",expr.kind)
        }
    }

    fn apply_subs(&self, ty: Ty<'tcx>) -> Ty<'tcx> {
        let binder = rustc_middle::ty::subst::EarlyBinder(ty);
        binder.subst(self.vm.tcx,self.in_func_subs)
    }

    fn expr_layout(&self, id: ExprId) -> crate::layout::Layout {
        let ty = self.in_func.exprs[id].ty;
        let ty = self.apply_subs(ty);
        crate::layout::Layout::from(ty, self.vm)
    }

    fn ty_to_func(&self, ty: Ty<'tcx>) -> Option<Arc<Function<'tcx>>> {
        match ty.kind() {
            TyKind::FnDef(func_id,subs) => {
                let func = self.vm.get_func(*func_id, subs);
                Some(func)
            }
            _ => None
        }
    }

    fn alloc_pattern(&mut self, pat: &Pat<'tcx>) -> Slot {
        match &pat.kind {
            PatKind::AscribeUserType{subpattern,..} => {
                self.alloc_pattern(subpattern)
            }
            PatKind::Binding{var,ty,subpattern,..} => {
                // todo mode may pose issues?

                let hir_id = var.0;
                assert_eq!(hir_id.owner.def_id,self.in_func_id);
                let var_id = hir_id.local_id;

                assert!(subpattern.is_none());
                
                let ty = self.apply_subs(*ty);
                let layout = crate::layout::Layout::from(ty, self.vm);

                let slot = self.stack.alloc(&layout);
                self.locals.push((var_id,slot));
                slot
            }
            _ => panic!("pat {:?}",pat.kind)
        }
    }

    fn find_local(&self, local: ItemLocalId) -> Slot {
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
    fn alloc(&mut self, layout: &crate::layout::Layout) -> Slot {

        self.align(layout.align);
        
        let base = self.top;
        let size = layout.size;

        self.entries.push(StackEntry { base, size });
        self.top += size;
        Slot::new(base)
    }

    fn align_for_call(&mut self) -> Slot {
        self.align(16);
        Slot::new(self.top)
    }

    fn align(&mut self, n: u32) {
        // todo there is 100% a better way to do this
        while (self.top % n) != 0 {
            self.top += 1;
        }
    }
}

struct StackEntry {
    base: u32,
    size: u32
}
