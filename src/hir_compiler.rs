use std::sync::Arc;

use crate::bytecode_select;
use crate::layout::Layout;
use crate::vm::Function;
use crate::vm::instr::Instr;
use crate::vm::{self, instr::Slot};

use rustc_middle::ty::{Ty, TyKind};
use rustc_middle::thir::{self, Thir, ExprId, ExprKind, StmtId, StmtKind, BlockId, Pat, PatKind, LogicalOp};

use rustc_hir::hir_id::ItemLocalId;
use rustc_hir::def_id::LocalDefId;

pub struct HirCompiler<'a,'tcx> {
    //mir: mir::Body<'tcx>,
    //tcx: TyCtxt<'tcx>,
    in_func_id: LocalDefId,
    in_func: &'a Thir<'tcx>,
    out_bc: Vec<vm::instr::Instr>,
    vm: &'a vm::VM<'tcx>,
    stack: CompilerStack,
    locals: Vec<(ItemLocalId,Slot)>,
    last_scope: ItemLocalId,
    loops: Vec<(ItemLocalId,Slot,usize)>,
    loop_breaks: Vec<(ItemLocalId,usize)>
}

impl<'a,'tcx> HirCompiler<'a,'tcx> {
    pub fn compile(vm: &'a vm::VM<'tcx>, in_func_id: LocalDefId, in_func: &'a Thir<'tcx>, root_expr: ExprId) -> Vec<vm::instr::Instr> {

        let mut compiler = HirCompiler {
            in_func_id,
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
        let ret_slot = compiler.stack.alloc(out_ty);
        assert_eq!(ret_slot.offset(),0);
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
            self.lower_expr(expr, dst_slot)
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
                //self.debug(|| "use start");
                let res = self.lower_expr(*source, dst_slot);
                //self.debug(|| "use end");
                res
            }
            ExprKind::NeverToAny{source} => {
                self.lower_expr(*source, dst_slot)
            }
            ExprKind::VarRef{id} => {
                let hir_id = id.0;
                assert_eq!(hir_id.owner.def_id,self.in_func_id);

                let local_slot = self.find_local(hir_id.local_id);

                if let Some(dst_slot) = dst_slot {
                    let size = Layout::from(expr.ty).size;

                    self.out_bc.push(bytecode_select::copy(dst_slot, local_slot, size));
                    dst_slot
                } else {
                    local_slot
                }
            }
            ExprKind::Literal{lit,neg} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr.ty)
                });

                let layout = Layout::from(expr.ty);
                
                self.out_bc.push(bytecode_select::literal(&lit.node, layout.size, dst_slot, *neg));
                dst_slot
            }
            ExprKind::Unary{op, arg} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr.ty)
                });

                let arg_slot = self.lower_expr(*arg, None);

                let layout = Layout::from(self.expr_ty(*arg));

                let ctor = bytecode_select::unary(*op, &layout);
                self.out_bc.push(ctor(dst_slot,arg_slot));
                dst_slot
            }
            ExprKind::Cast{source} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr.ty)
                });

                let arg_slot = self.lower_expr(*source, None);
                
                let arg_ty = self.expr_ty(*source);
                let dst_ty = expr.ty;

                let ctor = bytecode_select::cast(arg_ty,dst_ty);
                self.out_bc.push(ctor(dst_slot,arg_slot));
                dst_slot
            }
            ExprKind::Binary{op, lhs, rhs} => {
                let dst_slot = dst_slot.unwrap_or_else(|| {
                    self.stack.alloc(expr.ty)
                });

                let lhs_slot = self.lower_expr(*lhs, None);
                let rhs_slot = self.lower_expr(*rhs, None);

                let layout = Layout::from(self.expr_ty(*lhs));

                let (ctor,swap) = bytecode_select::binary(*op, &layout);
                if swap {
                    self.out_bc.push(ctor(dst_slot,rhs_slot,lhs_slot));
                } else {
                    self.out_bc.push(ctor(dst_slot,lhs_slot,rhs_slot));
                }
                dst_slot
            }
            ExprKind::Assign{lhs, rhs} => {
                if let Some(lhs_slot) = self.expr_to_slot(*lhs) {
                    let rhs_slot = self.lower_expr(*rhs, None);

                    let layout = Layout::from(self.expr_ty(*lhs));

                    self.out_bc.push(bytecode_select::copy(lhs_slot, rhs_slot, layout.size));
                } else {
                    panic!("nontrivial assign");
                }

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::AssignOp{op,lhs,rhs} => {
                
                if let Some(lhs_slot) = self.expr_to_slot(*lhs) {
                    let rhs_slot = self.lower_expr(*rhs, None);

                    let layout = Layout::from(self.expr_ty(*lhs));
                    
                    let (ctor,swap) = bytecode_select::binary(*op, &layout);
                    assert!(!swap);

                    self.out_bc.push(ctor(lhs_slot,lhs_slot,rhs_slot));
                } else {
                    panic!("nontrivial assign");
                }

                // the destination is (), just return a dummy value
                dst_slot.unwrap_or(Slot::DUMMY)
            }
            ExprKind::Call{ty,args,..} => {
                let func = self.ty_to_func(*ty).expect("can't find function");

                // TODO this stack layout is NOT robust, consider:
                // f(&(x+1),&(y+1),&(z+1))
                // we need space to store the temporaries that won't get overridden
                // I think the old interpreter accounted for this (somehow lol)
                let base_slot = self.stack.align_for_call();
                let ret_slot = self.stack.alloc(expr.ty);
                assert_eq!(ret_slot, base_slot);
                let arg_slots: Vec<_> = args.iter().map(|arg| {
                    let arg_ty = self.expr_ty(*arg);
                    self.stack.alloc(arg_ty)
                }).collect();

                for (arg, slot) in args.iter().zip(arg_slots.iter()) {
                    self.lower_expr(*arg, Some(*slot));
                }

                self.out_bc.push(Instr::Call(ret_slot, func));
                if let Some(dst_slot) = dst_slot {
                    let layout = Layout::from(expr.ty);
                    self.out_bc.push(bytecode_select::copy(dst_slot, ret_slot, layout.size));
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
                        self.stack.alloc(expr.ty)
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
                    self.stack.alloc(expr.ty)
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
                    self.stack.alloc(expr.ty)
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
            ExprKind::Tuple{fields} => {
                if fields.len() == 0 {
                    // no-op
                    dst_slot.unwrap_or(Slot::DUMMY)
                } else {
                    panic!("non-trivial tuple");
                }
            }
            _ => panic!("expr {:?}",expr.kind)
        }
    }

    /// Try converting an expression to a local slot. Should only work for locals and their fields.
    fn expr_to_slot(&mut self, id: ExprId) -> Option<Slot> {
        let expr = &self.in_func.exprs[id];

        match &expr.kind {
            ExprKind::Scope{region_scope,value,..} => {
                self.expr_to_slot(*value)
            }
            ExprKind::VarRef{id} => {
                let hir_id = id.0;
                assert_eq!(hir_id.owner.def_id,self.in_func_id);

                let local_slot = self.find_local(hir_id.local_id);
                Some(local_slot)
            }
            _ => panic!("expr_to_slot {:?}",expr.kind)
        }
    }

    fn expr_ty(&self, id: ExprId) -> Ty<'tcx> {
        self.in_func.exprs[id].ty
    }

    fn ty_to_func(&self, ty: Ty<'tcx>) -> Option<Arc<Function>> {
        match ty.kind() {
            TyKind::FnDef(func_id,subs) => {
                assert_eq!(subs.len(),0);
    
                if func_id.krate != rustc_hir::def_id::LOCAL_CRATE {
                    panic!("non-local call");
                }
    
                let local_id = rustc_hir::def_id::LocalDefId{ local_def_index: func_id.index };

                let func = self.vm.get_func(local_id);
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

                let slot = self.stack.alloc(*ty);
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

#[derive(Default)]
struct CompilerStack {
    entries: Vec<StackEntry>,
    top: u32
}

impl CompilerStack {
    fn alloc<'tcx>(&mut self, ty: Ty<'tcx>) -> Slot {

        let layout = Layout::from(ty);
        
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
