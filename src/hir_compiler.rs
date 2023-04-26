use std::sync::Arc;

use crate::bytecode_select;
use crate::layout::Layout;
use crate::vm::Function;
use crate::vm::instr::Instr;
use crate::vm::{self, instr::Slot};

use rustc_middle::ty::{Ty, TyKind};
use rustc_middle::thir::{self, Thir, ExprId, ExprKind, StmtId, StmtKind, BlockId, Pat, PatKind};

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
    locals: Vec<(ItemLocalId,Slot)>
}

impl<'a,'tcx> HirCompiler<'a,'tcx> {
    pub fn compile(vm: &'a vm::VM<'tcx>, in_func_id: LocalDefId, in_func: &'a Thir<'tcx>, root_expr: ExprId) -> Vec<vm::instr::Instr> {

        let mut compiler = HirCompiler {
            in_func_id,
            in_func,
            out_bc: Vec::new(),
            vm,
            stack: Default::default(),
            locals: Vec::new()
        };

        let fn_sig = match in_func.body_type {
            thir::BodyTy::Fn(sig) => sig,
            _ => panic!("const not supported")
        };
        println!("{:?}",fn_sig);

        //in_func.exprs[root_expr];

        compiler.lower_expr(root_expr,Some(Slot::new(0)));

        panic!("stop");
    }

    fn lower_block(&mut self, id: BlockId) -> ! {
        let block = &self.in_func.blocks[id];
        for stmt_id in block.stmts.iter() {
            self.lower_stmt(*stmt_id);
        }
        assert!(block.expr.is_none());
        panic!("exit block");
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
            _ => panic!("stmt {:?}",stmt.kind)
        }
    }

    fn lower_expr(&mut self, id: ExprId, dst_slot: Option<Slot>) -> Slot {
        let expr = &self.in_func.exprs[id];

        match &expr.kind {
            ExprKind::Scope{region_scope,value,..} => {
                self.lower_expr(*value, dst_slot)
            }
            ExprKind::Block{block} => {
                self.lower_block(*block);
            }
            ExprKind::ValueTypeAscription{source,..} => {
                self.lower_expr(*source, dst_slot)
            }
            ExprKind::VarRef{id} => {
                let hir_id = id.0;
                assert_eq!(hir_id.owner.def_id,self.in_func_id);

                let local_slot = self.find_local(hir_id.local_id);

                if let Some(dst_slot) = dst_slot {
                    panic!("todo move var");
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
            ExprKind::Call{ty,args,..} => {
                let func = self.ty_to_func(*ty).expect("can't find function");

                // TODO this stack layout is NOT robust, consider:
                // f(&(x+1),&(y+1),&(z+1))
                // we need space to store the temporaries that won't get overridden
                // I think the old interpreter accounted for this (somehow lol)
                let base_slot = self.stack.align_for_call();
                // todo alloc slot for return
                let arg_slots: Vec<_> = args.iter().map(|arg| {
                    let arg_ty = self.expr_ty(*arg);
                    self.stack.alloc(arg_ty)
                }).collect();

                for (arg, slot) in args.iter().zip(arg_slots.iter()) {
                    self.lower_expr(*arg, Some(*slot));
                }

                self.out_bc.push(Instr::Call(base_slot, func));
                if let Some(dst_slot) = dst_slot {
                    panic!("todo move to result");
                } else {
                    base_slot
                }
            }
            _ => panic!("expr {:?}",expr.kind)
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
        println!("alloc {}",base);
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
