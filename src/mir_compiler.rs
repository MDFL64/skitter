use crate::layout::IntSign;
use crate::layout::Layout;
use crate::layout::LayoutKind;
use crate::vm::VM;

use super::vm;
use super::vm::instr::*;
use rustc_middle::mir;

use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::Ty;

use rustc_hir::def_id::LocalDefId;

use rustc_middle::mir::Rvalue;
use rustc_middle::mir::Operand;
use rustc_middle::mir::ConstantKind;
use rustc_middle::mir::{BinOp, UnOp};
use rustc_middle::mir::CastKind;

use rustc_middle::mir::interpret::Scalar;
use rustc_middle::mir::interpret::ConstValue;

pub struct MirCompiler<'a,'tcx> {
    //mir: mir::Body<'tcx>,
    //tcx: TyCtxt<'tcx>,
    vm: &'a VM<'tcx>,
    current_block: usize,
    locals: &'a mir::LocalDecls<'tcx>,
    stack_info: StackInfo,
    out_bc: Vec<vm::instr::Instr>,
}

impl<'a,'tcx> MirCompiler<'a,'tcx> {
    pub fn compile(vm: &'a VM<'tcx>, mir: &'a mir::Body<'tcx>) -> Vec<vm::instr::Instr> {
        let mut compiler = Self {
            vm,
            current_block: 0,
            locals: &mir.local_decls,
            out_bc: Vec::new(),
            stack_info: StackInfo::default(),
        };

        // allocate args
        for i in 0..(mir.arg_count+1) {
            let local_id = mir::Local::from_usize(i);
            let local_ty = compiler.locals[local_id].ty;
            compiler.stack_info.alloc_slot(SlotId::Local(local_id), local_ty);
        }

        for (i,block) in mir.basic_blocks.iter().enumerate() {
            compiler.compile_block(i,&block);
        }

        compiler.out_bc
    }

    fn tcx(&self) -> TyCtxt<'tcx> {
        self.vm.tcx
    }

    fn compile_block(&mut self, block_n: usize, block: &mir::BasicBlockData<'tcx>) {
        self.current_block = block_n;

        for stmt in &block.statements {
            self.compile_statement(stmt);
        }

        if let Some(term) = &block.terminator {
            self.compile_terminator(term);
        } else {
            panic!("no terminator");
        }
        self.stack_info.free_all_internal();
        //panic!("todo post-block cleanup");
    }

    fn compile_statement(&mut self, stmt: &mir::Statement<'tcx>) {
        //println!("{:?}",stmt);
        use mir::StatementKind;
        match &stmt.kind {
            StatementKind::StorageLive(local_id) => {
                let local_ty = self.locals[*local_id].ty;
                self.stack_info.alloc_slot(SlotId::Local(*local_id), local_ty);
            }
            StatementKind::StorageDead(local_id) => {
                self.stack_info.free_local(*local_id);
            }
            StatementKind::FakeRead(_) | StatementKind::AscribeUserType(..) => {
                // no-op
            }
            StatementKind::Assign(assign) => {
                let place = assign.0;
                let dst_slot = self.prepare_slot_for_local(place);

                let source = &assign.1;
                match source {
                    Rvalue::Use(op) => {
                        self.operand_move_to_slot(op, dst_slot);
                    }
                    Rvalue::BinaryOp(op,args) => {
                        let ty = args.0.ty(self.locals,self.tcx());

                        let lhs = self.operand_get_slot(&args.0);
                        let rhs = self.operand_get_slot(&args.1);

                        let layout = Layout::from(ty);

                        let mut swap = false;

                        let ctor = match (&layout.kind,layout.size,op) {
                            (LayoutKind::Int(_),1,BinOp::Eq) => Instr::I8_Eq,
                            (LayoutKind::Int(_),1,BinOp::Ne) => Instr::I8_NotEq,
                            (LayoutKind::Int(_),1,BinOp::Add) => Instr::I8_Add,
                            (LayoutKind::Int(_),1,BinOp::Sub) => Instr::I8_Sub,
                            (LayoutKind::Int(_),1,BinOp::Mul) => Instr::I8_Mul,
                            (LayoutKind::Int(_),1,BinOp::BitOr) => Instr::I8_Or,
                            (LayoutKind::Int(_),1,BinOp::BitAnd) => Instr::I8_And,
                            (LayoutKind::Int(_),1,BinOp::BitXor) => Instr::I8_Xor,
                            (LayoutKind::Int(_),1,BinOp::Shl) => Instr::I8_ShiftL,
                            (LayoutKind::Int(IntSign::Signed),1,BinOp::Lt) => Instr::I8_S_Lt,
                            (LayoutKind::Int(IntSign::Signed),1,BinOp::Gt) => { swap = true; Instr::I8_S_Lt },
                            (LayoutKind::Int(IntSign::Signed),1,BinOp::Le) => Instr::I8_S_LtEq,
                            (LayoutKind::Int(IntSign::Signed),1,BinOp::Ge) => { swap = true; Instr::I8_S_LtEq },
                            (LayoutKind::Int(IntSign::Signed),1,BinOp::Div) => Instr::I8_S_Div,
                            (LayoutKind::Int(IntSign::Signed),1,BinOp::Rem) => Instr::I8_S_Rem,
                            (LayoutKind::Int(IntSign::Signed),1,BinOp::Shr) => Instr::I8_S_ShiftR,
                            (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Lt) => Instr::I8_U_Lt,
                            (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Gt) => { swap = true; Instr::I8_U_Lt },
                            (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Le) => Instr::I8_U_LtEq,
                            (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Ge) => { swap = true; Instr::I8_U_LtEq },
                            (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Div) => Instr::I8_U_Div,
                            (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Rem) => Instr::I8_U_Rem,
                            (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Shr) => Instr::I8_U_ShiftR,

                            (LayoutKind::Int(_),2,BinOp::Eq) => Instr::I16_Eq,
                            (LayoutKind::Int(_),2,BinOp::Ne) => Instr::I16_NotEq,
                            (LayoutKind::Int(_),2,BinOp::Add) => Instr::I16_Add,
                            (LayoutKind::Int(_),2,BinOp::Sub) => Instr::I16_Sub,
                            (LayoutKind::Int(_),2,BinOp::Mul) => Instr::I16_Mul,
                            (LayoutKind::Int(_),2,BinOp::BitOr) => Instr::I16_Or,
                            (LayoutKind::Int(_),2,BinOp::BitAnd) => Instr::I16_And,
                            (LayoutKind::Int(_),2,BinOp::BitXor) => Instr::I16_Xor,
                            (LayoutKind::Int(_),2,BinOp::Shl) => Instr::I16_ShiftL,
                            (LayoutKind::Int(IntSign::Signed),2,BinOp::Lt) => Instr::I16_S_Lt,
                            (LayoutKind::Int(IntSign::Signed),2,BinOp::Gt) => { swap = true; Instr::I16_S_Lt },
                            (LayoutKind::Int(IntSign::Signed),2,BinOp::Le) => Instr::I16_S_LtEq,
                            (LayoutKind::Int(IntSign::Signed),2,BinOp::Ge) => { swap = true; Instr::I16_S_LtEq },
                            (LayoutKind::Int(IntSign::Signed),2,BinOp::Div) => Instr::I16_S_Div,
                            (LayoutKind::Int(IntSign::Signed),2,BinOp::Rem) => Instr::I16_S_Rem,
                            (LayoutKind::Int(IntSign::Signed),2,BinOp::Shr) => Instr::I16_S_ShiftR,
                            (LayoutKind::Int(IntSign::Unsigned),2,BinOp::Lt) => Instr::I16_U_Lt,
                            (LayoutKind::Int(IntSign::Unsigned),2,BinOp::Gt) => { swap = true; Instr::I16_U_Lt },
                            (LayoutKind::Int(IntSign::Unsigned),2,BinOp::Le) => Instr::I16_U_LtEq,
                            (LayoutKind::Int(IntSign::Unsigned),2,BinOp::Ge) => { swap = true; Instr::I16_U_LtEq },
                            (LayoutKind::Int(IntSign::Unsigned),2,BinOp::Div) => Instr::I16_U_Div,
                            (LayoutKind::Int(IntSign::Unsigned),2,BinOp::Rem) => Instr::I16_U_Rem,
                            (LayoutKind::Int(IntSign::Unsigned),2,BinOp::Shr) => Instr::I16_U_ShiftR,

                            (LayoutKind::Int(_),4,BinOp::Eq) => Instr::I32_Eq,
                            (LayoutKind::Int(_),4,BinOp::Ne) => Instr::I32_NotEq,
                            (LayoutKind::Int(_),4,BinOp::Add) => Instr::I32_Add,
                            (LayoutKind::Int(_),4,BinOp::Sub) => Instr::I32_Sub,
                            (LayoutKind::Int(_),4,BinOp::Mul) => Instr::I32_Mul,
                            (LayoutKind::Int(_),4,BinOp::BitOr) => Instr::I32_Or,
                            (LayoutKind::Int(_),4,BinOp::BitAnd) => Instr::I32_And,
                            (LayoutKind::Int(_),4,BinOp::BitXor) => Instr::I32_Xor,
                            (LayoutKind::Int(_),4,BinOp::Shl) => Instr::I32_ShiftL,
                            (LayoutKind::Int(IntSign::Signed),4,BinOp::Lt) => Instr::I32_S_Lt,
                            (LayoutKind::Int(IntSign::Signed),4,BinOp::Gt) => { swap = true; Instr::I32_S_Lt },
                            (LayoutKind::Int(IntSign::Signed),4,BinOp::Le) => Instr::I32_S_LtEq,
                            (LayoutKind::Int(IntSign::Signed),4,BinOp::Ge) => { swap = true; Instr::I32_S_LtEq },
                            (LayoutKind::Int(IntSign::Signed),4,BinOp::Div) => Instr::I32_S_Div,
                            (LayoutKind::Int(IntSign::Signed),4,BinOp::Rem) => Instr::I32_S_Rem,
                            (LayoutKind::Int(IntSign::Signed),4,BinOp::Shr) => Instr::I32_S_ShiftR,
                            (LayoutKind::Int(IntSign::Unsigned),4,BinOp::Lt) => Instr::I32_U_Lt,
                            (LayoutKind::Int(IntSign::Unsigned),4,BinOp::Gt) => { swap = true; Instr::I32_U_Lt },
                            (LayoutKind::Int(IntSign::Unsigned),4,BinOp::Le) => Instr::I32_U_LtEq,
                            (LayoutKind::Int(IntSign::Unsigned),4,BinOp::Ge) => { swap = true; Instr::I32_U_LtEq },
                            (LayoutKind::Int(IntSign::Unsigned),4,BinOp::Div) => Instr::I32_U_Div,
                            (LayoutKind::Int(IntSign::Unsigned),4,BinOp::Rem) => Instr::I32_U_Rem,
                            (LayoutKind::Int(IntSign::Unsigned),4,BinOp::Shr) => Instr::I32_U_ShiftR,

                            (LayoutKind::Int(_),8,BinOp::Eq) => Instr::I64_Eq,
                            (LayoutKind::Int(_),8,BinOp::Ne) => Instr::I64_NotEq,
                            (LayoutKind::Int(_),8,BinOp::Add) => Instr::I64_Add,
                            (LayoutKind::Int(_),8,BinOp::Sub) => Instr::I64_Sub,
                            (LayoutKind::Int(_),8,BinOp::Mul) => Instr::I64_Mul,
                            (LayoutKind::Int(_),8,BinOp::BitOr) => Instr::I64_Or,
                            (LayoutKind::Int(_),8,BinOp::BitAnd) => Instr::I64_And,
                            (LayoutKind::Int(_),8,BinOp::BitXor) => Instr::I64_Xor,
                            (LayoutKind::Int(_),8,BinOp::Shl) => Instr::I64_ShiftL,
                            (LayoutKind::Int(IntSign::Signed),8,BinOp::Lt) => Instr::I64_S_Lt,
                            (LayoutKind::Int(IntSign::Signed),8,BinOp::Gt) => { swap = true; Instr::I64_S_Lt },
                            (LayoutKind::Int(IntSign::Signed),8,BinOp::Le) => Instr::I64_S_LtEq,
                            (LayoutKind::Int(IntSign::Signed),8,BinOp::Ge) => { swap = true; Instr::I64_S_LtEq },
                            (LayoutKind::Int(IntSign::Signed),8,BinOp::Div) => Instr::I64_S_Div,
                            (LayoutKind::Int(IntSign::Signed),8,BinOp::Rem) => Instr::I64_S_Rem,
                            (LayoutKind::Int(IntSign::Signed),8,BinOp::Shr) => Instr::I64_S_ShiftR,
                            (LayoutKind::Int(IntSign::Unsigned),8,BinOp::Lt) => Instr::I64_U_Lt,
                            (LayoutKind::Int(IntSign::Unsigned),8,BinOp::Gt) => { swap = true; Instr::I64_U_Lt },
                            (LayoutKind::Int(IntSign::Unsigned),8,BinOp::Le) => Instr::I64_U_LtEq,
                            (LayoutKind::Int(IntSign::Unsigned),8,BinOp::Ge) => { swap = true; Instr::I64_U_LtEq },
                            (LayoutKind::Int(IntSign::Unsigned),8,BinOp::Div) => Instr::I64_U_Div,
                            (LayoutKind::Int(IntSign::Unsigned),8,BinOp::Rem) => Instr::I64_U_Rem,
                            (LayoutKind::Int(IntSign::Unsigned),8,BinOp::Shr) => Instr::I64_U_ShiftR,

                            (LayoutKind::Int(_),16,BinOp::Eq) => Instr::I128_Eq,
                            (LayoutKind::Int(_),16,BinOp::Ne) => Instr::I128_NotEq,
                            (LayoutKind::Int(_),16,BinOp::Add) => Instr::I128_Add,
                            (LayoutKind::Int(_),16,BinOp::Sub) => Instr::I128_Sub,
                            (LayoutKind::Int(_),16,BinOp::Mul) => Instr::I128_Mul,
                            (LayoutKind::Int(_),16,BinOp::BitOr) => Instr::I128_Or,
                            (LayoutKind::Int(_),16,BinOp::BitAnd) => Instr::I128_And,
                            (LayoutKind::Int(_),16,BinOp::BitXor) => Instr::I128_Xor,
                            (LayoutKind::Int(_),16,BinOp::Shl) => Instr::I128_ShiftL,
                            (LayoutKind::Int(IntSign::Signed),16,BinOp::Lt) => Instr::I128_S_Lt,
                            (LayoutKind::Int(IntSign::Signed),16,BinOp::Gt) => { swap = true; Instr::I128_S_Lt },
                            (LayoutKind::Int(IntSign::Signed),16,BinOp::Le) => Instr::I128_S_LtEq,
                            (LayoutKind::Int(IntSign::Signed),16,BinOp::Ge) => { swap = true; Instr::I128_S_LtEq },
                            (LayoutKind::Int(IntSign::Signed),16,BinOp::Div) => Instr::I128_S_Div,
                            (LayoutKind::Int(IntSign::Signed),16,BinOp::Rem) => Instr::I128_S_Rem,
                            (LayoutKind::Int(IntSign::Signed),16,BinOp::Shr) => Instr::I128_S_ShiftR,
                            (LayoutKind::Int(IntSign::Unsigned),16,BinOp::Lt) => Instr::I128_U_Lt,
                            (LayoutKind::Int(IntSign::Unsigned),16,BinOp::Gt) => { swap = true; Instr::I128_U_Lt },
                            (LayoutKind::Int(IntSign::Unsigned),16,BinOp::Le) => Instr::I128_U_LtEq,
                            (LayoutKind::Int(IntSign::Unsigned),16,BinOp::Ge) => { swap = true; Instr::I128_U_LtEq },
                            (LayoutKind::Int(IntSign::Unsigned),16,BinOp::Div) => Instr::I128_U_Div,
                            (LayoutKind::Int(IntSign::Unsigned),16,BinOp::Rem) => Instr::I128_U_Rem,
                            (LayoutKind::Int(IntSign::Unsigned),16,BinOp::Shr) => Instr::I128_U_ShiftR,

                            (LayoutKind::Bool,1,BinOp::BitAnd) => Instr::I8_And,
                            (LayoutKind::Bool,1,BinOp::BitOr) => Instr::I8_Or,
                            (LayoutKind::Bool,1,BinOp::BitXor) => Instr::I8_Xor,
                            (LayoutKind::Bool,1,BinOp::Eq) => Instr::I8_Eq,
                            (LayoutKind::Bool,1,BinOp::Ne) => Instr::I8_NotEq,

                            (LayoutKind::Float,4,BinOp::Eq) => Instr::F32_Eq,
                            (LayoutKind::Float,4,BinOp::Ne) => Instr::F32_NotEq,
                            (LayoutKind::Float,4,BinOp::Add) => Instr::F32_Add,
                            (LayoutKind::Float,4,BinOp::Sub) => Instr::F32_Sub,
                            (LayoutKind::Float,4,BinOp::Mul) => Instr::F32_Mul,
                            (LayoutKind::Float,4,BinOp::Lt) => Instr::F32_Lt,
                            (LayoutKind::Float,4,BinOp::Gt) => Instr::F32_Gt,
                            (LayoutKind::Float,4,BinOp::Le) => Instr::F32_LtEq,
                            (LayoutKind::Float,4,BinOp::Ge) => Instr::F32_GtEq,
                            (LayoutKind::Float,4,BinOp::Div) => Instr::F32_Div,
                            (LayoutKind::Float,4,BinOp::Rem) => Instr::F32_Rem,

                            (LayoutKind::Float,8,BinOp::Eq) => Instr::F64_Eq,
                            (LayoutKind::Float,8,BinOp::Ne) => Instr::F64_NotEq,
                            (LayoutKind::Float,8,BinOp::Add) => Instr::F64_Add,
                            (LayoutKind::Float,8,BinOp::Sub) => Instr::F64_Sub,
                            (LayoutKind::Float,8,BinOp::Mul) => Instr::F64_Mul,
                            (LayoutKind::Float,8,BinOp::Lt) => Instr::F64_Lt,
                            (LayoutKind::Float,8,BinOp::Gt) => Instr::F64_Gt,
                            (LayoutKind::Float,8,BinOp::Le) => Instr::F64_LtEq,
                            (LayoutKind::Float,8,BinOp::Ge) => Instr::F64_GtEq,
                            (LayoutKind::Float,8,BinOp::Div) => Instr::F64_Div,
                            (LayoutKind::Float,8,BinOp::Rem) => Instr::F64_Rem,

                            //(PrimType::I32,BinOp::Div) => Instr::I32_Div,

                            _ => panic!("no binary op: {:?} {:?} {:?}",layout.kind,layout.size,op)
                        };

                        if swap {
                            self.out_bc.push(ctor(dst_slot,rhs,lhs));
                        } else {
                            self.out_bc.push(ctor(dst_slot,lhs,rhs));
                        }
                    }
                    Rvalue::UnaryOp(op,arg) => {
                        let ty = arg.ty(self.locals,self.tcx());
                        let arg = self.operand_get_slot(&arg);

                        let layout = Layout::from(ty);

                        let ctor = match (&layout.kind,layout.size,op) {
                            (LayoutKind::Int(IntSign::Signed),1,UnOp::Neg) => Instr::I8_Neg,
                            (LayoutKind::Int(_),1,UnOp::Not) => Instr::I8_Not,

                            (LayoutKind::Int(IntSign::Signed),2,UnOp::Neg) => Instr::I16_Neg,
                            (LayoutKind::Int(_),2,UnOp::Not) => Instr::I16_Not,

                            (LayoutKind::Int(IntSign::Signed),4,UnOp::Neg) => Instr::I32_Neg,
                            (LayoutKind::Int(_),4,UnOp::Not) => Instr::I32_Not,

                            (LayoutKind::Int(IntSign::Signed),8,UnOp::Neg) => Instr::I64_Neg,
                            (LayoutKind::Int(_),8,UnOp::Not) => Instr::I64_Not,

                            (LayoutKind::Int(IntSign::Signed),16,UnOp::Neg) => Instr::I128_Neg,
                            (LayoutKind::Int(_),16,UnOp::Not) => Instr::I128_Not,

                            (LayoutKind::Bool,1,UnOp::Not) => Instr::Bool_Not,

                            (LayoutKind::Float,4,UnOp::Neg) => Instr::F32_Neg,
                            (LayoutKind::Float,8,UnOp::Neg) => Instr::F64_Neg,

                            _ => panic!("no unary op: {:?} {:?} {:?}",layout.kind,layout.size,op)
                        };

                        self.out_bc.push(ctor(dst_slot,arg));
                    }
                    Rvalue::Cast(cast,arg,res_ty) => {
                        let arg_layout = Layout::from(arg.ty(self.locals,self.tcx()));
                        let res_layout = Layout::from(*res_ty);

                        let arg = self.operand_get_slot(&arg);

                        let ctor = match cast {
                            CastKind::IntToInt => {
                                
                                let sign = arg_layout.sign();

                                if arg_layout.size >= res_layout.size {
                                    match res_layout.size {
                                        1 => Instr::MovSS1,
                                        2 => Instr::MovSS2,
                                        4 => Instr::MovSS4,
                                        8 => Instr::MovSS8,
                                        16 => Instr::MovSS16,
                                        _ => panic!("no int2int cast for: {:?} {:?} {:?}",arg_layout.size,res_layout.size,sign)
                                    }
                                } else {
                                    match (arg_layout.size,res_layout.size,sign) {
                                        (1,2,IntSign::Signed) => Instr::I16_S_Widen_8,
                                        (1,2,IntSign::Unsigned) => Instr::I16_U_Widen_8,
    
                                        (2,4,IntSign::Signed) => Instr::I32_S_Widen_16,
                                        (2,4,IntSign::Unsigned) => Instr::I32_U_Widen_16,
                                        (1,4,IntSign::Signed) => Instr::I32_S_Widen_8,
                                        (1,4,IntSign::Unsigned) => Instr::I32_U_Widen_8,

                                        (4,8,IntSign::Signed) => Instr::I64_S_Widen_32,
                                        (4,8,IntSign::Unsigned) => Instr::I64_U_Widen_32,
                                        (2,8,IntSign::Signed) => Instr::I64_S_Widen_16,
                                        (2,8,IntSign::Unsigned) => Instr::I64_U_Widen_16,
                                        (1,8,IntSign::Signed) => Instr::I64_S_Widen_8,
                                        (1,8,IntSign::Unsigned) => Instr::I64_U_Widen_8,

                                        (8,16,IntSign::Signed) => Instr::I128_S_Widen_64,
                                        (8,16,IntSign::Unsigned) => Instr::I128_U_Widen_64,
                                        (4,16,IntSign::Signed) => Instr::I128_S_Widen_32,
                                        (4,16,IntSign::Unsigned) => Instr::I128_U_Widen_32,
                                        (2,16,IntSign::Signed) => Instr::I128_S_Widen_16,
                                        (2,16,IntSign::Unsigned) => Instr::I128_U_Widen_16,
                                        (1,16,IntSign::Signed) => Instr::I128_S_Widen_8,
                                        (1,16,IntSign::Unsigned) => Instr::I128_U_Widen_8,
    
                                        _ => panic!("no int2int cast for: {:?} {:?} {:?}",arg_layout.size,res_layout.size,sign)
                                    }
                                }
                            }
                            CastKind::FloatToFloat => {
                                match (arg_layout.size,res_layout.size) {
                                    (4,8) => Instr::F64_From_F32,
                                    (8,4) => Instr::F32_From_F64,
                                    _ => panic!("no float2float cast for: {:?} {:?}",arg_layout.size,res_layout.size)
                                }
                            }
                            CastKind::IntToFloat => {
                                let sign = arg_layout.sign();
                                match (arg_layout.size,res_layout.size,sign) {

                                    (1,4,IntSign::Signed) => Instr::F32_From_I8_S,
                                    (1,4,IntSign::Unsigned) => Instr::F32_From_I8_U,
                                    (1,8,IntSign::Signed) => Instr::F64_From_I8_S,
                                    (1,8,IntSign::Unsigned) => Instr::F64_From_I8_U,

                                    (2,4,IntSign::Signed) => Instr::F32_From_I16_S,
                                    (2,4,IntSign::Unsigned) => Instr::F32_From_I16_U,
                                    (2,8,IntSign::Signed) => Instr::F64_From_I16_S,
                                    (2,8,IntSign::Unsigned) => Instr::F64_From_I16_U,

                                    (4,4,IntSign::Signed) => Instr::F32_From_I32_S,
                                    (4,4,IntSign::Unsigned) => Instr::F32_From_I32_U,
                                    (4,8,IntSign::Signed) => Instr::F64_From_I32_S,
                                    (4,8,IntSign::Unsigned) => Instr::F64_From_I32_U,

                                    (8,4,IntSign::Signed) => Instr::F32_From_I64_S,
                                    (8,4,IntSign::Unsigned) => Instr::F32_From_I64_U,
                                    (8,8,IntSign::Signed) => Instr::F64_From_I64_S,
                                    (8,8,IntSign::Unsigned) => Instr::F64_From_I64_U,

                                    (16,4,IntSign::Signed) => Instr::F32_From_I128_S,
                                    (16,4,IntSign::Unsigned) => Instr::F32_From_I128_U,
                                    (16,8,IntSign::Signed) => Instr::F64_From_I128_S,
                                    (16,8,IntSign::Unsigned) => Instr::F64_From_I128_U,

                                    _ => panic!("no int2float cast for: {:?} {:?} {:?}",arg_layout.size,res_layout.size,sign)
                                }
                            }
                            CastKind::FloatToInt => {
                                let sign = res_layout.sign();
                                match (arg_layout.size,res_layout.size,sign) {

                                    (4,1,IntSign::Signed) => Instr::F32_Into_I8_S,
                                    (4,1,IntSign::Unsigned) => Instr::F32_Into_I8_U,
                                    (8,1,IntSign::Signed) => Instr::F64_Into_I8_S,
                                    (8,1,IntSign::Unsigned) => Instr::F64_Into_I8_U,

                                    (4,2,IntSign::Signed) => Instr::F32_Into_I16_S,
                                    (4,2,IntSign::Unsigned) => Instr::F32_Into_I16_U,
                                    (8,2,IntSign::Signed) => Instr::F64_Into_I16_S,
                                    (8,2,IntSign::Unsigned) => Instr::F64_Into_I16_U,

                                    (4,4,IntSign::Signed) => Instr::F32_Into_I32_S,
                                    (4,4,IntSign::Unsigned) => Instr::F32_Into_I32_U,
                                    (8,4,IntSign::Signed) => Instr::F64_Into_I32_S,
                                    (8,4,IntSign::Unsigned) => Instr::F64_Into_I32_U,

                                    (4,8,IntSign::Signed) => Instr::F32_Into_I64_S,
                                    (4,8,IntSign::Unsigned) => Instr::F32_Into_I64_U,
                                    (8,8,IntSign::Signed) => Instr::F64_Into_I64_S,
                                    (8,8,IntSign::Unsigned) => Instr::F64_Into_I64_U,

                                    (4,16,IntSign::Signed) => Instr::F32_Into_I128_S,
                                    (4,16,IntSign::Unsigned) => Instr::F32_Into_I128_U,
                                    (8,16,IntSign::Signed) => Instr::F64_Into_I128_S,
                                    (8,16,IntSign::Unsigned) => Instr::F64_Into_I128_U,

                                    _ => panic!("no int2float cast for: {:?} {:?} {:?}",arg_layout.size,res_layout.size,sign)
                                }
                            }
                            _ => panic!("no cast: {:?}",cast)
                        };

                        self.out_bc.push(ctor(dst_slot,arg));
                    }
                    _ => panic!("r-value unsupported {:?}",source)
                }
            }
            _ => panic!("? {:?}",stmt.kind)
        }
        self.stack_info.free_all_temporary();
    }

    fn compile_terminator(&mut self, term: &mir::Terminator<'tcx>) {
        use mir::TerminatorKind;
        match &term.kind {
            TerminatorKind::Call{func,args,destination,target,..} => {
                let (func_id,_subs) = func.const_fn_def().expect("non-trivial call");
                let dst_slot = self.prepare_slot_for_local(*destination);

                if func_id.krate != rustc_hir::def_id::LOCAL_CRATE {
                    panic!("non-local call");
                }

                let func = self.vm.get_func(LocalDefId{ local_def_index: func_id.index });

                let base_slot = self.stack_info.align_for_call();
                // destination slot
                let (tmp_dst,size_dst) = {
                    let dst_ty = destination.ty(self.locals,self.tcx());
                    assert!(dst_ty.variant_index.is_none());
                    let size_dst = Layout::from(dst_ty.ty).size;
                    (self.stack_info.alloc_slot(SlotId::Temp, dst_ty.ty),size_dst)
                };
                // arg slots
                let arg_slots: Vec<_> = args.iter().map(|arg| {
                    let ty = arg.ty(self.locals,self.tcx());
                    self.stack_info.alloc_slot(SlotId::Temp, ty)
                }).collect();

                for (arg,slot) in args.iter().zip(arg_slots.iter()) {
                    self.operand_move_to_slot(arg, *slot);
                }

                self.out_bc.push(Instr::Call(base_slot, func));

                self.move_slots(tmp_dst, dst_slot, size_dst);
            }
            TerminatorKind::Assert{cond,expected,msg,target,..} => {
                // todo asserts
                self.compile_goto(*target);
            }
            TerminatorKind::Return => {
                self.out_bc.push(Instr::Return);
            }
            TerminatorKind::Resume => {
                self.out_bc.push(Instr::Bad);
            }
            _ => panic!("? {:?}",term.kind)
        }
        self.stack_info.free_all_temporary();
    }

    fn prepare_slot_for_local(&mut self, place: mir::Place) -> Slot {
        if let Some(local) = place.as_local() {
            let local_info = &self.locals[local];
            self.stack_info.get_local_slot(local).unwrap_or_else(|| {
                if local_info.internal {
                    self.stack_info.alloc_slot(SlotId::LocalInternal(local), local_info.ty)
                } else {
                    panic!("destination slot not allocated -- {:?}",local);
                }
            })
        } else {
            panic!("non-trivial assigns not supported");
        }
    }

    fn compile_goto(&mut self, target: mir::BasicBlock) {
        if self.current_block + 1 != target.index() {
            panic!("todo jump");
        }
    }

    fn move_slots(&mut self, src_slot: Slot, dst_slot: Slot, size: usize) {
        match size {
            0 => (),
            1 => self.out_bc.push(Instr::MovSS1(dst_slot,src_slot)),
            2 => self.out_bc.push(Instr::MovSS2(dst_slot,src_slot)),
            4 => self.out_bc.push(Instr::MovSS4(dst_slot,src_slot)),
            8 => self.out_bc.push(Instr::MovSS8(dst_slot,src_slot)),
            16 => self.out_bc.push(Instr::MovSS16(dst_slot,src_slot)),
            _ => panic!("todo move {:?} -> {:?} / {}",src_slot,dst_slot,size)
        }
    }

    fn operand_get_slot(&mut self, op: &Operand<'tcx>) -> Slot {
        match op {
            Operand::Copy(place) | Operand::Move(place) => {
                let local = place.as_local().unwrap();
                self.stack_info.get_local_slot(local).unwrap()
            }
            Operand::Constant(constant) => {
                match constant.literal {
                    ConstantKind::Val(ConstValue::Scalar(Scalar::Int(scalar_int)),ty) => {
                        let dst_slot = self.stack_info.alloc_slot(SlotId::Temp, ty);

                        let size = scalar_int.size();
                        let raw = scalar_int.assert_bits(size);
                        match size.bytes() {
                            1 => self.out_bc.push(Instr::I8_Const(dst_slot, raw as i8)),
                            2 => self.out_bc.push(Instr::I16_Const(dst_slot, raw as i16)),
                            4 => self.out_bc.push(Instr::I32_Const(dst_slot, raw as i32)),
                            8 => self.out_bc.push(Instr::I64_Const(dst_slot, raw as i64)),
                            16 => self.out_bc.push(Instr::I128_Const(dst_slot, Box::new(raw as i128))),
                            _ => panic!("const size {:?}",size)
                        }

                        dst_slot
                    }
                    ConstantKind::Val(a,b) => panic!("todo const val"),

                    ConstantKind::Ty(a) => panic!("todo const ty"),
                    ConstantKind::Unevaluated(a,b) => panic!("todo const unevaluated"),
                }
            }
        }
    }

    fn operand_move_to_slot(&mut self, op: &Operand<'tcx>, dst_slot: Slot) {
        match op {
            Operand::Copy(place) | Operand::Move(place) => {

                let (src_slot,size) = if let Some(local) = place.as_local() {
                    self.stack_info.get_local_slot_with_size(local).expect("source slot not allocated")
                } else {
                    panic!("non-trivial assigns not supported");
                };

                self.move_slots(src_slot,dst_slot,size);
            }
            Operand::Constant(constant) => {
                match constant.literal {
                    ConstantKind::Val(ConstValue::Scalar(Scalar::Int(scalar_int)),_) => {
                        let size = scalar_int.size();
                        let raw = scalar_int.assert_bits(size);
                        match size.bytes() {
                            1 => self.out_bc.push(Instr::I8_Const(dst_slot, raw as i8)),
                            2 => self.out_bc.push(Instr::I16_Const(dst_slot, raw as i16)),
                            4 => self.out_bc.push(Instr::I32_Const(dst_slot, raw as i32)),
                            8 => self.out_bc.push(Instr::I64_Const(dst_slot, raw as i64)),
                            16 => self.out_bc.push(Instr::I128_Const(dst_slot, Box::new(raw as i128))),
                            _ => panic!("const size {:?}",size)
                        }
                    }
                    ConstantKind::Val(ConstValue::ZeroSized,_) => {
                        // no-op
                    }
                    
                    ConstantKind::Val(a,b) => panic!("todo const val {:?}",a),

                    ConstantKind::Ty(a) => panic!("todo const ty"),
                    ConstantKind::Unevaluated(a,b) => panic!("todo const unevaluated"),
                }
            }
        }
    }
}

#[derive(Default)]
struct StackInfo {
    frame: Vec<SlotInfo>,
    should_align_for_call: bool,
    count_internal: usize,
    count_temporary: usize
}

impl StackInfo {
    pub fn alloc_slot<'tcx>(&mut self, id: SlotId, ty: Ty<'tcx>) -> Slot {
        let mut base = self.frame.last().map(|last| last.top).unwrap_or(0);

        let layout = Layout::from(ty);

        let align = if self.should_align_for_call { 16 } else { layout.align };
        self.should_align_for_call = false;

        // todo this is dumb as shit
        while (base % align) != 0 {
            base += 1;
        }

        self.frame.push(SlotInfo{
            id,
            base,
            top: base + layout.size
        });

        match id {
            SlotId::LocalInternal(_) => self.count_internal += 1,
            SlotId::Temp => self.count_temporary += 1,
            _ => ()
        }

        Slot::new(base as u32)
    }

    pub fn align_for_call(&mut self) -> Slot {

        self.should_align_for_call = true;

        // this is very stupid
        let mut base = self.frame.last().map(|last| last.top).unwrap_or(0);
        let align = 16;
        while (base % align) != 0 {
            base += 1;
        }

        Slot::new(base as u32)
    }

    pub fn free_local(&mut self, id: mir::Local) {
        let indices = (0..self.frame.len()).rev();
        for i in indices {
            let info = &self.frame[i];
            match info.id {
                SlotId::Local(x) => {
                    if x == id {
                        self.free_index(i);
                        break;
                    }
                }
                _ => ()
            }
        }
    }

    fn free_all_temporary(&mut self) {
        if self.count_temporary == 0 {
            return;
        }

        let indices = (0..self.frame.len()).rev();
        for i in indices {
            if let Some(info) = self.frame.get(i) {
                match info.id {
                    SlotId::Temp => {
                        self.free_index(i);
                        self.count_temporary -= 1;
                        if self.count_temporary == 0 {
                            break;
                        }
                    }
                    _ => ()
                }
            }
        }
        assert_eq!(self.count_temporary,0);
    }

    fn free_all_internal(&mut self) {
        if self.count_internal == 0 {
            return;
        }

        let indices = (0..self.frame.len()).rev();
        for i in indices {
            if let Some(info) = self.frame.get(i) {
                match info.id {
                    SlotId::LocalInternal(_) => {
                        self.free_index(i);
                        self.count_internal -= 1;
                        if self.count_internal == 0 {
                            break;
                        }
                    }
                    _ => ()
                }
            }
        }
        assert_eq!(self.count_internal,0);
    }

    fn free_index(&mut self, i: usize) {
        if i == self.frame.len() - 1 {
            self.frame.pop();
            // remove additional dead slots
            while let Some(info) = self.frame.last() {
                if info.id == SlotId::Dead {
                    self.frame.pop();
                } else {
                    break;
                }
            }
        } else {
            self.frame[i].id = SlotId::Dead;
        }
    }

    pub fn get_local_slot(&self, id: mir::Local) -> Option<Slot> {
        self.get_local_slot_with_size(id).map(|x| x.0)
    }

    pub fn get_local_slot_with_size(&self, id: mir::Local) -> Option<(Slot,usize)> {
        for info in self.frame.iter().rev() {
            match info.id {
                SlotId::Local(x) | SlotId::LocalInternal(x) => {
                    if x == id {
                        return Some((Slot::new(info.base as u32),info.top - info.base));
                    }
                }
                _ => ()
            }
        }
        None
    }
}

#[derive(Debug)]
struct SlotInfo {
    id: SlotId,
    base: usize,
    top: usize
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum SlotId {
    Local(mir::Local),
    LocalInternal(mir::Local),
    Temp,
    Dead
}
