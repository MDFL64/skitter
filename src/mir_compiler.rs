use crate::layout::Layout;

use super::vm;
use super::vm::instr::*;
use rustc_middle::mir;

use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyKind;
use rustc_middle::ty::IntTy;
use rustc_middle::ty::UintTy;

use rustc_middle::mir::Rvalue;
use rustc_middle::mir::Operand;
use rustc_middle::mir::ConstantKind;
use rustc_middle::mir::{BinOp, UnOp};
use rustc_middle::mir::CastKind;

use rustc_middle::mir::interpret::Scalar;
use rustc_middle::mir::interpret::ConstValue;

pub struct MirCompiler<'a,'tcx> {
    //mir: mir::Body<'tcx>,
    tcx: TyCtxt<'tcx>,
    current_block: usize,
    locals: &'a mir::LocalDecls<'tcx>,
    stack_info: StackInfo,
    out_func: vm::function::Function,
}

impl<'a,'tcx> MirCompiler<'a,'tcx> {
    pub fn compile(tcx: TyCtxt<'tcx>, mir: &'a mir::Body<'tcx>) -> vm::function::Function {
        let mut compiler = Self {
            tcx,
            current_block: 0,
            locals: &mir.local_decls,
            out_func: vm::function::Function::default(),
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

        compiler.out_func
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
                let dst_slot = if let Some(local) = place.as_local() {
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
                };

                let source = &assign.1;
                match source {
                    Rvalue::Use(op) => {
                        self.operand_move_to_slot(op, dst_slot);
                    }
                    Rvalue::BinaryOp(op,args) => {
                        let ty = args.0.ty(self.locals,self.tcx);

                        let lhs = self.operand_get_slot(&args.0);
                        let rhs = self.operand_get_slot(&args.1);

                        let prim = PrimType::from(ty);

                        let mut swap = false;

                        let ctor = match (prim,op) {
                            (PrimType::I32(_),BinOp::Eq) => Instr::I32_Eq,
                            (PrimType::I32(_),BinOp::Ne) => Instr::I32_NotEq,

                            (PrimType::I32(_),BinOp::Add) => Instr::I32_Add,
                            (PrimType::I32(_),BinOp::Sub) => Instr::I32_Sub,
                            (PrimType::I32(_),BinOp::Mul) => Instr::I32_Mul,

                            (PrimType::I32(_),BinOp::BitOr) => Instr::I32_Or,
                            (PrimType::I32(_),BinOp::BitAnd) => Instr::I32_And,
                            (PrimType::I32(_),BinOp::BitXor) => Instr::I32_Xor,
                            (PrimType::I32(_),BinOp::Shl) => Instr::I32_ShiftL,

                            (PrimType::I32(IntSign::Signed),BinOp::Lt) => Instr::I32_S_Lt,
                            (PrimType::I32(IntSign::Signed),BinOp::Gt) => { swap = true; Instr::I32_S_Lt },
                            (PrimType::I32(IntSign::Signed),BinOp::Le) => Instr::I32_S_LtEq,
                            (PrimType::I32(IntSign::Signed),BinOp::Ge) => { swap = true; Instr::I32_S_LtEq },
                            (PrimType::I32(IntSign::Signed),BinOp::Div) => Instr::I32_S_Div,
                            (PrimType::I32(IntSign::Signed),BinOp::Rem) => Instr::I32_S_Rem,
                            (PrimType::I32(IntSign::Signed),BinOp::Shr) => Instr::I32_S_ShiftR,

                            (PrimType::I32(IntSign::Unsigned),BinOp::Lt) => Instr::I32_U_Lt,
                            (PrimType::I32(IntSign::Unsigned),BinOp::Gt) => { swap = true; Instr::I32_U_Lt },
                            (PrimType::I32(IntSign::Unsigned),BinOp::Le) => Instr::I32_U_LtEq,
                            (PrimType::I32(IntSign::Unsigned),BinOp::Ge) => { swap = true; Instr::I32_U_LtEq },
                            (PrimType::I32(IntSign::Unsigned),BinOp::Div) => Instr::I32_U_Div,
                            (PrimType::I32(IntSign::Unsigned),BinOp::Rem) => Instr::I32_U_Rem,
                            (PrimType::I32(IntSign::Unsigned),BinOp::Shr) => Instr::I32_U_ShiftR,

                            (PrimType::Bool,BinOp::BitAnd) => Instr::Bool_And,
                            //(PrimType::I32,BinOp::Div) => Instr::I32_Div,

                            _ => panic!("no op: {:?} {:?}",prim,op)
                        };

                        if swap {
                            self.out_func.instr.push(ctor(dst_slot,rhs,lhs));
                        } else {
                            self.out_func.instr.push(ctor(dst_slot,lhs,rhs));
                        }
                    }
                    Rvalue::UnaryOp(op,arg) => {
                        let ty = arg.ty(self.locals,self.tcx);
                        let arg = self.operand_get_slot(&arg);

                        let prim = PrimType::from(ty);

                        let ctor = match (prim,op) {
                            (PrimType::I32(IntSign::Signed),UnOp::Neg) => Instr::I32_Neg,
                            (PrimType::I32(_),UnOp::Not) => Instr::I32_Not,
                            _ => panic!("no op: {:?} {:?}",prim,op)
                        };

                        self.out_func.instr.push(ctor(dst_slot,arg));
                    }
                    Rvalue::Cast(cast,arg,res_ty) => {
                        let arg_ty = PrimType::from(arg.ty(self.locals,self.tcx));
                        let res_ty = PrimType::from(*res_ty);

                        let arg = self.operand_get_slot(&arg);

                        let ctor = match (cast,arg_ty,res_ty) {
                            (CastKind::IntToInt,PrimType::I32(_),PrimType::I128(_)) => Instr::I128_S_Widen_32,
                            _ => panic!("no cast: {:?} {:?} {:?}",cast,arg_ty,res_ty)
                        };

                        self.out_func.instr.push(ctor(dst_slot,arg));
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
                let name = format!("{:?}",func);
                let ct = if name == "_builtin::print_int" {
                    CallTarget::PrintInt
                } else if name == "_builtin::print_uint" {
                    CallTarget::PrintUint
                } else if name == "_builtin::print_bool" {
                    CallTarget::PrintBool
                } else {
                    panic!("todo call {:?}",func);
                };

                let base_slot = self.stack_info.align_for_call();
                // todo dst slot
                let arg_slots: Vec<_> = args.iter().map(|arg| {
                    let ty = arg.ty(self.locals,self.tcx);
                    self.stack_info.alloc_slot(SlotId::Temp, ty)
                }).collect();

                //println!("> {:?}",arg_slots);

                for (arg,slot) in args.iter().zip(arg_slots.iter()) {
                    self.operand_move_to_slot(arg, *slot);
                }

                self.out_func.instr.push(Instr::Call(base_slot, ct));
            }
            TerminatorKind::Assert{cond,expected,msg,target,..} => {
                // todo asserts
                self.compile_goto(*target);
            }
            TerminatorKind::Return => {
                self.out_func.instr.push(Instr::Return);
            }
            TerminatorKind::Resume => {
                self.out_func.instr.push(Instr::Bad);
            }
            _ => panic!("? {:?}",term.kind)
        }
        self.stack_info.free_all_temporary();
    }

    fn compile_goto(&mut self, target: mir::BasicBlock) {
        if self.current_block + 1 != target.index() {
            panic!("todo jump");
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
                            4 => self.out_func.instr.push(Instr::I32_Const(dst_slot, raw as i32)),
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

                match size {
                    1 => self.out_func.instr.push(Instr::MovSS1(dst_slot,src_slot)),
                    4 => self.out_func.instr.push(Instr::MovSS4(dst_slot,src_slot)),
                    16 => self.out_func.instr.push(Instr::MovSS16(dst_slot,src_slot)),
                    _ => panic!("todo move {:?} -> {:?} / {}",src_slot,dst_slot,size)
                }
            }
            Operand::Constant(constant) => {
                match constant.literal {
                    ConstantKind::Val(ConstValue::Scalar(Scalar::Int(scalar_int)),_) => {
                        let size = scalar_int.size();
                        let raw = scalar_int.assert_bits(size);
                        match size.bytes() {
                            4 => self.out_func.instr.push(Instr::I32_Const(dst_slot, raw as i32)),
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

#[derive(Debug,Copy,Clone)]
enum PrimType {
    I32(IntSign),
    I128(IntSign),
    Bool
}

#[derive(Debug,Copy,Clone)]
enum IntSign {
    Signed,
    Unsigned
}

impl PrimType {
    pub fn from(ty: Ty) -> Self {
        let kind = ty.kind();
        match kind {
            //TyKind::
            TyKind::Int(IntTy::I32) => PrimType::I32(IntSign::Signed),
            TyKind::Int(IntTy::I128) => PrimType::I128(IntSign::Signed),

            TyKind::Uint(UintTy::U32) => PrimType::I32(IntSign::Unsigned),
            TyKind::Uint(UintTy::U128) => PrimType::I128(IntSign::Unsigned),

            TyKind::Bool => PrimType::Bool,
            _ => panic!("can't convert {:?} to primitive type",ty)
        }
    }
}
