/// Just a few utility functions for picking the right bytecode instructions

use rustc_ast::ast::LitKind;
use rustc_middle::mir::{UnOp, BinOp};
use rustc_middle::ty::Ty;

use crate::{vm::instr::{Slot, Instr}, layout::{Layout, LayoutKind, IntSign}};

pub fn literal(lit: &LitKind, size: u32, slot: Slot, neg: bool) -> Instr {
    match lit {
        LitKind::Int(n,_) => {
            let mut n = *n as i128;
            if neg {
                n = -n;
            }
            match size {
                1  => Instr::I8_Const(slot, n as i8),
                2  => Instr::I16_Const(slot, n as i16),
                4  => Instr::I32_Const(slot, n as i32),
                8  => Instr::I64_Const(slot, n as i64),
                16 => Instr::I128_Const(slot, Box::new(n)),
                _ => panic!("int size {}",size)
            }
        }
        _ => panic!("pick literal {:?}",lit)
    }
}

pub fn unary(op: UnOp, layout: &Layout) -> fn(Slot,Slot) -> Instr {
    match (op,&layout.kind,layout.size) {
        (UnOp::Neg,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_Neg,
        (UnOp::Neg,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_Neg,
        (UnOp::Neg,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_Neg,
        (UnOp::Neg,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_Neg,
        (UnOp::Neg,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_Neg,

        (UnOp::Not,LayoutKind::Int(_),1) => Instr::I8_Not,
        (UnOp::Not,LayoutKind::Int(_),2) => Instr::I16_Not,
        (UnOp::Not,LayoutKind::Int(_),4) => Instr::I32_Not,
        (UnOp::Not,LayoutKind::Int(_),8) => Instr::I64_Not,
        (UnOp::Not,LayoutKind::Int(_),16) => Instr::I128_Not,

        _ => panic!("no unary op: {:?} {:?} {:?}",op,&layout.kind,layout.size)
    }
}

pub fn binary(op: BinOp, layout: &Layout) -> (fn(Slot,Slot,Slot) -> Instr,bool) {

    let mut swap = false;
    let ctor = match (op,&layout.kind,layout.size) {
        /*(LayoutKind::Int(_),1,BinOp::Eq) => Instr::I8_Eq,
        (LayoutKind::Int(_),1,BinOp::Ne) => Instr::I8_NotEq,

        (LayoutKind::Int(_),1,BinOp::Shl) => Instr::I8_ShiftL,
        (LayoutKind::Int(IntSign::Signed),1,BinOp::Lt) => Instr::I8_S_Lt,
        (LayoutKind::Int(IntSign::Signed),1,BinOp::Gt) => { swap = true; Instr::I8_S_Lt },
        (LayoutKind::Int(IntSign::Signed),1,BinOp::Le) => Instr::I8_S_LtEq,
        (LayoutKind::Int(IntSign::Signed),1,BinOp::Ge) => { swap = true; Instr::I8_S_LtEq },

        (LayoutKind::Int(IntSign::Signed),1,BinOp::Rem) => Instr::I8_S_Rem,
        (LayoutKind::Int(IntSign::Signed),1,BinOp::Shr) => Instr::I8_S_ShiftR,
        (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Lt) => Instr::I8_U_Lt,
        (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Gt) => { swap = true; Instr::I8_U_Lt },
        (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Le) => Instr::I8_U_LtEq,
        (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Ge) => { swap = true; Instr::I8_U_LtEq },
        (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Div) => Instr::I8_U_Div,
        (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Rem) => Instr::I8_U_Rem,
        (LayoutKind::Int(IntSign::Unsigned),1,BinOp::Shr) => Instr::I8_U_ShiftR,*/

        (BinOp::Eq,LayoutKind::Int(_),1) => Instr::I8_Eq,
        (BinOp::Eq,LayoutKind::Int(_),2) => Instr::I16_Eq,
        (BinOp::Eq,LayoutKind::Int(_),4) => Instr::I32_Eq,
        (BinOp::Eq,LayoutKind::Int(_),8) => Instr::I64_Eq,
        (BinOp::Eq,LayoutKind::Int(_),16) => Instr::I128_Eq,
        (BinOp::Ne,LayoutKind::Int(_),1) => Instr::I8_NotEq,
        (BinOp::Ne,LayoutKind::Int(_),2) => Instr::I16_NotEq,
        (BinOp::Ne,LayoutKind::Int(_),4) => Instr::I32_NotEq,
        (BinOp::Ne,LayoutKind::Int(_),8) => Instr::I64_NotEq,
        (BinOp::Ne,LayoutKind::Int(_),16) => Instr::I128_NotEq,

        (BinOp::Add,LayoutKind::Int(_),1) => Instr::I8_Add,
        (BinOp::Add,LayoutKind::Int(_),2) => Instr::I16_Add,
        (BinOp::Add,LayoutKind::Int(_),4) => Instr::I32_Add,
        (BinOp::Add,LayoutKind::Int(_),8) => Instr::I64_Add,
        (BinOp::Add,LayoutKind::Int(_),16) => Instr::I128_Add,
        (BinOp::Sub,LayoutKind::Int(_),1) => Instr::I8_Sub,
        (BinOp::Sub,LayoutKind::Int(_),2) => Instr::I16_Sub,
        (BinOp::Sub,LayoutKind::Int(_),4) => Instr::I32_Sub,
        (BinOp::Sub,LayoutKind::Int(_),8) => Instr::I64_Sub,
        (BinOp::Sub,LayoutKind::Int(_),16) => Instr::I128_Sub,
        (BinOp::Mul,LayoutKind::Int(_),1) => Instr::I8_Mul,
        (BinOp::Mul,LayoutKind::Int(_),2) => Instr::I16_Mul,
        (BinOp::Mul,LayoutKind::Int(_),4) => Instr::I32_Mul,
        (BinOp::Mul,LayoutKind::Int(_),8) => Instr::I64_Mul,
        (BinOp::Mul,LayoutKind::Int(_),16) => Instr::I128_Mul,

        (BinOp::BitOr,LayoutKind::Int(_),1) => Instr::I8_Or,
        (BinOp::BitOr,LayoutKind::Int(_),2) => Instr::I16_Or,
        (BinOp::BitOr,LayoutKind::Int(_),4) => Instr::I32_Or,
        (BinOp::BitOr,LayoutKind::Int(_),8) => Instr::I64_Or,
        (BinOp::BitOr,LayoutKind::Int(_),16) => Instr::I128_Or,
        (BinOp::BitAnd,LayoutKind::Int(_),1) => Instr::I8_And,
        (BinOp::BitAnd,LayoutKind::Int(_),2) => Instr::I16_And,
        (BinOp::BitAnd,LayoutKind::Int(_),4) => Instr::I32_And,
        (BinOp::BitAnd,LayoutKind::Int(_),8) => Instr::I64_And,
        (BinOp::BitAnd,LayoutKind::Int(_),16) => Instr::I128_And,
        (BinOp::BitXor,LayoutKind::Int(_),1) => Instr::I8_Xor,
        (BinOp::BitXor,LayoutKind::Int(_),2) => Instr::I16_Xor,
        (BinOp::BitXor,LayoutKind::Int(_),4) => Instr::I32_Xor,
        (BinOp::BitXor,LayoutKind::Int(_),8) => Instr::I64_Xor,
        (BinOp::BitXor,LayoutKind::Int(_),16) => Instr::I128_Xor,
        (BinOp::Shl,LayoutKind::Int(_),1) => Instr::I8_ShiftL,
        (BinOp::Shl,LayoutKind::Int(_),2) => Instr::I16_ShiftL,
        (BinOp::Shl,LayoutKind::Int(_),4) => Instr::I32_ShiftL,
        (BinOp::Shl,LayoutKind::Int(_),8) => Instr::I64_ShiftL,
        (BinOp::Shl,LayoutKind::Int(_),16) => Instr::I128_ShiftL,

        (BinOp::Lt,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_Lt,
        (BinOp::Lt,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_Lt,
        (BinOp::Lt,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_Lt,
        (BinOp::Lt,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_Lt,
        (BinOp::Lt,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_Lt,
        (BinOp::Gt,LayoutKind::Int(IntSign::Signed),1) => { swap = true; Instr::I8_S_Lt }
        (BinOp::Gt,LayoutKind::Int(IntSign::Signed),2) => { swap = true; Instr::I16_S_Lt }
        (BinOp::Gt,LayoutKind::Int(IntSign::Signed),4) => { swap = true; Instr::I32_S_Lt }
        (BinOp::Gt,LayoutKind::Int(IntSign::Signed),8) => { swap = true; Instr::I64_S_Lt }
        (BinOp::Gt,LayoutKind::Int(IntSign::Signed),16) => { swap = true; Instr::I128_S_Lt }
        (BinOp::Le,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_LtEq,
        (BinOp::Le,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_LtEq,
        (BinOp::Le,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_LtEq,
        (BinOp::Le,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_LtEq,
        (BinOp::Le,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_LtEq,
        (BinOp::Ge,LayoutKind::Int(IntSign::Signed),1) => { swap = true; Instr::I8_S_LtEq }
        (BinOp::Ge,LayoutKind::Int(IntSign::Signed),2) => { swap = true; Instr::I16_S_LtEq }
        (BinOp::Ge,LayoutKind::Int(IntSign::Signed),4) => { swap = true; Instr::I32_S_LtEq }
        (BinOp::Ge,LayoutKind::Int(IntSign::Signed),8) => { swap = true; Instr::I64_S_LtEq }
        (BinOp::Ge,LayoutKind::Int(IntSign::Signed),16) => { swap = true; Instr::I128_S_LtEq }

        (BinOp::Div,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_Div,
        (BinOp::Div,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_Div,
        (BinOp::Div,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_Div,
        (BinOp::Div,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_Div,
        (BinOp::Div,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_Div,
        (BinOp::Rem,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_Rem,
        (BinOp::Rem,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_Rem,
        (BinOp::Rem,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_Rem,
        (BinOp::Rem,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_Rem,
        (BinOp::Rem,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_Rem,

        (BinOp::Shr,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_ShiftR,
        (BinOp::Shr,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_ShiftR,
        (BinOp::Shr,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_ShiftR,
        (BinOp::Shr,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_ShiftR,
        (BinOp::Shr,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_ShiftR,

        _ => panic!("no binary op: {:?} {:?} {:?}",&layout.kind,layout.size,op)
    };

    (ctor,swap)
}

pub fn cast<'tcx>(arg_ty: Ty<'tcx>, res_ty: Ty<'tcx>) -> fn(Slot,Slot) -> Instr {
    let arg_layout = Layout::from(arg_ty);
    let res_layout = Layout::from(res_ty);

    match (&arg_layout.kind,&res_layout.kind) {
        (LayoutKind::Int(_),LayoutKind::Int(_)) => {
            
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
        (LayoutKind::Float,LayoutKind::Float) => {
            match (arg_layout.size,res_layout.size) {
                (4,8) => Instr::F64_From_F32,
                (8,4) => Instr::F32_From_F64,
                _ => panic!("no float2float cast for: {:?} {:?}",arg_layout.size,res_layout.size)
            }
        }
        (LayoutKind::Int(_),LayoutKind::Float) => {
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
        (LayoutKind::Float,LayoutKind::Int(_)) => {
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
        _ => panic!("no cast: {:?} -> {:?}",arg_layout.kind,res_layout.kind)
    }
}
