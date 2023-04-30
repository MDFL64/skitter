/// Just a few utility functions for picking the right bytecode instructions

use rustc_ast::ast::LitKind;
use rustc_middle::mir::{UnOp, BinOp};
use rustc_middle::ty::Ty;

use std::str::FromStr;

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
        LitKind::Float(sym,_) => {
            match size {
                4 => {
                    let mut n = f32::from_str(sym.as_str()).unwrap();
                    if neg {
                        n = -n;
                    }
                    let x: i32 = unsafe { std::mem::transmute(n) };
                    Instr::I32_Const(slot, x)
                }
                8 => {
                    let mut n = f64::from_str(sym.as_str()).unwrap();
                    if neg {
                        n = -n;
                    }
                    let x: i64 = unsafe { std::mem::transmute(n) };
                    Instr::I64_Const(slot, x)
                }
                _ => panic!("float size {}",size)
            }
        }
        LitKind::Char(c) => {
            let n = *c as i32;
            Instr::I32_Const(slot, n)
        }
        LitKind::Bool(b) => {
            Instr::I8_Const(slot, *b as i8)
        }
        _ => panic!("pick literal {:?}",lit)
    }
}

pub fn copy<'tcx>(dst: Slot, src: Slot, size: u32) -> Option<Instr<'tcx>> {
    if size == 0 {
        None
    } else {
        Some(match size {
            1  => Instr::MovSS1(dst, src),
            2  => Instr::MovSS2(dst, src),
            4  => Instr::MovSS4(dst, src),
            8  => Instr::MovSS8(dst, src),
            16 => Instr::MovSS16(dst, src),
            _ => Instr::MovSSN(dst, src, size)
        })
    }
}

pub fn copy_from_ptr<'tcx>(dst: Slot, src: Slot, size: u32, offset: i32) -> Option<Instr<'tcx>> {
    if size == 0 {
        None
    } else {
        Some(match size {
            1  => Instr::MovSP1(dst, src, offset),
            2  => Instr::MovSP2(dst, src, offset),
            4  => Instr::MovSP4(dst, src, offset),
            8  => Instr::MovSP8(dst, src, offset),
            16 => Instr::MovSP16(dst, src, offset),
            _ => panic!("copy from ptr {}",size)
        })
    }
}

pub fn copy_to_ptr<'tcx>(dst: Slot, src: Slot, size: u32, offset: i32) -> Option<Instr<'tcx>> {
    if size == 0 {
        None
    } else {
        Some(match size {
            1  => Instr::MovPS1(dst, src, offset),
            2  => Instr::MovPS2(dst, src, offset),
            4  => Instr::MovPS4(dst, src, offset),
            8  => Instr::MovPS8(dst, src, offset),
            16 => Instr::MovPS16(dst, src, offset),
            _ => panic!("copy to ptr {}",size)
        })
    }
}

pub fn unary<'tcx>(op: UnOp, layout: &Layout) -> fn(Slot,Slot) -> Instr<'tcx> {
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

        (UnOp::Neg,LayoutKind::Float,4) => Instr::F32_Neg,
        (UnOp::Neg,LayoutKind::Float,8) => Instr::F64_Neg,

        (UnOp::Not,LayoutKind::Bool,1) => Instr::Bool_Not,

        _ => panic!("no unary op: {:?} {:?} {:?}",op,&layout.kind,layout.size)
    }
}

pub fn binary<'tcx>(op: BinOp, layout: &Layout) -> (fn(Slot,Slot,Slot) -> Instr<'tcx>,bool) {

    let mut swap = false;
    let ctor = match (op,&layout.kind,layout.size) {

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

        (BinOp::Lt,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_Lt,
        (BinOp::Lt,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_Lt,
        (BinOp::Lt,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_Lt,
        (BinOp::Lt,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_Lt,
        (BinOp::Lt,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_Lt,
        (BinOp::Gt,LayoutKind::Int(IntSign::Unsigned),1) => { swap = true; Instr::I8_U_Lt }
        (BinOp::Gt,LayoutKind::Int(IntSign::Unsigned),2) => { swap = true; Instr::I16_U_Lt }
        (BinOp::Gt,LayoutKind::Int(IntSign::Unsigned),4) => { swap = true; Instr::I32_U_Lt }
        (BinOp::Gt,LayoutKind::Int(IntSign::Unsigned),8) => { swap = true; Instr::I64_U_Lt }
        (BinOp::Gt,LayoutKind::Int(IntSign::Unsigned),16) => { swap = true; Instr::I128_U_Lt }
        (BinOp::Le,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_LtEq,
        (BinOp::Le,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_LtEq,
        (BinOp::Le,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_LtEq,
        (BinOp::Le,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_LtEq,
        (BinOp::Le,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_LtEq,
        (BinOp::Ge,LayoutKind::Int(IntSign::Unsigned),1) => { swap = true; Instr::I8_U_LtEq }
        (BinOp::Ge,LayoutKind::Int(IntSign::Unsigned),2) => { swap = true; Instr::I16_U_LtEq }
        (BinOp::Ge,LayoutKind::Int(IntSign::Unsigned),4) => { swap = true; Instr::I32_U_LtEq }
        (BinOp::Ge,LayoutKind::Int(IntSign::Unsigned),8) => { swap = true; Instr::I64_U_LtEq }
        (BinOp::Ge,LayoutKind::Int(IntSign::Unsigned),16) => { swap = true; Instr::I128_U_LtEq }

        (BinOp::Div,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_Div,
        (BinOp::Div,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_Div,
        (BinOp::Div,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_Div,
        (BinOp::Div,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_Div,
        (BinOp::Div,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_Div,
        (BinOp::Rem,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_Rem,
        (BinOp::Rem,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_Rem,
        (BinOp::Rem,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_Rem,
        (BinOp::Rem,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_Rem,
        (BinOp::Rem,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_Rem,

        (BinOp::Shr,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_ShiftR,
        (BinOp::Shr,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_ShiftR,
        (BinOp::Shr,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_ShiftR,
        (BinOp::Shr,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_ShiftR,
        (BinOp::Shr,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_ShiftR,

        (BinOp::Eq,LayoutKind::Float,4) => Instr::F32_Eq,
        (BinOp::Eq,LayoutKind::Float,8) => Instr::F64_Eq,
        (BinOp::Ne,LayoutKind::Float,4) => Instr::F32_NotEq,
        (BinOp::Ne,LayoutKind::Float,8) => Instr::F64_NotEq,
        (BinOp::Lt,LayoutKind::Float,4) => Instr::F32_Lt,
        (BinOp::Lt,LayoutKind::Float,8) => Instr::F64_Lt,
        (BinOp::Gt,LayoutKind::Float,4) => Instr::F32_Gt,
        (BinOp::Gt,LayoutKind::Float,8) => Instr::F64_Gt,

        (BinOp::Le,LayoutKind::Float,4) => Instr::F32_LtEq,
        (BinOp::Le,LayoutKind::Float,8) => Instr::F64_LtEq,
        (BinOp::Ge,LayoutKind::Float,4) => Instr::F32_GtEq,
        (BinOp::Ge,LayoutKind::Float,8) => Instr::F64_GtEq,

        (BinOp::Add,LayoutKind::Float,4) => Instr::F32_Add,
        (BinOp::Add,LayoutKind::Float,8) => Instr::F64_Add,
        (BinOp::Sub,LayoutKind::Float,4) => Instr::F32_Sub,
        (BinOp::Sub,LayoutKind::Float,8) => Instr::F64_Sub,
        (BinOp::Mul,LayoutKind::Float,4) => Instr::F32_Mul,
        (BinOp::Mul,LayoutKind::Float,8) => Instr::F64_Mul,
        (BinOp::Div,LayoutKind::Float,4) => Instr::F32_Div,
        (BinOp::Div,LayoutKind::Float,8) => Instr::F64_Div,
        (BinOp::Rem,LayoutKind::Float,4) => Instr::F32_Rem,
        (BinOp::Rem,LayoutKind::Float,8) => Instr::F64_Rem,

        (BinOp::Eq,LayoutKind::Bool,1) => Instr::I8_Eq,
        (BinOp::Ne,LayoutKind::Bool,1) => Instr::I8_NotEq,

        (BinOp::BitOr,LayoutKind::Bool,1) => Instr::I8_Or,
        (BinOp::BitAnd,LayoutKind::Bool,1) => Instr::I8_And,
        (BinOp::BitXor,LayoutKind::Bool,1) => Instr::I8_Xor,

        _ => panic!("no binary op: {:?} {:?} {:?}",&layout.kind,layout.size,op)
    };

    (ctor,swap)
}

pub fn cast<'tcx>(arg_layout: &Layout, res_layout: &Layout) -> fn(Slot,Slot) -> Instr<'tcx> {

    match (&arg_layout.kind,&res_layout.kind) {
        (LayoutKind::Int(_),LayoutKind::Int(_)) |
        (LayoutKind::Bool,LayoutKind::Int(_)) |
        (LayoutKind::Ptr,LayoutKind::Int(_)) |
        (LayoutKind::Int(_),LayoutKind::Ptr) => {
            
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
