/// Just a few utility functions for picking the right bytecode instructions

use crate::ir::{UnaryOp, BinaryOp};
use crate::{vm::instr::{Slot, Instr}, layout::{Layout, LayoutKind, IntSign}};

pub fn literal<'tcx>(n: i128, size: u32, slot: Slot) -> Instr<'tcx> {
    match size {
        1  => Instr::I8_Const(slot, n as i8),
        2  => Instr::I16_Const(slot, n as i16),
        4  => Instr::I32_Const(slot, n as i32),
        8  => Instr::I64_Const(slot, n as i64),
        16 => Instr::I128_Const(slot, Box::new(n)),
        _ => panic!("int size {}",size)
    }
}

// TODO NONE OF THESE ACCOUNT FOR ALIGNMENT!
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

pub fn unary<'tcx>(op: UnaryOp, layout: &Layout) -> fn(Slot,Slot) -> Instr<'tcx> {
    match (op,&layout.kind,layout.assert_size()) {
        (UnaryOp::Neg,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_Neg,
        (UnaryOp::Neg,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_Neg,
        (UnaryOp::Neg,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_Neg,
        (UnaryOp::Neg,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_Neg,
        (UnaryOp::Neg,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_Neg,

        (UnaryOp::Not,LayoutKind::Int(_),1) => Instr::I8_Not,
        (UnaryOp::Not,LayoutKind::Int(_),2) => Instr::I16_Not,
        (UnaryOp::Not,LayoutKind::Int(_),4) => Instr::I32_Not,
        (UnaryOp::Not,LayoutKind::Int(_),8) => Instr::I64_Not,
        (UnaryOp::Not,LayoutKind::Int(_),16) => Instr::I128_Not,

        (UnaryOp::Neg,LayoutKind::Float,4) => Instr::F32_Neg,
        (UnaryOp::Neg,LayoutKind::Float,8) => Instr::F64_Neg,

        (UnaryOp::Not,LayoutKind::Bool,1) => Instr::Bool_Not,

        _ => panic!("no unary op: {:?} {:?} {:?}",op,&layout.kind,layout.assert_size())
    }
}

pub fn binary<'tcx>(op: BinaryOp, layout: &Layout) -> (fn(Slot,Slot,Slot) -> Instr<'tcx>,bool) {

    let mut swap = false;
    let ctor = match (op,&layout.kind,layout.assert_size()) {

        (BinaryOp::Eq,LayoutKind::Int(_),1) => Instr::I8_Eq,
        (BinaryOp::Eq,LayoutKind::Int(_),2) => Instr::I16_Eq,
        (BinaryOp::Eq,LayoutKind::Int(_),4) => Instr::I32_Eq,
        (BinaryOp::Eq,LayoutKind::Int(_),8) => Instr::I64_Eq,
        (BinaryOp::Eq,LayoutKind::Int(_),16) => Instr::I128_Eq,
        (BinaryOp::NotEq,LayoutKind::Int(_),1) => Instr::I8_NotEq,
        (BinaryOp::NotEq,LayoutKind::Int(_),2) => Instr::I16_NotEq,
        (BinaryOp::NotEq,LayoutKind::Int(_),4) => Instr::I32_NotEq,
        (BinaryOp::NotEq,LayoutKind::Int(_),8) => Instr::I64_NotEq,
        (BinaryOp::NotEq,LayoutKind::Int(_),16) => Instr::I128_NotEq,

        (BinaryOp::Add,LayoutKind::Int(_),1) => Instr::I8_Add,
        (BinaryOp::Add,LayoutKind::Int(_),2) => Instr::I16_Add,
        (BinaryOp::Add,LayoutKind::Int(_),4) => Instr::I32_Add,
        (BinaryOp::Add,LayoutKind::Int(_),8) => Instr::I64_Add,
        (BinaryOp::Add,LayoutKind::Int(_),16) => Instr::I128_Add,
        (BinaryOp::Sub,LayoutKind::Int(_),1) => Instr::I8_Sub,
        (BinaryOp::Sub,LayoutKind::Int(_),2) => Instr::I16_Sub,
        (BinaryOp::Sub,LayoutKind::Int(_),4) => Instr::I32_Sub,
        (BinaryOp::Sub,LayoutKind::Int(_),8) => Instr::I64_Sub,
        (BinaryOp::Sub,LayoutKind::Int(_),16) => Instr::I128_Sub,
        (BinaryOp::Mul,LayoutKind::Int(_),1) => Instr::I8_Mul,
        (BinaryOp::Mul,LayoutKind::Int(_),2) => Instr::I16_Mul,
        (BinaryOp::Mul,LayoutKind::Int(_),4) => Instr::I32_Mul,
        (BinaryOp::Mul,LayoutKind::Int(_),8) => Instr::I64_Mul,
        (BinaryOp::Mul,LayoutKind::Int(_),16) => Instr::I128_Mul,

        (BinaryOp::BitOr,LayoutKind::Int(_),1) => Instr::I8_Or,
        (BinaryOp::BitOr,LayoutKind::Int(_),2) => Instr::I16_Or,
        (BinaryOp::BitOr,LayoutKind::Int(_),4) => Instr::I32_Or,
        (BinaryOp::BitOr,LayoutKind::Int(_),8) => Instr::I64_Or,
        (BinaryOp::BitOr,LayoutKind::Int(_),16) => Instr::I128_Or,
        (BinaryOp::BitAnd,LayoutKind::Int(_),1) => Instr::I8_And,
        (BinaryOp::BitAnd,LayoutKind::Int(_),2) => Instr::I16_And,
        (BinaryOp::BitAnd,LayoutKind::Int(_),4) => Instr::I32_And,
        (BinaryOp::BitAnd,LayoutKind::Int(_),8) => Instr::I64_And,
        (BinaryOp::BitAnd,LayoutKind::Int(_),16) => Instr::I128_And,
        (BinaryOp::BitXor,LayoutKind::Int(_),1) => Instr::I8_Xor,
        (BinaryOp::BitXor,LayoutKind::Int(_),2) => Instr::I16_Xor,
        (BinaryOp::BitXor,LayoutKind::Int(_),4) => Instr::I32_Xor,
        (BinaryOp::BitXor,LayoutKind::Int(_),8) => Instr::I64_Xor,
        (BinaryOp::BitXor,LayoutKind::Int(_),16) => Instr::I128_Xor,
        (BinaryOp::ShiftL,LayoutKind::Int(_),1) => Instr::I8_ShiftL,
        (BinaryOp::ShiftL,LayoutKind::Int(_),2) => Instr::I16_ShiftL,
        (BinaryOp::ShiftL,LayoutKind::Int(_),4) => Instr::I32_ShiftL,
        (BinaryOp::ShiftL,LayoutKind::Int(_),8) => Instr::I64_ShiftL,
        (BinaryOp::ShiftL,LayoutKind::Int(_),16) => Instr::I128_ShiftL,

        (BinaryOp::Lt,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_Lt,
        (BinaryOp::Lt,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_Lt,
        (BinaryOp::Lt,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_Lt,
        (BinaryOp::Lt,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_Lt,
        (BinaryOp::Lt,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_Lt,
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Signed),1) => { swap = true; Instr::I8_S_Lt }
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Signed),2) => { swap = true; Instr::I16_S_Lt }
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Signed),4) => { swap = true; Instr::I32_S_Lt }
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Signed),8) => { swap = true; Instr::I64_S_Lt }
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Signed),16) => { swap = true; Instr::I128_S_Lt }
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_LtEq,
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_LtEq,
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_LtEq,
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_LtEq,
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_LtEq,
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Signed),1) => { swap = true; Instr::I8_S_LtEq }
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Signed),2) => { swap = true; Instr::I16_S_LtEq }
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Signed),4) => { swap = true; Instr::I32_S_LtEq }
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Signed),8) => { swap = true; Instr::I64_S_LtEq }
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Signed),16) => { swap = true; Instr::I128_S_LtEq }

        (BinaryOp::Div,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_Div,
        (BinaryOp::Div,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_Div,
        (BinaryOp::Div,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_Div,
        (BinaryOp::Div,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_Div,
        (BinaryOp::Div,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_Div,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_Rem,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_Rem,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_Rem,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_Rem,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_Rem,

        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Signed),1) => Instr::I8_S_ShiftR,
        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Signed),2) => Instr::I16_S_ShiftR,
        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Signed),4) => Instr::I32_S_ShiftR,
        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Signed),8) => Instr::I64_S_ShiftR,
        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Signed),16) => Instr::I128_S_ShiftR,

        (BinaryOp::Lt,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_Lt,
        (BinaryOp::Lt,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_Lt,
        (BinaryOp::Lt,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_Lt,
        (BinaryOp::Lt,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_Lt,
        (BinaryOp::Lt,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_Lt,
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Unsigned),1) => { swap = true; Instr::I8_U_Lt }
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Unsigned),2) => { swap = true; Instr::I16_U_Lt }
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Unsigned),4) => { swap = true; Instr::I32_U_Lt }
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Unsigned),8) => { swap = true; Instr::I64_U_Lt }
        (BinaryOp::Gt,LayoutKind::Int(IntSign::Unsigned),16) => { swap = true; Instr::I128_U_Lt }
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_LtEq,
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_LtEq,
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_LtEq,
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_LtEq,
        (BinaryOp::LtEq,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_LtEq,
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Unsigned),1) => { swap = true; Instr::I8_U_LtEq }
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Unsigned),2) => { swap = true; Instr::I16_U_LtEq }
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Unsigned),4) => { swap = true; Instr::I32_U_LtEq }
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Unsigned),8) => { swap = true; Instr::I64_U_LtEq }
        (BinaryOp::GtEq,LayoutKind::Int(IntSign::Unsigned),16) => { swap = true; Instr::I128_U_LtEq }

        (BinaryOp::Div,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_Div,
        (BinaryOp::Div,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_Div,
        (BinaryOp::Div,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_Div,
        (BinaryOp::Div,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_Div,
        (BinaryOp::Div,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_Div,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_Rem,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_Rem,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_Rem,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_Rem,
        (BinaryOp::Rem,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_Rem,

        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Unsigned),1) => Instr::I8_U_ShiftR,
        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Unsigned),2) => Instr::I16_U_ShiftR,
        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Unsigned),4) => Instr::I32_U_ShiftR,
        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Unsigned),8) => Instr::I64_U_ShiftR,
        (BinaryOp::ShiftR,LayoutKind::Int(IntSign::Unsigned),16) => Instr::I128_U_ShiftR,

        (BinaryOp::Eq,LayoutKind::Float,4) => Instr::F32_Eq,
        (BinaryOp::Eq,LayoutKind::Float,8) => Instr::F64_Eq,
        (BinaryOp::NotEq,LayoutKind::Float,4) => Instr::F32_NotEq,
        (BinaryOp::NotEq,LayoutKind::Float,8) => Instr::F64_NotEq,
        (BinaryOp::Lt,LayoutKind::Float,4) => Instr::F32_Lt,
        (BinaryOp::Lt,LayoutKind::Float,8) => Instr::F64_Lt,
        (BinaryOp::Gt,LayoutKind::Float,4) => Instr::F32_Gt,
        (BinaryOp::Gt,LayoutKind::Float,8) => Instr::F64_Gt,

        (BinaryOp::LtEq,LayoutKind::Float,4) => Instr::F32_LtEq,
        (BinaryOp::LtEq,LayoutKind::Float,8) => Instr::F64_LtEq,
        (BinaryOp::GtEq,LayoutKind::Float,4) => Instr::F32_GtEq,
        (BinaryOp::GtEq,LayoutKind::Float,8) => Instr::F64_GtEq,

        (BinaryOp::Add,LayoutKind::Float,4) => Instr::F32_Add,
        (BinaryOp::Add,LayoutKind::Float,8) => Instr::F64_Add,
        (BinaryOp::Sub,LayoutKind::Float,4) => Instr::F32_Sub,
        (BinaryOp::Sub,LayoutKind::Float,8) => Instr::F64_Sub,
        (BinaryOp::Mul,LayoutKind::Float,4) => Instr::F32_Mul,
        (BinaryOp::Mul,LayoutKind::Float,8) => Instr::F64_Mul,
        (BinaryOp::Div,LayoutKind::Float,4) => Instr::F32_Div,
        (BinaryOp::Div,LayoutKind::Float,8) => Instr::F64_Div,
        (BinaryOp::Rem,LayoutKind::Float,4) => Instr::F32_Rem,
        (BinaryOp::Rem,LayoutKind::Float,8) => Instr::F64_Rem,

        (BinaryOp::Eq,LayoutKind::Bool,1) => Instr::I8_Eq,
        (BinaryOp::NotEq,LayoutKind::Bool,1) => Instr::I8_NotEq,

        (BinaryOp::BitOr,LayoutKind::Bool,1) => Instr::I8_Or,
        (BinaryOp::BitAnd,LayoutKind::Bool,1) => Instr::I8_And,
        (BinaryOp::BitXor,LayoutKind::Bool,1) => Instr::I8_Xor,

        _ => panic!("no binary op: {:?} {:?} {:?}",&layout.kind,layout.assert_size(),op)
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

            if arg_layout.assert_size() >= res_layout.assert_size() {
                match res_layout.assert_size() {
                    1 => Instr::MovSS1,
                    2 => Instr::MovSS2,
                    4 => Instr::MovSS4,
                    8 => Instr::MovSS8,
                    16 => Instr::MovSS16,
                    _ => panic!("no int2int cast for: {:?} {:?} {:?}",arg_layout.assert_size(),res_layout.assert_size(),sign)
                }
            } else {
                match (arg_layout.assert_size(),res_layout.assert_size(),sign) {
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

                    _ => panic!("no int2int cast for: {:?} {:?} {:?}",arg_layout.assert_size(),res_layout.assert_size(),sign)
                }
            }
        }
        (LayoutKind::Float,LayoutKind::Float) => {
            match (arg_layout.assert_size(),res_layout.assert_size()) {
                (4,8) => Instr::F64_From_F32,
                (8,4) => Instr::F32_From_F64,
                _ => panic!("no float2float cast for: {:?} {:?}",arg_layout.assert_size(),res_layout.assert_size())
            }
        }
        (LayoutKind::Int(_),LayoutKind::Float) => {
            let sign = arg_layout.sign();
            match (arg_layout.assert_size(),res_layout.assert_size(),sign) {

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

                _ => panic!("no int2float cast for: {:?} {:?} {:?}",arg_layout.assert_size(),res_layout.assert_size(),sign)
            }
        }
        (LayoutKind::Float,LayoutKind::Int(_)) => {
            let sign = res_layout.sign();
            match (arg_layout.assert_size(),res_layout.assert_size(),sign) {

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

                _ => panic!("no int2float cast for: {:?} {:?} {:?}",arg_layout.assert_size(),res_layout.assert_size(),sign)
            }
        }
        _ => panic!("no cast: {:?} -> {:?}",arg_layout.kind,res_layout.kind)
    }
}
