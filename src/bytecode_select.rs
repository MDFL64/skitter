/// Just a few utility functions for picking the right bytecode instructions

use crate::ir::{UnaryOp, BinaryOp};
use crate::types::{TypeKind, IntWidth, IntSign, Type};
use crate::{vm::instr::{Slot, Instr}, layout::{Layout}};

pub fn literal(n: i128, size: u32, slot: Slot) -> Instr {
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
pub fn copy(dst: Slot, src: Slot, size: u32) -> Option<Instr> {
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

pub fn copy_from_ptr(dst: Slot, src: Slot, size: u32, offset: i32) -> Option<Instr> {
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

pub fn copy_to_ptr(dst: Slot, src: Slot, size: u32, offset: i32) -> Option<Instr> {
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

pub fn unary(op: UnaryOp, ty: Type) -> fn(Slot,Slot) -> Instr {
    let size = 0xFFFF;
    match (op,ty.kind(),size) {
        (UnaryOp::Neg,TypeKind::Int(_,IntSign::Signed),1) => Instr::I8_Neg,
        (UnaryOp::Neg,TypeKind::Int(_,IntSign::Signed),2) => Instr::I16_Neg,
        (UnaryOp::Neg,TypeKind::Int(_,IntSign::Signed),4) => Instr::I32_Neg,
        (UnaryOp::Neg,TypeKind::Int(_,IntSign::Signed),8) => Instr::I64_Neg,
        (UnaryOp::Neg,TypeKind::Int(_,IntSign::Signed),16) => Instr::I128_Neg,

        (UnaryOp::Not,TypeKind::Int(..),1) => Instr::I8_Not,
        (UnaryOp::Not,TypeKind::Int(..),2) => Instr::I16_Not,
        (UnaryOp::Not,TypeKind::Int(..),4) => Instr::I32_Not,
        (UnaryOp::Not,TypeKind::Int(..),8) => Instr::I64_Not,
        (UnaryOp::Not,TypeKind::Int(..),16) => Instr::I128_Not,

        (UnaryOp::Neg,TypeKind::Float(..),4) => Instr::F32_Neg,
        (UnaryOp::Neg,TypeKind::Float(..),8) => Instr::F64_Neg,

        (UnaryOp::Not,TypeKind::Bool,1) => Instr::Bool_Not,

        _ => panic!("no unary op: {:?} {:?}",op,ty)
    }
}

pub fn binary(op: BinaryOp, ty: Type) -> (fn(Slot,Slot,Slot) -> Instr,bool) {
    let size = 0xFFFF;

    let mut swap = false;
    let ctor = match (op,ty.kind(),size) {

        (BinaryOp::Eq,TypeKind::Int(..),1) => Instr::I8_Eq,
        (BinaryOp::Eq,TypeKind::Int(..),2) => Instr::I16_Eq,
        (BinaryOp::Eq,TypeKind::Int(..),4) => Instr::I32_Eq,
        (BinaryOp::Eq,TypeKind::Int(..),8) => Instr::I64_Eq,
        (BinaryOp::Eq,TypeKind::Int(..),16) => Instr::I128_Eq,
        (BinaryOp::NotEq,TypeKind::Int(..),1) => Instr::I8_NotEq,
        (BinaryOp::NotEq,TypeKind::Int(..),2) => Instr::I16_NotEq,
        (BinaryOp::NotEq,TypeKind::Int(..),4) => Instr::I32_NotEq,
        (BinaryOp::NotEq,TypeKind::Int(..),8) => Instr::I64_NotEq,
        (BinaryOp::NotEq,TypeKind::Int(..),16) => Instr::I128_NotEq,

        (BinaryOp::Add,TypeKind::Int(..),1) => Instr::I8_Add,
        (BinaryOp::Add,TypeKind::Int(..),2) => Instr::I16_Add,
        (BinaryOp::Add,TypeKind::Int(..),4) => Instr::I32_Add,
        (BinaryOp::Add,TypeKind::Int(..),8) => Instr::I64_Add,
        (BinaryOp::Add,TypeKind::Int(..),16) => Instr::I128_Add,
        (BinaryOp::Sub,TypeKind::Int(..),1) => Instr::I8_Sub,
        (BinaryOp::Sub,TypeKind::Int(..),2) => Instr::I16_Sub,
        (BinaryOp::Sub,TypeKind::Int(..),4) => Instr::I32_Sub,
        (BinaryOp::Sub,TypeKind::Int(..),8) => Instr::I64_Sub,
        (BinaryOp::Sub,TypeKind::Int(..),16) => Instr::I128_Sub,
        (BinaryOp::Mul,TypeKind::Int(..),1) => Instr::I8_Mul,
        (BinaryOp::Mul,TypeKind::Int(..),2) => Instr::I16_Mul,
        (BinaryOp::Mul,TypeKind::Int(..),4) => Instr::I32_Mul,
        (BinaryOp::Mul,TypeKind::Int(..),8) => Instr::I64_Mul,
        (BinaryOp::Mul,TypeKind::Int(..),16) => Instr::I128_Mul,

        (BinaryOp::BitOr,TypeKind::Int(..),1) => Instr::I8_Or,
        (BinaryOp::BitOr,TypeKind::Int(..),2) => Instr::I16_Or,
        (BinaryOp::BitOr,TypeKind::Int(..),4) => Instr::I32_Or,
        (BinaryOp::BitOr,TypeKind::Int(..),8) => Instr::I64_Or,
        (BinaryOp::BitOr,TypeKind::Int(..),16) => Instr::I128_Or,
        (BinaryOp::BitAnd,TypeKind::Int(..),1) => Instr::I8_And,
        (BinaryOp::BitAnd,TypeKind::Int(..),2) => Instr::I16_And,
        (BinaryOp::BitAnd,TypeKind::Int(..),4) => Instr::I32_And,
        (BinaryOp::BitAnd,TypeKind::Int(..),8) => Instr::I64_And,
        (BinaryOp::BitAnd,TypeKind::Int(..),16) => Instr::I128_And,
        (BinaryOp::BitXor,TypeKind::Int(..),1) => Instr::I8_Xor,
        (BinaryOp::BitXor,TypeKind::Int(..),2) => Instr::I16_Xor,
        (BinaryOp::BitXor,TypeKind::Int(..),4) => Instr::I32_Xor,
        (BinaryOp::BitXor,TypeKind::Int(..),8) => Instr::I64_Xor,
        (BinaryOp::BitXor,TypeKind::Int(..),16) => Instr::I128_Xor,
        (BinaryOp::ShiftL,TypeKind::Int(..),1) => Instr::I8_ShiftL,
        (BinaryOp::ShiftL,TypeKind::Int(..),2) => Instr::I16_ShiftL,
        (BinaryOp::ShiftL,TypeKind::Int(..),4) => Instr::I32_ShiftL,
        (BinaryOp::ShiftL,TypeKind::Int(..),8) => Instr::I64_ShiftL,
        (BinaryOp::ShiftL,TypeKind::Int(..),16) => Instr::I128_ShiftL,

        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Signed),1) => Instr::I8_S_Lt,
        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Signed),2) => Instr::I16_S_Lt,
        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Signed),4) => Instr::I32_S_Lt,
        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Signed),8) => Instr::I64_S_Lt,
        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Signed),16) => Instr::I128_S_Lt,
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Signed),1) => { swap = true; Instr::I8_S_Lt }
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Signed),2) => { swap = true; Instr::I16_S_Lt }
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Signed),4) => { swap = true; Instr::I32_S_Lt }
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Signed),8) => { swap = true; Instr::I64_S_Lt }
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Signed),16) => { swap = true; Instr::I128_S_Lt }
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Signed),1) => Instr::I8_S_LtEq,
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Signed),2) => Instr::I16_S_LtEq,
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Signed),4) => Instr::I32_S_LtEq,
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Signed),8) => Instr::I64_S_LtEq,
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Signed),16) => Instr::I128_S_LtEq,
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Signed),1) => { swap = true; Instr::I8_S_LtEq }
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Signed),2) => { swap = true; Instr::I16_S_LtEq }
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Signed),4) => { swap = true; Instr::I32_S_LtEq }
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Signed),8) => { swap = true; Instr::I64_S_LtEq }
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Signed),16) => { swap = true; Instr::I128_S_LtEq }

        (BinaryOp::Div,TypeKind::Int(_,IntSign::Signed),1) => Instr::I8_S_Div,
        (BinaryOp::Div,TypeKind::Int(_,IntSign::Signed),2) => Instr::I16_S_Div,
        (BinaryOp::Div,TypeKind::Int(_,IntSign::Signed),4) => Instr::I32_S_Div,
        (BinaryOp::Div,TypeKind::Int(_,IntSign::Signed),8) => Instr::I64_S_Div,
        (BinaryOp::Div,TypeKind::Int(_,IntSign::Signed),16) => Instr::I128_S_Div,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Signed),1) => Instr::I8_S_Rem,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Signed),2) => Instr::I16_S_Rem,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Signed),4) => Instr::I32_S_Rem,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Signed),8) => Instr::I64_S_Rem,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Signed),16) => Instr::I128_S_Rem,

        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Signed),1) => Instr::I8_S_ShiftR,
        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Signed),2) => Instr::I16_S_ShiftR,
        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Signed),4) => Instr::I32_S_ShiftR,
        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Signed),8) => Instr::I64_S_ShiftR,
        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Signed),16) => Instr::I128_S_ShiftR,

        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Unsigned),1) => Instr::I8_U_Lt,
        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Unsigned),2) => Instr::I16_U_Lt,
        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Unsigned),4) => Instr::I32_U_Lt,
        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Unsigned),8) => Instr::I64_U_Lt,
        (BinaryOp::Lt,TypeKind::Int(_,IntSign::Unsigned),16) => Instr::I128_U_Lt,
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Unsigned),1) => { swap = true; Instr::I8_U_Lt }
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Unsigned),2) => { swap = true; Instr::I16_U_Lt }
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Unsigned),4) => { swap = true; Instr::I32_U_Lt }
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Unsigned),8) => { swap = true; Instr::I64_U_Lt }
        (BinaryOp::Gt,TypeKind::Int(_,IntSign::Unsigned),16) => { swap = true; Instr::I128_U_Lt }
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Unsigned),1) => Instr::I8_U_LtEq,
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Unsigned),2) => Instr::I16_U_LtEq,
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Unsigned),4) => Instr::I32_U_LtEq,
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Unsigned),8) => Instr::I64_U_LtEq,
        (BinaryOp::LtEq,TypeKind::Int(_,IntSign::Unsigned),16) => Instr::I128_U_LtEq,
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Unsigned),1) => { swap = true; Instr::I8_U_LtEq }
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Unsigned),2) => { swap = true; Instr::I16_U_LtEq }
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Unsigned),4) => { swap = true; Instr::I32_U_LtEq }
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Unsigned),8) => { swap = true; Instr::I64_U_LtEq }
        (BinaryOp::GtEq,TypeKind::Int(_,IntSign::Unsigned),16) => { swap = true; Instr::I128_U_LtEq }

        (BinaryOp::Div,TypeKind::Int(_,IntSign::Unsigned),1) => Instr::I8_U_Div,
        (BinaryOp::Div,TypeKind::Int(_,IntSign::Unsigned),2) => Instr::I16_U_Div,
        (BinaryOp::Div,TypeKind::Int(_,IntSign::Unsigned),4) => Instr::I32_U_Div,
        (BinaryOp::Div,TypeKind::Int(_,IntSign::Unsigned),8) => Instr::I64_U_Div,
        (BinaryOp::Div,TypeKind::Int(_,IntSign::Unsigned),16) => Instr::I128_U_Div,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Unsigned),1) => Instr::I8_U_Rem,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Unsigned),2) => Instr::I16_U_Rem,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Unsigned),4) => Instr::I32_U_Rem,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Unsigned),8) => Instr::I64_U_Rem,
        (BinaryOp::Rem,TypeKind::Int(_,IntSign::Unsigned),16) => Instr::I128_U_Rem,

        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Unsigned),1) => Instr::I8_U_ShiftR,
        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Unsigned),2) => Instr::I16_U_ShiftR,
        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Unsigned),4) => Instr::I32_U_ShiftR,
        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Unsigned),8) => Instr::I64_U_ShiftR,
        (BinaryOp::ShiftR,TypeKind::Int(_,IntSign::Unsigned),16) => Instr::I128_U_ShiftR,

        (BinaryOp::Eq,TypeKind::Float(_),4) => Instr::F32_Eq,
        (BinaryOp::Eq,TypeKind::Float(_),8) => Instr::F64_Eq,
        (BinaryOp::NotEq,TypeKind::Float(_),4) => Instr::F32_NotEq,
        (BinaryOp::NotEq,TypeKind::Float(_),8) => Instr::F64_NotEq,
        (BinaryOp::Lt,TypeKind::Float(_),4) => Instr::F32_Lt,
        (BinaryOp::Lt,TypeKind::Float(_),8) => Instr::F64_Lt,
        (BinaryOp::Gt,TypeKind::Float(_),4) => Instr::F32_Gt,
        (BinaryOp::Gt,TypeKind::Float(_),8) => Instr::F64_Gt,

        (BinaryOp::LtEq,TypeKind::Float(_),4) => Instr::F32_LtEq,
        (BinaryOp::LtEq,TypeKind::Float(_),8) => Instr::F64_LtEq,
        (BinaryOp::GtEq,TypeKind::Float(_),4) => Instr::F32_GtEq,
        (BinaryOp::GtEq,TypeKind::Float(_),8) => Instr::F64_GtEq,

        (BinaryOp::Add,TypeKind::Float(_),4) => Instr::F32_Add,
        (BinaryOp::Add,TypeKind::Float(_),8) => Instr::F64_Add,
        (BinaryOp::Sub,TypeKind::Float(_),4) => Instr::F32_Sub,
        (BinaryOp::Sub,TypeKind::Float(_),8) => Instr::F64_Sub,
        (BinaryOp::Mul,TypeKind::Float(_),4) => Instr::F32_Mul,
        (BinaryOp::Mul,TypeKind::Float(_),8) => Instr::F64_Mul,
        (BinaryOp::Div,TypeKind::Float(_),4) => Instr::F32_Div,
        (BinaryOp::Div,TypeKind::Float(_),8) => Instr::F64_Div,
        (BinaryOp::Rem,TypeKind::Float(_),4) => Instr::F32_Rem,
        (BinaryOp::Rem,TypeKind::Float(_),8) => Instr::F64_Rem,

        (BinaryOp::Eq,TypeKind::Bool,1) => Instr::I8_Eq,
        (BinaryOp::NotEq,TypeKind::Bool,1) => Instr::I8_NotEq,

        (BinaryOp::BitOr,TypeKind::Bool,1) => Instr::I8_Or,
        (BinaryOp::BitAnd,TypeKind::Bool,1) => Instr::I8_And,
        (BinaryOp::BitXor,TypeKind::Bool,1) => Instr::I8_Xor,

        _ => panic!("no binary op: {:?} {:?}",op,ty)
    };

    (ctor,swap)
}

pub fn cast(arg_layout: Type, res_layout: Type) -> fn(Slot,Slot) -> Instr {
    panic!("todo fix cast");
    /*match (&arg_layout.kind,&res_layout.kind) {
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
    }*/
}
