#[derive(Debug,Clone,Copy)]
pub struct Slot(u32);

impl Slot {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn offset(&self) -> usize {
        self.0 as usize
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
#[repr(u16)]
pub enum Instr {
    Bool_And(Slot, Slot, Slot),

    I32_Const(Slot, i32),
    I32_Neg(Slot, Slot),
    I32_Not(Slot, Slot),
    I32_Eq(Slot, Slot, Slot),
    I32_NotEq(Slot, Slot, Slot),
    I32_Add(Slot, Slot, Slot),
    I32_Sub(Slot, Slot, Slot),
    I32_Mul(Slot, Slot, Slot),
    I32_Or(Slot, Slot, Slot),
    I32_And(Slot, Slot, Slot),
    I32_Xor(Slot, Slot, Slot),
    I32_ShiftL(Slot, Slot, Slot),
    I32_S_Lt(Slot, Slot, Slot),
    I32_S_LtEq(Slot, Slot, Slot),
    I32_S_Div(Slot, Slot, Slot),
    I32_S_Rem(Slot, Slot, Slot),
    I32_S_ShiftR(Slot, Slot, Slot),
    I32_U_Lt(Slot, Slot, Slot),
    I32_U_LtEq(Slot, Slot, Slot),
    I32_U_Div(Slot, Slot, Slot),
    I32_U_Rem(Slot, Slot, Slot),
    I32_U_ShiftR(Slot, Slot, Slot),

    I128_S_Widen_32(Slot, Slot),
    I128_U_Widen_32(Slot, Slot),

    MovSS1(Slot, Slot),
    MovSS2(Slot, Slot),
    MovSS4(Slot, Slot),
    MovSS8(Slot, Slot),
    MovSS16(Slot, Slot),

    Call(Slot, CallTarget),

    Return,
    Bad,
}

#[derive(Debug)]
pub enum CallTarget {
    PrintInt,
    PrintUint,
    PrintBool,
}
