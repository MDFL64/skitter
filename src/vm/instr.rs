use super::vm::Function;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Slot(u32);

impl Slot {
    pub const DUMMY: Self = Self(0);

    const SUB_OFFSET: u32 = 1_000_000_000;

    pub fn new(id: u32) -> Self {
        assert!(id < Self::SUB_OFFSET);
        Self(id)
    }

    pub fn new_frame_sub(id: u32) -> Self {
        Self(id + Self::SUB_OFFSET)
    }

    pub fn get_frame_sub(&self) -> Option<u32> {
        if self.0 >= Self::SUB_OFFSET {
            Some(self.0 - Self::SUB_OFFSET)
        } else {
            None
        }
    }

    pub fn index(&self) -> usize {
        self.0 as usize
    }

    pub fn offset_by(&self, offset: i32) -> Self {
        Self((self.0 as i32 + offset) as u32)
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
#[repr(u16)]
pub enum Instr<'vm> {
    I8_Const(Slot, i8),
    I8_Neg(Slot, Slot),
    I8_Not(Slot, Slot),
    I8_Eq(Slot, Slot, Slot),
    I8_NotEq(Slot, Slot, Slot),
    I8_Add(Slot, Slot, Slot),
    I8_Sub(Slot, Slot, Slot),
    I8_Mul(Slot, Slot, Slot),
    I8_Or(Slot, Slot, Slot),
    I8_And(Slot, Slot, Slot),
    I8_Xor(Slot, Slot, Slot),
    I8_ShiftL(Slot, Slot, Slot),
    I8_S_Lt(Slot, Slot, Slot),
    I8_S_LtEq(Slot, Slot, Slot),
    I8_S_Div(Slot, Slot, Slot),
    I8_S_Rem(Slot, Slot, Slot),
    I8_S_ShiftR(Slot, Slot, Slot),
    I8_U_Lt(Slot, Slot, Slot),
    I8_U_LtEq(Slot, Slot, Slot),
    I8_U_Div(Slot, Slot, Slot),
    I8_U_Rem(Slot, Slot, Slot),
    I8_U_ShiftR(Slot, Slot, Slot),

    I16_Const(Slot, i16),
    I16_Neg(Slot, Slot),
    I16_Not(Slot, Slot),
    I16_Eq(Slot, Slot, Slot),
    I16_NotEq(Slot, Slot, Slot),
    I16_Add(Slot, Slot, Slot),
    I16_Sub(Slot, Slot, Slot),
    I16_Mul(Slot, Slot, Slot),
    I16_Or(Slot, Slot, Slot),
    I16_And(Slot, Slot, Slot),
    I16_Xor(Slot, Slot, Slot),
    I16_ShiftL(Slot, Slot, Slot),
    I16_S_Lt(Slot, Slot, Slot),
    I16_S_LtEq(Slot, Slot, Slot),
    I16_S_Div(Slot, Slot, Slot),
    I16_S_Rem(Slot, Slot, Slot),
    I16_S_ShiftR(Slot, Slot, Slot),
    I16_U_Lt(Slot, Slot, Slot),
    I16_U_LtEq(Slot, Slot, Slot),
    I16_U_Div(Slot, Slot, Slot),
    I16_U_Rem(Slot, Slot, Slot),
    I16_U_ShiftR(Slot, Slot, Slot),

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

    I64_Const(Slot, i64),
    I64_Neg(Slot, Slot),
    I64_Not(Slot, Slot),
    I64_Eq(Slot, Slot, Slot),
    I64_NotEq(Slot, Slot, Slot),
    I64_Add(Slot, Slot, Slot),
    I64_Sub(Slot, Slot, Slot),
    I64_Mul(Slot, Slot, Slot),
    I64_Or(Slot, Slot, Slot),
    I64_And(Slot, Slot, Slot),
    I64_Xor(Slot, Slot, Slot),
    I64_ShiftL(Slot, Slot, Slot),
    I64_S_Lt(Slot, Slot, Slot),
    I64_S_LtEq(Slot, Slot, Slot),
    I64_S_Div(Slot, Slot, Slot),
    I64_S_Rem(Slot, Slot, Slot),
    I64_S_ShiftR(Slot, Slot, Slot),
    I64_U_Lt(Slot, Slot, Slot),
    I64_U_LtEq(Slot, Slot, Slot),
    I64_U_Div(Slot, Slot, Slot),
    I64_U_Rem(Slot, Slot, Slot),
    I64_U_ShiftR(Slot, Slot, Slot),

    I128_Const(Slot, Box<i128>),
    I128_Neg(Slot, Slot),
    I128_Not(Slot, Slot),
    I128_Eq(Slot, Slot, Slot),
    I128_NotEq(Slot, Slot, Slot),
    I128_Add(Slot, Slot, Slot),
    I128_Sub(Slot, Slot, Slot),
    I128_Mul(Slot, Slot, Slot),
    I128_Or(Slot, Slot, Slot),
    I128_And(Slot, Slot, Slot),
    I128_Xor(Slot, Slot, Slot),
    I128_ShiftL(Slot, Slot, Slot),
    I128_S_Lt(Slot, Slot, Slot),
    I128_S_LtEq(Slot, Slot, Slot),
    I128_S_Div(Slot, Slot, Slot),
    I128_S_Rem(Slot, Slot, Slot),
    I128_S_ShiftR(Slot, Slot, Slot),
    I128_U_Lt(Slot, Slot, Slot),
    I128_U_LtEq(Slot, Slot, Slot),
    I128_U_Div(Slot, Slot, Slot),
    I128_U_Rem(Slot, Slot, Slot),
    I128_U_ShiftR(Slot, Slot, Slot),

    Bool_Not(Slot, Slot),

    F32_Neg(Slot, Slot),
    F32_Eq(Slot, Slot, Slot),
    F32_NotEq(Slot, Slot, Slot),
    F32_Add(Slot, Slot, Slot),
    F32_Sub(Slot, Slot, Slot),
    F32_Mul(Slot, Slot, Slot),
    F32_Div(Slot, Slot, Slot),
    F32_Rem(Slot, Slot, Slot),
    F32_Lt(Slot, Slot, Slot),
    F32_LtEq(Slot, Slot, Slot),
    F32_Gt(Slot, Slot, Slot),
    F32_GtEq(Slot, Slot, Slot),

    F64_Neg(Slot, Slot),
    F64_Eq(Slot, Slot, Slot),
    F64_NotEq(Slot, Slot, Slot),
    F64_Add(Slot, Slot, Slot),
    F64_Sub(Slot, Slot, Slot),
    F64_Mul(Slot, Slot, Slot),
    F64_Div(Slot, Slot, Slot),
    F64_Rem(Slot, Slot, Slot),
    F64_Lt(Slot, Slot, Slot),
    F64_LtEq(Slot, Slot, Slot),
    F64_Gt(Slot, Slot, Slot),
    F64_GtEq(Slot, Slot, Slot),

    // Integer widening ops used for casts
    // Narrowing needs no special instructions
    I16_S_Widen_8(Slot, Slot),
    I16_U_Widen_8(Slot, Slot),

    I32_S_Widen_16(Slot, Slot),
    I32_U_Widen_16(Slot, Slot),
    I32_S_Widen_8(Slot, Slot),
    I32_U_Widen_8(Slot, Slot),

    I64_S_Widen_32(Slot, Slot),
    I64_U_Widen_32(Slot, Slot),
    I64_S_Widen_16(Slot, Slot),
    I64_U_Widen_16(Slot, Slot),
    I64_S_Widen_8(Slot, Slot),
    I64_U_Widen_8(Slot, Slot),

    I128_S_Widen_64(Slot, Slot),
    I128_U_Widen_64(Slot, Slot),
    I128_S_Widen_32(Slot, Slot),
    I128_U_Widen_32(Slot, Slot),
    I128_S_Widen_16(Slot, Slot),
    I128_U_Widen_16(Slot, Slot),
    I128_S_Widen_8(Slot, Slot),
    I128_U_Widen_8(Slot, Slot),

    // Float casts
    F32_From_F64(Slot, Slot),
    F32_From_I8_S(Slot, Slot),
    F32_From_I16_S(Slot, Slot),
    F32_From_I32_S(Slot, Slot),
    F32_From_I64_S(Slot, Slot),
    F32_From_I128_S(Slot, Slot),
    F32_From_I8_U(Slot, Slot),
    F32_From_I16_U(Slot, Slot),
    F32_From_I32_U(Slot, Slot),
    F32_From_I64_U(Slot, Slot),
    F32_From_I128_U(Slot, Slot),
    F32_Into_I8_S(Slot, Slot),
    F32_Into_I16_S(Slot, Slot),
    F32_Into_I32_S(Slot, Slot),
    F32_Into_I64_S(Slot, Slot),
    F32_Into_I128_S(Slot, Slot),
    F32_Into_I8_U(Slot, Slot),
    F32_Into_I16_U(Slot, Slot),
    F32_Into_I32_U(Slot, Slot),
    F32_Into_I64_U(Slot, Slot),
    F32_Into_I128_U(Slot, Slot),

    F64_From_F32(Slot, Slot),
    F64_From_I8_S(Slot, Slot),
    F64_From_I16_S(Slot, Slot),
    F64_From_I32_S(Slot, Slot),
    F64_From_I64_S(Slot, Slot),
    F64_From_I128_S(Slot, Slot),
    F64_From_I8_U(Slot, Slot),
    F64_From_I16_U(Slot, Slot),
    F64_From_I32_U(Slot, Slot),
    F64_From_I64_U(Slot, Slot),
    F64_From_I128_U(Slot, Slot),
    F64_Into_I8_S(Slot, Slot),
    F64_Into_I16_S(Slot, Slot),
    F64_Into_I32_S(Slot, Slot),
    F64_Into_I64_S(Slot, Slot),
    F64_Into_I128_S(Slot, Slot),
    F64_Into_I8_U(Slot, Slot),
    F64_Into_I16_U(Slot, Slot),
    F64_Into_I32_U(Slot, Slot),
    F64_Into_I64_U(Slot, Slot),
    F64_Into_I128_U(Slot, Slot),

    MovSS1(Slot, Slot),
    MovSS2(Slot, Slot),
    MovSS4(Slot, Slot),
    MovSS8(Slot, Slot),
    MovSS16(Slot, Slot),
    MovSS1N(Slot, Slot, u32),
    MovSS2N(Slot, Slot, u32),
    MovSS4N(Slot, Slot, u32),
    MovSS8N(Slot, Slot, u32),
    MovSS16N(Slot, Slot, u32),

    // NOTE: we could actually make ONE of these u16's wider, if we change the layout to: (u16, Slot, Slot, u32), or just remove the c abi restriction
    MovSP1(Slot, Slot, i32),
    MovSP2(Slot, Slot, i32),
    MovSP4(Slot, Slot, i32),
    MovSP8(Slot, Slot, i32),
    MovSP16(Slot, Slot, i32),

    MovPS1(Slot, Slot, i32),
    MovPS2(Slot, Slot, i32),
    MovPS4(Slot, Slot, i32),
    MovPS8(Slot, Slot, i32),
    MovPS16(Slot, Slot, i32),

    MovSP1N(Slot, Slot, i16, u16),
    MovSP2N(Slot, Slot, i16, u16),
    MovSP4N(Slot, Slot, i16, u16),
    MovSP8N(Slot, Slot, i16, u16),
    MovSP16N(Slot, Slot, i16, u16),

    MovPS1N(Slot, Slot, i16, u16),
    MovPS2N(Slot, Slot, i16, u16),
    MovPS4N(Slot, Slot, i16, u16),
    MovPS8N(Slot, Slot, i16, u16),
    MovPS16N(Slot, Slot, i16, u16),

    /// src, dst, count (bytes) - copies NON-OVERLAPPING memory chunks
    MemCopy(Slot, Slot, Slot),
    MemCompare(Slot, Slot, Slot),

    ArrayRepeat {
        base: Slot,
        size: u32,
        count: u32,
    },

    SlotAddr(Slot, Slot),

    PointerOffset3(Slot, Slot, i32),
    PointerOffset2(Slot, Slot, i32),
    // name the fields since these are less straightforward
    SlotAddrOffset {
        out: Slot,
        arg: Slot,
        offset: Slot,
    },
    IndexCalc {
        arg_out: Slot,
        elem_size: u32,
        elem_count: u32,
    },
    IndexCalcDyn {
        arg_out: Slot,
        elem_size: u32,
        elem_count: Slot,
    },
    IndexCalcEndPointer {
        out: Slot,
        slice: Slot,
        elem_size: u32,
    },

    //OffsetPtr(Slot, Slot, i32),
    Jump(i32),
    JumpF(i32, Slot),
    JumpT(i32, Slot),

    Call(Slot, &'vm Function<'vm>),
    CallPtr {
        frame: Slot,
        func_ptr: Slot,
    },

    VTableFunc(Slot, Slot, u32),

    Return,
    Bad,
    Debug(Box<String>),

    // used for intrinsic Box::new
    Alloc{out: Slot, size: u32, align: u32},

    // RUST INTRINSICS
    WriteBytes {
        size: u16,
        dst: Slot,
        val: Slot,
        count: Slot,
    },

    I8_PopCount(Slot, Slot),
    I16_PopCount(Slot, Slot),
    I32_PopCount(Slot, Slot),
    I64_PopCount(Slot, Slot),
    I128_PopCount(Slot, Slot),

    I8_TrailingZeros(Slot, Slot),
    I16_TrailingZeros(Slot, Slot),
    I32_TrailingZeros(Slot, Slot),
    I64_TrailingZeros(Slot, Slot),
    I128_TrailingZeros(Slot, Slot),

    I8_ReverseBits(Slot, Slot),
    I16_ReverseBits(Slot, Slot),
    I32_ReverseBits(Slot, Slot),
    I64_ReverseBits(Slot, Slot),
    I128_ReverseBits(Slot, Slot),

    I8_RotateLeft(Slot, Slot, Slot),
    I16_RotateLeft(Slot, Slot, Slot),
    I32_RotateLeft(Slot, Slot, Slot),
    I64_RotateLeft(Slot, Slot, Slot),
    I128_RotateLeft(Slot, Slot, Slot),

    I8_RotateRight(Slot, Slot, Slot),
    I16_RotateRight(Slot, Slot, Slot),
    I32_RotateRight(Slot, Slot, Slot),
    I64_RotateRight(Slot, Slot, Slot),
    I128_RotateRight(Slot, Slot, Slot),

    F32_Min(Slot, Slot, Slot),
    F64_Min(Slot, Slot, Slot),

    F32_Max(Slot, Slot, Slot),
    F64_Max(Slot, Slot, Slot),
}

/* This used to be used to setup call frames, but it was buggy. Probably best to stop using it.

impl<'vm> Instr<'vm> {
    pub fn replace_arg_sub(&mut self, call_slot: Slot) {
        if let Some(res) = self.get_result() {
            if let Some(offset) = res.get_frame_sub() {
                *res = call_slot.offset_by(offset);
            }
        };
    }

    fn get_result(&mut self) -> Option<&mut Slot> {
        match self {
            Instr::I8_Const(x, _)
            | Instr::I8_Neg(x, _)
            | Instr::I8_Not(x, _)
            | Instr::I8_Eq(x, _, _)
            | Instr::I8_NotEq(x, _, _)
            | Instr::I8_Add(x, _, _)
            | Instr::I8_Sub(x, _, _)
            | Instr::I8_Mul(x, _, _)
            | Instr::I8_Or(x, _, _)
            | Instr::I8_And(x, _, _)
            | Instr::I8_Xor(x, _, _)
            | Instr::I8_ShiftL(x, _, _)
            | Instr::I8_S_Lt(x, _, _)
            | Instr::I8_S_LtEq(x, _, _)
            | Instr::I8_S_Div(x, _, _)
            | Instr::I8_S_Rem(x, _, _)
            | Instr::I8_S_ShiftR(x, _, _)
            | Instr::I8_U_Lt(x, _, _)
            | Instr::I8_U_LtEq(x, _, _)
            | Instr::I8_U_Div(x, _, _)
            | Instr::I8_U_Rem(x, _, _)
            | Instr::I8_U_ShiftR(x, _, _) => Some(x),

            Instr::I16_Const(x, _)
            | Instr::I16_Neg(x, _)
            | Instr::I16_Not(x, _)
            | Instr::I16_Eq(x, _, _)
            | Instr::I16_NotEq(x, _, _)
            | Instr::I16_Add(x, _, _)
            | Instr::I16_Sub(x, _, _)
            | Instr::I16_Mul(x, _, _)
            | Instr::I16_Or(x, _, _)
            | Instr::I16_And(x, _, _)
            | Instr::I16_Xor(x, _, _)
            | Instr::I16_ShiftL(x, _, _)
            | Instr::I16_S_Lt(x, _, _)
            | Instr::I16_S_LtEq(x, _, _)
            | Instr::I16_S_Div(x, _, _)
            | Instr::I16_S_Rem(x, _, _)
            | Instr::I16_S_ShiftR(x, _, _)
            | Instr::I16_U_Lt(x, _, _)
            | Instr::I16_U_LtEq(x, _, _)
            | Instr::I16_U_Div(x, _, _)
            | Instr::I16_U_Rem(x, _, _)
            | Instr::I16_U_ShiftR(x, _, _) => Some(x),

            Instr::I32_Const(x, _)
            | Instr::I32_Neg(x, _)
            | Instr::I32_Not(x, _)
            | Instr::I32_Eq(x, _, _)
            | Instr::I32_NotEq(x, _, _)
            | Instr::I32_Add(x, _, _)
            | Instr::I32_Sub(x, _, _)
            | Instr::I32_Mul(x, _, _)
            | Instr::I32_Or(x, _, _)
            | Instr::I32_And(x, _, _)
            | Instr::I32_Xor(x, _, _)
            | Instr::I32_ShiftL(x, _, _)
            | Instr::I32_S_Lt(x, _, _)
            | Instr::I32_S_LtEq(x, _, _)
            | Instr::I32_S_Div(x, _, _)
            | Instr::I32_S_Rem(x, _, _)
            | Instr::I32_S_ShiftR(x, _, _)
            | Instr::I32_U_Lt(x, _, _)
            | Instr::I32_U_LtEq(x, _, _)
            | Instr::I32_U_Div(x, _, _)
            | Instr::I32_U_Rem(x, _, _)
            | Instr::I32_U_ShiftR(x, _, _) => Some(x),

            Instr::I64_Const(x, _)
            | Instr::I64_Neg(x, _)
            | Instr::I64_Not(x, _)
            | Instr::I64_Eq(x, _, _)
            | Instr::I64_NotEq(x, _, _)
            | Instr::I64_Add(x, _, _)
            | Instr::I64_Sub(x, _, _)
            | Instr::I64_Mul(x, _, _)
            | Instr::I64_Or(x, _, _)
            | Instr::I64_And(x, _, _)
            | Instr::I64_Xor(x, _, _)
            | Instr::I64_ShiftL(x, _, _)
            | Instr::I64_S_Lt(x, _, _)
            | Instr::I64_S_LtEq(x, _, _)
            | Instr::I64_S_Div(x, _, _)
            | Instr::I64_S_Rem(x, _, _)
            | Instr::I64_S_ShiftR(x, _, _)
            | Instr::I64_U_Lt(x, _, _)
            | Instr::I64_U_LtEq(x, _, _)
            | Instr::I64_U_Div(x, _, _)
            | Instr::I64_U_Rem(x, _, _)
            | Instr::I64_U_ShiftR(x, _, _) => Some(x),

            Instr::I128_Const(x, _)
            | Instr::I128_Neg(x, _)
            | Instr::I128_Not(x, _)
            | Instr::I128_Eq(x, _, _)
            | Instr::I128_NotEq(x, _, _)
            | Instr::I128_Add(x, _, _)
            | Instr::I128_Sub(x, _, _)
            | Instr::I128_Mul(x, _, _)
            | Instr::I128_Or(x, _, _)
            | Instr::I128_And(x, _, _)
            | Instr::I128_Xor(x, _, _)
            | Instr::I128_ShiftL(x, _, _)
            | Instr::I128_S_Lt(x, _, _)
            | Instr::I128_S_LtEq(x, _, _)
            | Instr::I128_S_Div(x, _, _)
            | Instr::I128_S_Rem(x, _, _)
            | Instr::I128_S_ShiftR(x, _, _)
            | Instr::I128_U_Lt(x, _, _)
            | Instr::I128_U_LtEq(x, _, _)
            | Instr::I128_U_Div(x, _, _)
            | Instr::I128_U_Rem(x, _, _)
            | Instr::I128_U_ShiftR(x, _, _) => Some(x),

            Instr::Bool_Not(x, _) => Some(x),

            Instr::F32_Neg(x, _)
            | Instr::F32_Eq(x, _, _)
            | Instr::F32_NotEq(x, _, _)
            | Instr::F32_Add(x, _, _)
            | Instr::F32_Sub(x, _, _)
            | Instr::F32_Mul(x, _, _)
            | Instr::F32_Div(x, _, _)
            | Instr::F32_Rem(x, _, _)
            | Instr::F32_Lt(x, _, _)
            | Instr::F32_LtEq(x, _, _)
            | Instr::F32_Gt(x, _, _)
            | Instr::F32_GtEq(x, _, _) => Some(x),

            Instr::F64_Neg(x, _)
            | Instr::F64_Eq(x, _, _)
            | Instr::F64_NotEq(x, _, _)
            | Instr::F64_Add(x, _, _)
            | Instr::F64_Sub(x, _, _)
            | Instr::F64_Mul(x, _, _)
            | Instr::F64_Div(x, _, _)
            | Instr::F64_Rem(x, _, _)
            | Instr::F64_Lt(x, _, _)
            | Instr::F64_LtEq(x, _, _)
            | Instr::F64_Gt(x, _, _)
            | Instr::F64_GtEq(x, _, _) => Some(x),

            Instr::I16_S_Widen_8(x, _)
            | Instr::I16_U_Widen_8(x, _)
            | Instr::I32_S_Widen_16(x, _)
            | Instr::I32_U_Widen_16(x, _)
            | Instr::I32_S_Widen_8(x, _)
            | Instr::I32_U_Widen_8(x, _)
            | Instr::I64_S_Widen_32(x, _)
            | Instr::I64_U_Widen_32(x, _)
            | Instr::I64_S_Widen_16(x, _)
            | Instr::I64_U_Widen_16(x, _)
            | Instr::I64_S_Widen_8(x, _)
            | Instr::I64_U_Widen_8(x, _)
            | Instr::I128_S_Widen_64(x, _)
            | Instr::I128_U_Widen_64(x, _)
            | Instr::I128_S_Widen_32(x, _)
            | Instr::I128_U_Widen_32(x, _)
            | Instr::I128_S_Widen_16(x, _)
            | Instr::I128_U_Widen_16(x, _)
            | Instr::I128_S_Widen_8(x, _)
            | Instr::I128_U_Widen_8(x, _)
            | Instr::F32_From_F64(x, _)
            | Instr::F32_From_I8_S(x, _)
            | Instr::F32_From_I16_S(x, _)
            | Instr::F32_From_I32_S(x, _)
            | Instr::F32_From_I64_S(x, _)
            | Instr::F32_From_I128_S(x, _)
            | Instr::F32_From_I8_U(x, _)
            | Instr::F32_From_I16_U(x, _)
            | Instr::F32_From_I32_U(x, _)
            | Instr::F32_From_I64_U(x, _)
            | Instr::F32_From_I128_U(x, _)
            | Instr::F32_Into_I8_S(x, _)
            | Instr::F32_Into_I16_S(x, _)
            | Instr::F32_Into_I32_S(x, _)
            | Instr::F32_Into_I64_S(x, _)
            | Instr::F32_Into_I128_S(x, _)
            | Instr::F32_Into_I8_U(x, _)
            | Instr::F32_Into_I16_U(x, _)
            | Instr::F32_Into_I32_U(x, _)
            | Instr::F32_Into_I64_U(x, _)
            | Instr::F32_Into_I128_U(x, _)
            | Instr::F64_From_F32(x, _)
            | Instr::F64_From_I8_S(x, _)
            | Instr::F64_From_I16_S(x, _)
            | Instr::F64_From_I32_S(x, _)
            | Instr::F64_From_I64_S(x, _)
            | Instr::F64_From_I128_S(x, _)
            | Instr::F64_From_I8_U(x, _)
            | Instr::F64_From_I16_U(x, _)
            | Instr::F64_From_I32_U(x, _)
            | Instr::F64_From_I64_U(x, _)
            | Instr::F64_From_I128_U(x, _)
            | Instr::F64_Into_I8_S(x, _)
            | Instr::F64_Into_I16_S(x, _)
            | Instr::F64_Into_I32_S(x, _)
            | Instr::F64_Into_I64_S(x, _)
            | Instr::F64_Into_I128_S(x, _)
            | Instr::F64_Into_I8_U(x, _)
            | Instr::F64_Into_I16_U(x, _)
            | Instr::F64_Into_I32_U(x, _)
            | Instr::F64_Into_I64_U(x, _)
            | Instr::F64_Into_I128_U(x, _) => Some(x),

            Instr::MovSS1(x, _)
            | Instr::MovSS2(x, _)
            | Instr::MovSS4(x, _)
            | Instr::MovSS8(x, _)
            | Instr::MovSS16(x, _)
            | Instr::MovSS1N(x, _, _)
            | Instr::MovSS2N(x, _, _)
            | Instr::MovSS4N(x, _, _)
            | Instr::MovSS8N(x, _, _)
            | Instr::MovSS16N(x, _, _)
            | Instr::MovSP1(x, _, _)
            | Instr::MovSP2(x, _, _)
            | Instr::MovSP4(x, _, _)
            | Instr::MovSP8(x, _, _)
            | Instr::MovSP16(x, _, _)
            | Instr::MovSP1N(x, _, _, _)
            | Instr::MovSP2N(x, _, _, _)
            | Instr::MovSP4N(x, _, _, _)
            | Instr::MovSP8N(x, _, _, _)
            | Instr::MovSP16N(x, _, _, _) => Some(x),

            Instr::MovPS1(x, _, _)
            | Instr::MovPS2(x, _, _)
            | Instr::MovPS4(x, _, _)
            | Instr::MovPS8(x, _, _)
            | Instr::MovPS16(x, _, _)
            | Instr::MovPS1N(x, _, _, _)
            | Instr::MovPS2N(x, _, _, _)
            | Instr::MovPS4N(x, _, _, _)
            | Instr::MovPS8N(x, _, _, _)
            | Instr::MovPS16N(x, _, _, _) => Some(x),

            Instr::ArrayRepeat { base, size, count } => Some(base),

            Instr::SlotAddr(x, _) => Some(x),
            Instr::SlotAddrOffset { out, .. } => Some(out),
            Instr::PointerOffset3(x, _, _) => Some(x),
            Instr::PointerOffset2(x, _, _) => Some(x),

            Instr::IndexCalc { .. } | Instr::IndexCalcDyn { .. } => None,

            Instr::Jump(_)
            | Instr::JumpF(_, _)
            | Instr::JumpT(_, _)
            | Instr::Return
            | Instr::Bad
            | Instr::Debug(_)
            | Instr::Call(_, _)
            | Instr::CallPtr { .. } => None,

            Instr::I8_PopCount(x, _)
            | Instr::I16_PopCount(x, _)
            | Instr::I32_PopCount(x, _)
            | Instr::I64_PopCount(x, _)
            | Instr::I128_PopCount(x, _) => Some(x),

            Instr::I8_TrailingZeros(x, _)
            | Instr::I16_TrailingZeros(x, _)
            | Instr::I32_TrailingZeros(x, _)
            | Instr::I64_TrailingZeros(x, _)
            | Instr::I128_TrailingZeros(x, _) => Some(x),

            Instr::WriteBytes { .. } => None,
        }
    }
}
*/
