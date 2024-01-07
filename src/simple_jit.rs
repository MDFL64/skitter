use dynasmrt::{dynasm, DynamicLabel, DynasmApi, DynasmLabelApi};

use crate::{
    bytecode_compiler::FunctionBytecode,
    vm::{instr::Instr, Function, NativeFunc, VMThread},
};

static mut OUT: i32 = 0;

// CALLEE-SAVED: r12, r13, r14, r15, rbx, rsp, rbp
pub fn compile<'vm>(bytecode: &'vm FunctionBytecode<'vm>) -> Result<NativeFunc, String> {
    // must be 8 mod 16 to maintain alignment
    const FRAME_SIZE: i32 = 0x28;

    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    dynasm!(ops
        ; sub rsp, FRAME_SIZE
        ; mov [rsp + 8], rdi    // stack pointer
        ; mov [rsp + 16], rsi   // thread
    );

    if bytecode.drops.len() > 0 {
        return Err("drops are unsupported".to_owned());
    }

    let labels: Vec<_> = bytecode
        .code
        .iter()
        .map(|_| ops.new_dynamic_label())
        .collect();

    for (pc, (bc, label)) in bytecode.code.iter().zip(&labels).enumerate() {
        dynasm!(ops
            ; =>*label
        );
        match bc {
            Instr::I8_Const(slot, n) => {
                dynasm!(ops
                    ; mov BYTE [rdi + slot.index() as i32], *n
                );
            }
            Instr::I32_Const(slot, n) => {
                dynasm!(ops
                    ; mov DWORD [rdi + slot.index() as i32], *n
                );
            }
            Instr::I64_Const(slot, n) => {
                dynasm!(ops
                    ; mov rax, QWORD *n
                    ; mov [rdi + slot.index() as i32], rax
                );
            }
            Instr::I32_Neg(dst, src) => {
                dynasm!(ops
                    ; mov eax, [rdi + src.index() as i32]
                    ; neg eax
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_Not(dst, src) => {
                dynasm!(ops
                    ; mov eax, [rdi + src.index() as i32]
                    ; not eax
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_Add(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; add eax, ecx
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_Sub(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; sub eax, ecx
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_Mul(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; mul ecx
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_And(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; and eax, ecx
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_Or(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; or eax, ecx
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_Xor(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; xor eax, ecx
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_S_Div(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; cdq
                    ; idiv ecx
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_S_Rem(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; cdq
                    ; idiv ecx
                    ; mov [rdi + dst.index() as i32], edx
                );
            }
            Instr::I32_ShiftL(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; sal eax, cl
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_S_ShiftR(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; sar eax, cl
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::I32_Eq(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; test eax, ecx
                    ; setz al
                    ; mov [rdi + dst.index() as i32], al
                );
            }
            Instr::I32_NotEq(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; test eax, ecx
                    ; setnz al
                    ; mov [rdi + dst.index() as i32], al
                );
            }
            Instr::I32_S_Lt(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; sub eax, ecx
                    ; setl al
                    ; mov [rdi + dst.index() as i32], al
                );
            }
            Instr::I32_S_LtEq(dst, lhs, rhs) => {
                dynasm!(ops
                    ; mov eax, [rdi + lhs.index() as i32]
                    ; mov ecx, [rdi + rhs.index() as i32]
                    ; sub eax, ecx
                    ; setle al
                    ; mov [rdi + dst.index() as i32], al
                );
            }
            Instr::I128_S_Widen_32(dst, src) => {
                dynasm!(ops
                    ; movsx rax, DWORD [rdi + src.index() as i32]
                    ; mov [rdi + dst.index() as i32], rax
                    ; sar rax, 63
                    ; mov [rdi + dst.index() as i32 + 8], rax
                );
            }
            Instr::MovSS1(dst, src) => {
                dynasm!(ops
                    ; mov al, [rdi + src.index() as i32]
                    ; mov [rdi + dst.index() as i32], al
                );
            }
            Instr::MovSS4(dst, src) => {
                dynasm!(ops
                    ; mov eax, [rdi + src.index() as i32]
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::MovSS8(dst, src) => {
                dynasm!(ops
                    ; mov rax, [rdi + src.index() as i32]
                    ; mov [rdi + dst.index() as i32], rax
                );
            }
            Instr::MovSS16(dst, src) => {
                dynasm!(ops
                    ; mov rax, [rdi + src.index() as i32]
                    ; mov [rdi + dst.index() as i32], rax
                    ; mov rax, [rdi + src.index() as i32 + 8]
                    ; mov [rdi + dst.index() as i32 + 8], rax
                );
            }
            Instr::MovSS1N(dst, src, n) => {
                assert!(*n < 8);
                for i in 0..(*n as i32) {
                    dynasm!(ops
                        ; mov al, [rdi + src.index() as i32 + i]
                        ; mov [rdi + dst.index() as i32 + i], al
                    );
                }
            }
            Instr::MovSS4N(dst, src, n) => {
                assert!(*n < 8);
                for i in 0..(*n as i32) {
                    dynasm!(ops
                        ; mov eax, [rdi + src.index() as i32 + i*4]
                        ; mov [rdi + dst.index() as i32 + i*4], eax
                    );
                }
            }
            Instr::MovSS8N(dst, src, n) => {
                assert!(*n < 8);
                for i in 0..(*n as i32) {
                    dynasm!(ops
                        ; mov rax, [rdi + src.index() as i32 + i*8]
                        ; mov [rdi + dst.index() as i32 + i*8], rax
                    );
                }
            }
            // copy TO pointer
            Instr::MovPS1(dst, src, offset) => {
                dynasm!(ops
                    ; mov al, [rdi + src.index() as i32]
                    ; mov rcx, [rdi + dst.index() as i32]
                    ; mov [rcx + *offset], al
                );
            }
            // copy FROM pointer
            Instr::MovSP1(dst, src, offset) => {
                dynasm!(ops
                    ; mov rcx, [rdi + src.index() as i32]
                    ; mov al, [rcx + *offset]
                    ; mov [rdi + dst.index() as i32], al
                );
            }
            Instr::MovSP4N(dst, src, offset, n) => {
                assert!(*n < 8);
                dynasm!(ops
                    ; mov rcx, [rdi + src.index() as i32]
                );
                for i in 0..(*n as i32) {
                    let offset_i = i * 4 + *offset as i32;
                    dynasm!(ops
                        ; mov eax, [rcx + offset_i]
                        ; mov [rdi + dst.index() as i32 + offset_i], eax
                    );
                }
            }
            Instr::SlotAddr(dst, src) => {
                dynasm!(ops
                    ; lea rax, [rdi + src.index() as i32]
                    ; mov [rdi + dst.index() as i32], rax
                );
            }
            Instr::PointerOffset2(dst, src, n) => {
                dynasm!(ops
                    ; mov rax, [rdi + src.index() as i32]
                    ; add rax, *n
                    ; mov [rdi + dst.index() as i32], eax
                );
            }
            Instr::Call(base, func) => {
                let func_ptr = *func as *const Function as i64;
                let call_ptr = VMThread::call as i64;
                dynasm!(ops
                    ; mov rdx, rdi                  // stack pointer
                    ; add rdx, base.index() as i32
                    ; mov rdi, [rsp + 16]           // self (VMThread)
                    ; mov rsi, QWORD func_ptr       // function
                    ; mov rax, QWORD call_ptr
                    ; call rax
                    // restore stack pointer
                    ; mov rdi, [rsp + 8]
                );

                // self = rdi
                //VMThread::call(&mut self, func, stack_offset)
                // self.call(func,stack_offset + base.index() as u32);
            }
            Instr::Return => {
                dynasm!(ops
                    ; add rsp, FRAME_SIZE
                    ; ret
                );
            }
            Instr::Jump(offset) => {
                let target = labels[(pc as i32 + *offset) as usize];
                dynasm!(ops
                    ; jmp =>target
                );
            }
            Instr::JumpF(offset, cond) => {
                let target = labels[(pc as i32 + *offset) as usize];
                dynasm!(ops
                    ; mov al, [rdi + cond.index() as i32]
                    ; test al, al
                    ; jz =>target
                );
            }
            Instr::JumpT(offset, cond) => {
                let target = labels[(pc as i32 + *offset) as usize];
                dynasm!(ops
                    ; mov al, [rdi + cond.index() as i32]
                    ; test al, al
                    ; jnz =>target
                );
            }
            _ => return Err(format!("nyi: {:?}", bc)),
        }
    }

    dynasm!(ops
        ; mov rax, 0
        ; mov [0], rax
    );

    let buf = ops.finalize().unwrap();

    /*{
        eprint!("code: ");
        for x in buf.iter() {
            eprint!("{:02x}",x);
        }
        eprintln!();
    }*/

    let ptr = buf.as_ptr();
    std::mem::forget(buf);

    unsafe { Ok(std::mem::transmute(ptr)) }
}
