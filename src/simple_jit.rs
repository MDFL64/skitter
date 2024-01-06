use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi, DynamicLabel};

use crate::{vm::{NativeFunc, instr::Instr, VMThread, Function}, bytecode_compiler::FunctionBytecode};

static mut OUT: i32 = 0;


// CALLEE-SAVED: r12, r13, r14, r15, rbx, rsp, rbp
pub fn compile<'vm>(bytecode: &'vm FunctionBytecode<'vm>) -> Result<NativeFunc,String> {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    dynasm!(ops
        ; sub rsp, 0x20
        ; mov [rsp + 8], rdi    // stack pointer
        ; mov [rsp + 16], rsi   // thread
    );

    if bytecode.drops.len() > 0 {
        return Err("drops are unsupported".to_owned());
    }

    for bc in bytecode.code.iter() {
        match bc {
            Instr::I32_Const(slot,n) => {
                dynasm!(ops
                    ; mov DWORD [rdi + slot.index() as i32], *n
                );
            }
            Instr::I32_Neg(dst,src) => {
                dynasm!(ops
                    ; mov eax, [rdi + src.index() as i32]
                    ; neg eax
                    ; mov [rdi + dst.index() as i32], eax
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
            Instr::MovSS16(dst, src) => {
                dynasm!(ops
                    ; mov rax, [rdi + src.index() as i32]
                    ; mov [rdi + dst.index() as i32], rax
                    ; mov rax, [rdi + src.index() as i32 + 8]
                    ; mov [rdi + dst.index() as i32 + 8], rax
                );
            }
            Instr::Call(base,func) => {
                let func_ptr = *func as *const Function as i64;
                let call_ptr = meme as i64;//VMThread::call as i64;
                dynasm!(ops
                    ; mov rdx, rdi                  // stack offset
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
                    ; add rsp, 0x20
                    ; ret
                );
            }
            _ => return Err(format!("nyi: {:?}",bc))
        }
    }

    dynasm!(ops
        ; mov rax, 0
        ; mov [0], rax
    );

    let buf = ops.finalize().unwrap();

    {
        print!("code: ");
        for x in buf.iter() {
            print!("{:02x}",x);
        }
        println!();
    }

    //Err("not implemented".to_owned())

    let ptr = buf.as_ptr();
    std::mem::forget(buf);

    unsafe {
        Ok(std::mem::transmute(ptr))
    }
}

pub fn check() {
    println!("{}",unsafe { OUT });
}

extern "C" fn meme(vmt: &VMThread, func: &Function, stack_offset: u32) {
    println!("meme! {}",stack_offset);
}
