// I do not like proc macros, but in retrospect this whole thing
// could probably be accomplished with some trivial macros.

pub fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    write_exec_match();
}

fn write_unary(instr: &str, ty: &str, op: &str, source: &mut String) {
    source.push_str(&format!(
        "
    Instr::{instr}(out, src) => {{
        let x: {ty} = read_stack(stack, *src);
        let res = {op};
        write_stack(stack, *out, res);
    }}"
    ));
}

fn read_pointer(instr: &str, ty: &str, source: &mut String) {
    source.push_str(&format!(
        "
    Instr::{instr}(out, src, offset) => {{
        let ptr: *mut u8 = read_stack(stack, *src);
        let ptr = ptr.offset(*offset as isize) as *mut {ty};
        let res = *ptr;
        write_stack(stack, *out, res);
    }}"
    ));
}

fn write_pointer(instr: &str, ty: &str, source: &mut String) {
    source.push_str(&format!(
        "
    Instr::{instr}(out, src, offset) => {{
        let res: {ty} = read_stack(stack, *src);
        let ptr: *mut u8 = read_stack(stack, *out);
        let ptr = ptr.offset(*offset as isize) as *mut {ty};
        *ptr = res;
    }}"
    ));
}

fn write_bulk_move_ss(instr: &str, ty: &str, source: &mut String) {
    source.push_str(&format!(
        "
    Instr::{instr}(dst, src, n) => {{
        let src_ptr = stack.add(src.index()) as *mut {ty};
        let dst_ptr = stack.add(dst.index()) as *mut {ty};
        for i in 0..(*n as usize) {{
            let s = src_ptr.add(i);
            let d = dst_ptr.add(i);

            *d = *s;
        }}
    }}"
    ));
}

fn write_bulk_move_ps(instr: &str, ty: &str, source: &mut String) {
    source.push_str(&format!(
        "
    Instr::{instr}(dst, src, n) => {{
        let src_ptr = stack.add(*src as usize) as *mut {ty};
        let dst_ptr: *mut {ty} = read_stack(stack, *dst);
        for i in 0..(*n as usize) {{
            let s = src_ptr.add(i);
            let d = dst_ptr.add(i);

            *d = *s;
        }}
    }}"
    ));
}

fn write_binary(instr: &str, ty: &str, op: &str, source: &mut String) {
    source.push_str(&format!(
        "
    Instr::{instr}(out, lhs, rhs) => {{
        let a: {ty} = read_stack(stack, *lhs);
        let b: {ty} = read_stack(stack, *rhs);
        let res = {op};
        write_stack(stack, *out, res);
    }}"
    ));
}

fn write_shift(instr: &str, ty: &str, op: &str, source: &mut String) {
    source.push_str(&format!(
        "
    Instr::{instr}(out, lhs, rhs) => {{
        let a: {ty} = read_stack(stack, *lhs);
        let b: u8 = read_stack(stack, *rhs);
        let res = {op};
        write_stack(stack, *out, res);
    }}"
    ));
}

fn write_immediate(instr: &str, ty: &str, op: &str, source: &mut String) {
    source.push_str(&format!(
        "
    Instr::{instr}(out, x) => {{
        let res: {ty} = {op};
        write_stack(stack, *out, res);
    }}"
    ));
}

fn write_widen(dst_bits: i32, src_bits: i32, signed: bool, source: &mut String) {
    let sign_char = if signed { 'S' } else { 'U' };
    let ty_char = if signed { 'i' } else { 'u' };
    source.push_str(&format!(
        "
    Instr::I{dst_bits}_{sign_char}_Widen_{src_bits}(out, src) => {{
        let x: {ty_char}{src_bits} = read_stack(stack, *src);
        let res = x as {ty_char}{dst_bits};
        write_stack(stack, *out, res);
    }}"
    ));
}

fn write_cast(name: &str, dst_ty: &str, src_ty: &str, source: &mut String) {
    source.push_str(&format!(
        "
    Instr::{name}(out, src) => {{
        let x: {src_ty} = read_stack(stack, *src);
        let res = x as {dst_ty};
        write_stack(stack, *out, res);
    }}"
    ));
}

fn write_int_ops(signed: &str, unsigned: &str, source: &mut String) {
    let big = signed.to_uppercase();

    if signed == "i128" {
        // I128 constants are boxed
        write_immediate(&format!("{}_Const", big), signed, "**x", source);
    } else {
        write_immediate(&format!("{}_Const", big), signed, "*x", source);
    }
    write_unary(&format!("{}_Neg", big), signed, "x.wrapping_neg()", source);
    write_unary(&format!("{}_Not", big), signed, "!x", source);
    write_binary(&format!("{}_Eq", big), signed, "a == b", source);
    write_binary(&format!("{}_NotEq", big), signed, "a != b", source);
    write_binary(&format!("{}_Add", big), signed, "a.wrapping_add(b)", source);
    write_binary(&format!("{}_Sub", big), signed, "a.wrapping_sub(b)", source);
    write_binary(&format!("{}_Mul", big), signed, "a.wrapping_mul(b)", source);
    write_binary(&format!("{}_Or", big), signed, "a | b", source);
    write_binary(&format!("{}_And", big), signed, "a & b", source);
    write_binary(&format!("{}_Xor", big), signed, "a ^ b", source);
    write_shift(
        &format!("{}_ShiftL", big),
        signed,
        "a.wrapping_shl(b as _)",
        source,
    );
    write_binary(&format!("{}_S_Lt", big), signed, "a < b", source);
    write_binary(&format!("{}_S_LtEq", big), signed, "a <= b", source);
    write_binary(
        &format!("{}_S_Div", big),
        signed,
        "a.wrapping_div(b)",
        source,
    );
    write_binary(
        &format!("{}_S_Rem", big),
        signed,
        "a.wrapping_rem(b)",
        source,
    );
    write_shift(
        &format!("{}_S_ShiftR", big),
        signed,
        "a.wrapping_shr(b as _)",
        source,
    );
    write_binary(&format!("{}_U_Lt", big), unsigned, "a < b", source);
    write_binary(&format!("{}_U_LtEq", big), unsigned, "a <= b", source);
    write_binary(
        &format!("{}_U_Div", big),
        unsigned,
        "a.wrapping_div(b)",
        source,
    );
    write_binary(
        &format!("{}_U_Rem", big),
        unsigned,
        "a.wrapping_rem(b)",
        source,
    );
    write_shift(
        &format!("{}_U_ShiftR", big),
        unsigned,
        "a.wrapping_shr(b as _)",
        source,
    );
}

fn write_float_ops(ty: &str, source: &mut String) {
    let big = ty.to_uppercase();
    write_unary(&format!("{}_Neg", big), ty, "-x", source);
    write_binary(&format!("{}_Add", big), ty, "a + b", source);
    write_binary(&format!("{}_Sub", big), ty, "a - b", source);
    write_binary(&format!("{}_Mul", big), ty, "a * b", source);
    write_binary(&format!("{}_Div", big), ty, "a / b", source);
    write_binary(&format!("{}_Rem", big), ty, "a % b", source);

    write_binary(&format!("{}_Eq", big), ty, "a == b", source);
    write_binary(&format!("{}_NotEq", big), ty, "a != b", source);

    write_binary(&format!("{}_Lt", big), ty, "a < b", source);
    write_binary(&format!("{}_LtEq", big), ty, "a <= b", source);
    write_binary(&format!("{}_Gt", big), ty, "a > b", source);
    write_binary(&format!("{}_GtEq", big), ty, "a >= b", source);
}

fn write_exec_match() {
    let mut source = String::new();
    source.push_str("match instr {");

    write_int_ops("i8", "u8", &mut source);
    write_int_ops("i16", "u16", &mut source);
    write_int_ops("i32", "u32", &mut source);
    write_int_ops("i64", "u64", &mut source);
    write_int_ops("i128", "u128", &mut source);

    write_float_ops("f32", &mut source);
    write_float_ops("f64", &mut source);

    // Integer not won't work for bools
    write_unary("Bool_Not", "bool", "!x", &mut source);

    // float casts
    write_cast("F64_From_F32", "f64", "f32", &mut source);
    write_cast("F64_From_I8_S", "f64", "i8", &mut source);
    write_cast("F64_From_I8_U", "f64", "u8", &mut source);
    write_cast("F64_From_I16_S", "f64", "i16", &mut source);
    write_cast("F64_From_I16_U", "f64", "u16", &mut source);
    write_cast("F64_From_I32_S", "f64", "i32", &mut source);
    write_cast("F64_From_I32_U", "f64", "u32", &mut source);
    write_cast("F64_From_I64_S", "f64", "i64", &mut source);
    write_cast("F64_From_I64_U", "f64", "u64", &mut source);
    write_cast("F64_From_I128_S", "f64", "i128", &mut source);
    write_cast("F64_From_I128_U", "f64", "u128", &mut source);

    write_cast("F64_Into_I8_S", "i8", "f64", &mut source);
    write_cast("F64_Into_I8_U", "u8", "f64", &mut source);
    write_cast("F64_Into_I16_S", "i16", "f64", &mut source);
    write_cast("F64_Into_I16_U", "u16", "f64", &mut source);
    write_cast("F64_Into_I32_S", "i32", "f64", &mut source);
    write_cast("F64_Into_I32_U", "u32", "f64", &mut source);
    write_cast("F64_Into_I64_S", "i64", "f64", &mut source);
    write_cast("F64_Into_I64_U", "u64", "f64", &mut source);
    write_cast("F64_Into_I128_S", "i128", "f64", &mut source);
    write_cast("F64_Into_I128_U", "u128", "f64", &mut source);

    write_cast("F32_From_F64", "f32", "f64", &mut source);
    write_cast("F32_From_I8_S", "f32", "i8", &mut source);
    write_cast("F32_From_I8_U", "f32", "u8", &mut source);
    write_cast("F32_From_I16_S", "f32", "i16", &mut source);
    write_cast("F32_From_I16_U", "f32", "u16", &mut source);
    write_cast("F32_From_I32_S", "f32", "i32", &mut source);
    write_cast("F32_From_I32_U", "f32", "u32", &mut source);
    write_cast("F32_From_I64_S", "f32", "i64", &mut source);
    write_cast("F32_From_I64_U", "f32", "u64", &mut source);
    write_cast("F32_From_I128_S", "f32", "i128", &mut source);
    write_cast("F32_From_I128_U", "f32", "u128", &mut source);

    write_cast("F32_Into_I8_S", "i8", "f32", &mut source);
    write_cast("F32_Into_I8_U", "u8", "f32", &mut source);
    write_cast("F32_Into_I16_S", "i16", "f32", &mut source);
    write_cast("F32_Into_I16_U", "u16", "f32", &mut source);
    write_cast("F32_Into_I32_S", "i32", "f32", &mut source);
    write_cast("F32_Into_I32_U", "u32", "f32", &mut source);
    write_cast("F32_Into_I64_S", "i64", "f32", &mut source);
    write_cast("F32_Into_I64_U", "u64", "f32", &mut source);
    write_cast("F32_Into_I128_S", "i128", "f32", &mut source);
    write_cast("F32_Into_I128_U", "u128", "f32", &mut source);

    // widening operations
    write_widen(16, 8, true, &mut source);
    write_widen(16, 8, false, &mut source);

    write_widen(32, 16, true, &mut source);
    write_widen(32, 16, false, &mut source);
    write_widen(32, 8, true, &mut source);
    write_widen(32, 8, false, &mut source);

    write_widen(64, 32, true, &mut source);
    write_widen(64, 32, false, &mut source);
    write_widen(64, 16, true, &mut source);
    write_widen(64, 16, false, &mut source);
    write_widen(64, 8, true, &mut source);
    write_widen(64, 8, false, &mut source);

    write_widen(128, 64, true, &mut source);
    write_widen(128, 64, false, &mut source);
    write_widen(128, 32, true, &mut source);
    write_widen(128, 32, false, &mut source);
    write_widen(128, 16, true, &mut source);
    write_widen(128, 16, false, &mut source);
    write_widen(128, 8, true, &mut source);
    write_widen(128, 8, false, &mut source);

    // moves
    write_unary("MovSS1", "u8", "x", &mut source);
    write_unary("MovSS2", "u16", "x", &mut source);
    write_unary("MovSS4", "u32", "x", &mut source);
    write_unary("MovSS8", "u64", "x", &mut source);
    write_unary("MovSS16", "u128", "x", &mut source);

    read_pointer("MovSP1", "u8", &mut source);
    read_pointer("MovSP2", "u16", &mut source);
    read_pointer("MovSP4", "u32", &mut source);
    read_pointer("MovSP8", "u64", &mut source);
    read_pointer("MovSP16", "u128", &mut source);

    write_pointer("MovPS1", "u8", &mut source);
    write_pointer("MovPS2", "u16", &mut source);
    write_pointer("MovPS4", "u32", &mut source);
    write_pointer("MovPS8", "u64", &mut source);
    write_pointer("MovPS16", "u128", &mut source);

    //write_bulk_move_ss("MovSS4N", "u32", &mut source);
    //write_bulk_move_ps("MovPS4N", "u32", &mut source);

    write_bulk_move_ss("MovSSN","u8",&mut source);

    source.push_str(
        r#"
    Instr::Jump(offset) => {
        pc = (pc as isize + *offset as isize) as usize;
        continue;
    }
    Instr::JumpF(offset, cond) => {
        let x: bool = read_stack(stack, *cond);
        if !x {
            pc = (pc as isize + *offset as isize) as usize;
            continue;
        }
    }
    Instr::JumpT(offset, cond) => {
        let x: bool = read_stack(stack, *cond);
        if x {
            pc = (pc as isize + *offset as isize) as usize;
            continue;
        }
    }
    /*Instr::Call(base,func) => {
        self.call(func,stack_offset + base.index() as u32);
    }*/
    Instr::SlotAddr(out,arg) => {
        let res = stack.add(arg.index()) as usize;
        write_stack(stack, *out, res);
    }
    Instr::SlotAddrOffset{out,arg,offset} => {
        let offset: usize = read_stack(stack, *offset);
        let res = stack.add(arg.index()) as usize + offset;
        write_stack(stack, *out, res);
    }
    Instr::PointerOffset(arg_out,arg_2,offset_n) => {
        let offset_1: usize = read_stack(stack, *arg_out);
        let offset_2: usize = read_stack(stack, *arg_2);

        let res = offset_1 + offset_2 + *offset_n as usize;
        write_stack(stack, *arg_out, res);
    }
    Instr::IndexCalc { arg_out, elem_size, elem_count } => {
        let index: usize = read_stack(stack, *arg_out);
        if index >= *elem_count as usize {
            panic!("array index out of bounds");
        }
        let offset = index * *elem_size as usize;
        write_stack(stack, *arg_out, offset);
    }
    Instr::IndexCalcDyn{ arg_out, elem_size, elem_count } => {
        let index: usize = read_stack(stack, *arg_out);
        let elem_count: usize = read_stack(stack, *elem_count);
        if index >= elem_count {
            println!("{} {}",index,elem_count);
            panic!("array index out of bounds");
        }
        let offset = index * *elem_size as usize;
        write_stack(stack, *arg_out, offset);
    }
    Instr::Return => break,
    Instr::Bad => panic!("encountered bad instruction"),
    Instr::Debug(_) => (),
    _ => panic!("NYI {:?}",instr)
}"#,
    );

    let out_dir = std::env::var("OUT_DIR").unwrap();
    std::fs::write(format!("{out_dir}/exec_match.rs"), source).unwrap();
}
