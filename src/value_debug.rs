use crate::{
    abi::POINTER_SIZE,
    items::AdtKind,
    types::{FloatWidth, IntSign, IntWidth, Type, TypeKind},
    variants::Discriminant,
};

/// A debug utility for printing values at runtime.
pub unsafe fn print_value<'vm>(ty: Type<'vm>, ptr: *const u8, meta: usize) {
    match ty.kind() {
        TypeKind::Ref(ref_ty, _) => {
            let ref_ptr: *const u8 = std::ptr::read(ptr as _);
            if ref_ty.is_sized() {
                print!("&({:?}) ", ref_ptr);
                print_value(*ref_ty, ref_ptr, 0);
            } else {
                let meta: usize = std::ptr::read(ptr.offset(POINTER_SIZE.bytes() as _) as _);
                print!("&({:?},0x{:x}) ", ref_ptr, meta);
                print_value(*ref_ty, ref_ptr, meta);
            }
        }
        TypeKind::Ptr(ref_ty, _) => {
            let ref_ptr: *const u8 = std::ptr::read(ptr as _);
            if ref_ty.is_sized() {
                print!("*({:?})", ref_ptr);
            } else {
                let meta: usize = std::ptr::read(ptr.offset(POINTER_SIZE.bytes() as _) as _);
                print!("*({:?},0x{:x})", ref_ptr, meta);
            }
        }

        TypeKind::Adt(item_ref) => {
            let adt_info = item_ref.item.adt_info();
            let adt_layout = ty.layout();

            let discriminant = match &adt_info.kind {
                AdtKind::Struct => {
                    print!("struct{{ ");
                    Some(Discriminant::NONE)
                }
                AdtKind::Enum(e) => {
                    let disc = e.discriminant_internal;

                    print!("enum(");
                    print_value(disc, ptr, 0);
                    print!("){{ ");

                    match e.discriminant_internal.kind() {
                        TypeKind::Int(IntWidth::I32, IntSign::Unsigned) => {
                            let n: u32 = std::ptr::read(ptr as _);
                            Some(Discriminant::new(n as _))
                        }
                        _ => {
                            print!("?");
                            None
                        }
                    }
                }
                AdtKind::Union => {
                    print!("union{{ ?");
                    None
                }
            };

            if let Some(discriminant) = discriminant {
                let variant = adt_info.variant_fields.index_for_discriminant(discriminant);

                for (i, (ty, offset)) in adt_info
                    .variant_fields
                    .get(variant)
                    .iter()
                    .zip(adt_layout.field_offsets.get(variant))
                    .enumerate()
                {
                    let field_ty = ty.sub(&item_ref.subs);
                    if i != 0 {
                        print!(" , ");
                    }

                    let field_ptr = ptr.offset(*offset as isize);
                    print_value(field_ty, field_ptr, 0);
                }
            }
            print!(" }}");
        }
        TypeKind::Tuple(children) => {
            if children.len() == 0 {
                print!("()");
            } else {
                let tup_layout = ty.layout();
                print!("( ");

                for (i, (ty, offset)) in children
                    .iter()
                    .zip(tup_layout.field_offsets.assert_single())
                    .enumerate()
                {
                    if i != 0 {
                        print!(" , ");
                    }

                    let field_ptr = ptr.offset(*offset as isize);
                    print_value(*ty, field_ptr, 0);
                }
                print!(" )");
            }
        }

        TypeKind::StringSlice => {
            let byte_slice = std::slice::from_raw_parts(ptr, meta);
            let string = std::str::from_utf8(byte_slice).expect("bar str");
            print!("{:?}", string);
        }
        TypeKind::Slice(child_ty) => {
            print!("[ ");
            let child_size = child_ty.layout().assert_size();
            for i in 0..meta {
                if i != 0 {
                    print!(" , ");
                }
                let child_offset = child_size as isize * i as isize;

                print_value(*child_ty, ptr.offset(child_offset), 0);
            }
            print!(" ]");
        }

        TypeKind::Int(IntWidth::I32, IntSign::Signed) => print_raw::<i32>(ptr),
        TypeKind::Int(IntWidth::I64, IntSign::Signed) => print_raw::<i64>(ptr),

        TypeKind::Int(IntWidth::I8, IntSign::Unsigned) => print_raw::<u8>(ptr),
        TypeKind::Int(IntWidth::I32, IntSign::Unsigned) => print_raw::<u32>(ptr),
        TypeKind::Int(IntWidth::ISize, IntSign::Unsigned) => print_raw::<usize>(ptr),

        TypeKind::Float(FloatWidth::F32) => print_raw::<f32>(ptr),

        TypeKind::Bool => print_raw::<bool>(ptr),
        TypeKind::Char => {
            print!("'");
            print_raw::<char>(ptr);
            print!("'");
        }

        _ => print!("NYI={}", ty),
    }
}

unsafe fn print_raw<T: std::fmt::Display>(ptr: *const u8) {
    let n: T = std::ptr::read(ptr as _);
    print!("{}", n);
}
