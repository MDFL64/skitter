use std::{collections::HashMap, cell::RefCell};

use rustc_middle::ty::{TyCtxt, Ty, TyKind, IntTy, UintTy, FloatTy};
use rustc_hir::def_id::DefId;
use bumpalo::Bump;

use super::{TypeKind, IntWidth, IntSign, TypeDef, FloatWidth};

#[derive(Debug,Hash,PartialEq,Eq,Copy,Clone)]
pub struct Type<'vm>(&'vm InternedType<'vm>);

#[derive(Default)]
pub struct TypeContext<'vm> {
    arena: Bump,
    table: RefCell<HashMap<TypeKind<'vm>,Type<'vm>>>
}

impl<'vm> TypeContext<'vm> {
    pub fn type_from_rustc<'tcx>(&'vm self, ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Type {
        let kind = ty.kind();
        let new_kind = match kind {
            TyKind::Int(int) => {
                match int {
                    IntTy::I8 => TypeKind::Int(IntWidth::I8, IntSign::Signed),
                    IntTy::I16 => TypeKind::Int(IntWidth::I16, IntSign::Signed),
                    IntTy::I32 => TypeKind::Int(IntWidth::I32, IntSign::Signed),
                    IntTy::I64 => TypeKind::Int(IntWidth::I64, IntSign::Signed),
                    IntTy::I128 => TypeKind::Int(IntWidth::I128, IntSign::Signed),
                    IntTy::Isize => TypeKind::Int(IntWidth::ISize, IntSign::Signed),
                }
            }
            TyKind::Uint(int) => {
                match int {
                    UintTy::U8 => TypeKind::Int(IntWidth::I8, IntSign::Unsigned),
                    UintTy::U16 => TypeKind::Int(IntWidth::I16, IntSign::Unsigned),
                    UintTy::U32 => TypeKind::Int(IntWidth::I32, IntSign::Unsigned),
                    UintTy::U64 => TypeKind::Int(IntWidth::I64, IntSign::Unsigned),
                    UintTy::U128 => TypeKind::Int(IntWidth::I128, IntSign::Unsigned),
                    UintTy::Usize => TypeKind::Int(IntWidth::ISize, IntSign::Unsigned),
                }
            }
            TyKind::Float(float) => {
                match float {
                    FloatTy::F32 => TypeKind::Float(FloatWidth::F32),
                    FloatTy::F64 => TypeKind::Float(FloatWidth::F64)
                }
            }
            TyKind::Bool => TypeKind::Bool,
            TyKind::Char => TypeKind::Char,
            TyKind::Never => TypeKind::Never,

            TyKind::Ref(_,ref_ty,_) => {
                let ref_ty = self.type_from_rustc(*ref_ty, tcx);
                TypeKind::Ref(ref_ty)
            }
            TyKind::RawPtr(ptr) => {
                let ref_ty = self.type_from_rustc(ptr.ty, tcx);
                TypeKind::Ptr(ref_ty)
            }
            TyKind::Tuple(members) => {
                let members = members.iter().map(|ty| {
                    self.type_from_rustc(ty, tcx)
                }).collect();
                TypeKind::Tuple(members)
            }
            TyKind::Array(member_ty,count) => {
                let param_env = rustc_middle::ty::ParamEnv::reveal_all();
                let count = count.eval_target_usize(tcx, param_env) as u32;
                let member_ty = self.type_from_rustc(*member_ty, tcx);
                TypeKind::Array(member_ty, count)
            }
            TyKind::Slice(member_ty) => {
                let member_ty = self.type_from_rustc(*member_ty, tcx);
                TypeKind::Slice(member_ty)
            }

            TyKind::Adt(adt,subs) => {
                assert!(subs.len() == 0);

                let def = Self::def_from_rustc(adt.did());
                TypeKind::Adt(def)
            }
            TyKind::FnDef(did,subs) => {
                assert!(subs.len() == 0);

                let def = Self::def_from_rustc(*did);
                TypeKind::FunctionDef( def )
            }
            _ => panic!("convert ty: {:?}",kind)
        };
        self.intern(new_kind)
    }

    fn def_from_rustc(did: DefId) -> TypeDef {
        if did.krate == rustc_hir::def_id::LOCAL_CRATE {
            TypeDef::Local(did.index.as_u32())
        } else {
            panic!("todo non-local def");
        }
    }

    fn intern(&'vm self, kind: TypeKind<'vm>) -> Type<'vm> {
        let mut table = self.table.borrow_mut();
        let entry = table.entry(kind.clone());

        entry.or_insert_with(|| {
            let intern_ref = self.arena.alloc(InternedType{kind});
            //println!("interned {:?}",intern_ref);
            Type(intern_ref)
        }).clone()
    }
}

#[derive(Debug,Hash,PartialEq,Eq)]
struct InternedType<'vm> {
    kind: TypeKind<'vm>
}
