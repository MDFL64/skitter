use std::{collections::HashMap, sync::{Mutex, OnceLock}, ops::SubAssign};

use rustc_middle::ty::{Ty, TyKind, IntTy, UintTy, FloatTy, GenericArg};
use rustc_hir::def_id::DefId;

use crate::{vm::VM, rustc_worker::RustCContext};

use colosseum::sync::Arena;

use super::{TypeKind, IntWidth, IntSign, TypeDef, FloatWidth, layout::Layout};

#[derive(Copy,Clone)]
pub struct Type<'vm>(&'vm InternedType<'vm>,&'vm VM<'vm>);

impl<'vm> Type<'vm> {
    pub fn kind(&self) -> &TypeKind<'vm> {
        &self.0.kind
    }

    pub fn layout(&self) -> &'vm Layout {
        self.0.layout.get_or_init(|| {
            super::layout::Layout::from(*self)
        })
    }

    pub fn sign(&self) -> IntSign {
        match self.kind() {
            TypeKind::Int(_,sign) => *sign,
            _ => IntSign::Unsigned
        }
    }
}

impl<'vm> std::fmt::Debug for Type<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'vm> std::hash::Hash for Type<'vm> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const _ as usize);
    }
}

impl<'vm> std::cmp::PartialEq for Type<'vm> {
    fn eq(&self, other: &Self) -> bool {
        let a = self.0 as *const _;
        let b = other.0 as *const _;
        a == b
    }
}

impl<'vm> std::cmp::Eq for Type<'vm> {}

pub struct TypeContext<'vm>{
    table: Mutex<HashMap<TypeKind<'vm>,Type<'vm>>>,
    arena: Arena<InternedType<'vm>>
}

impl<'vm> TypeContext<'vm> {
    pub fn new() -> Self {
        TypeContext{
            table: Default::default(),
            arena: Arena::new()
        }
    }

    pub fn type_from_rustc<'tcx,'a>(&'vm self, ty: Ty<'tcx>, ctx: &RustCContext<'vm,'tcx,'a>) -> Type<'vm> {
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
                let ref_ty = self.type_from_rustc(*ref_ty, ctx);
                TypeKind::Ref(ref_ty)
            }
            TyKind::RawPtr(ptr) => {
                let ref_ty = self.type_from_rustc(ptr.ty, ctx);
                TypeKind::Ptr(ref_ty)
            }
            TyKind::Tuple(members) => {
                let members = members.iter().map(|ty| {
                    self.type_from_rustc(ty, ctx)
                }).collect();
                TypeKind::Tuple(members)
            }
            TyKind::Array(member_ty,count) => {
                let param_env = rustc_middle::ty::ParamEnv::reveal_all();
                let count = count.eval_target_usize(ctx.tcx, param_env) as u32;
                let member_ty = self.type_from_rustc(*member_ty, ctx);
                TypeKind::Array(member_ty, count)
            }
            TyKind::Slice(member_ty) => {
                let member_ty = self.type_from_rustc(*member_ty, ctx);
                TypeKind::Slice(member_ty)
            }

            TyKind::Adt(adt,subs) => {
                assert!(subs.len() == 0);

                let def = Self::def_from_rustc(adt.did(),ctx,subs);
                TypeKind::Adt(def)
            }
            TyKind::FnDef(did,subs) => {
                let def = Self::def_from_rustc(*did,ctx,subs);
                TypeKind::FunctionDef(def)
            }
            _ => panic!("convert ty: {:?}",kind)
        };
        self.intern(new_kind,ctx.vm)
    }

    fn def_from_rustc<'tcx,'a>(did: DefId, ctx: &RustCContext<'vm,'tcx,'a>, subs: &[GenericArg]) -> TypeDef<'vm> {
        let item = if did.krate == rustc_hir::def_id::LOCAL_CRATE {
            *ctx.items_local.get(&did.index).expect("couldn't find local item")
        } else {
            panic!("todo non-local def {:?}",did);
        };

        assert!(subs.len() == 0);

        TypeDef{
            item,
            subs: Vec::new()
        }
    }

    fn intern(&'vm self, kind: TypeKind<'vm>, vm: &'vm VM<'vm>) -> Type<'vm> {
        let mut inner = self.table.lock().unwrap();
        let entry = inner.entry(kind.clone());

        entry.or_insert_with(|| {
            let intern_ref = self.arena.alloc(InternedType{
                kind,
                layout: Default::default()
            });
            Type(intern_ref,vm)
        }).clone()
    }
}

#[derive(Debug)]
struct InternedType<'vm> {
    kind: TypeKind<'vm>,
    layout: OnceLock<Layout>
}
