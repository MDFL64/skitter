use std::{collections::HashMap, sync::{Mutex}};

use rustc_middle::ty::{Ty, TyKind, IntTy, UintTy, FloatTy, GenericArg, GenericArgKind, ImplSubject, AliasKind};
use rustc_hir::def_id::DefId;

use crate::{vm::VM, rustc_worker::RustCContext, types::Sub, items::{ItemPath, path_from_rustc}};

use colosseum::sync::Arena;

use super::{TypeKind, IntWidth, IntSign, FloatWidth, ItemWithSubs, ArraySize, Mutability, Type, InternedType, SubList};

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

    pub fn type_from_rustc<'tcx>(&'vm self, ty: Ty<'tcx>, ctx: &RustCContext<'vm,'tcx>) -> Type<'vm> {
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

            TyKind::Ref(_,ref_ty,mutability) => {
                let ref_ty = self.type_from_rustc(*ref_ty, ctx);
                let mutability = match mutability {
                    rustc_middle::mir::Mutability::Mut => Mutability::Mut,
                    rustc_middle::mir::Mutability::Not => Mutability::Const
                };
                TypeKind::Ref(ref_ty,mutability)
            }
            TyKind::RawPtr(ptr) => {
                let ref_ty = self.type_from_rustc(ptr.ty, ctx);
                let mutability = match ptr.mutbl {
                    rustc_middle::mir::Mutability::Mut => Mutability::Mut,
                    rustc_middle::mir::Mutability::Not => Mutability::Const
                };
                TypeKind::Ptr(ref_ty,mutability)
            }
            TyKind::Tuple(members) => {
                let members = members.iter().map(|ty| {
                    self.type_from_rustc(ty, ctx)
                }).collect();
                TypeKind::Tuple(members)
            }
            TyKind::Array(member_ty,count) => {
                let param_env = rustc_middle::ty::ParamEnv::reveal_all();
                let count = match count.try_eval_target_usize(ctx.tcx, param_env) {
                    Some(n) => ArraySize::Static(n as u32),
                    None => ArraySize::ConstParam(0xFFFF) // todo
                };
                let member_ty = self.type_from_rustc(*member_ty, ctx);
                TypeKind::Array(member_ty, count)
            }
            TyKind::Slice(member_ty) => {
                let member_ty = self.type_from_rustc(*member_ty, ctx);
                TypeKind::Slice(member_ty)
            }
            TyKind::Str => {
                TypeKind::StringSlice
            }

            TyKind::Adt(adt,subs) => {
                let item_with_subs = self.def_from_rustc(adt.did(),subs,ctx);
                TypeKind::Adt(item_with_subs)
            }
            TyKind::FnDef(did,subs) => {
                let item_with_subs = self.def_from_rustc(*did,subs,ctx);
                TypeKind::FunctionDef(item_with_subs)
            }
            TyKind::Alias(alias_kind,alias_ty) => {
                match alias_kind {
                    AliasKind::Projection => {
                        let item_with_subs = self.def_from_rustc(alias_ty.def_id,alias_ty.substs,ctx);
                        TypeKind::AssociatedType(item_with_subs)
                    }
                    AliasKind::Opaque => {
                        panic!("todo opaque type {:?} / {:?}",alias_kind,alias_ty);
                    }
                }
            }
            TyKind::Foreign(_) => {
                TypeKind::Foreign
            }
            TyKind::Dynamic(..) => {
                TypeKind::Dynamic
            }
            TyKind::FnPtr(_) => {
                TypeKind::FunctionPointer
            }

            TyKind::Param(param) => {
                TypeKind::Param(param.index)
            }

            _ => panic!("convert ty: {:?}",kind)
        };
        //println!("{:?} -> {:?}",ty,new_kind);
        self.intern(new_kind,ctx.vm)
    }

    pub fn subs_from_rustc<'tcx>(&'vm self, args: &[GenericArg<'tcx>], ctx: &RustCContext<'vm,'tcx>) -> SubList<'vm> {
        let list = args.iter().map(|s| {
            match s.unpack() {
                GenericArgKind::Type(ty) => Sub::Type(self.type_from_rustc(ty, ctx)),
                GenericArgKind::Lifetime(_) => Sub::Lifetime,
                GenericArgKind::Const(_) => Sub::Const,
            }
        }).collect();
        SubList{ list }
    }

    pub fn def_from_rustc<'tcx>(&'vm self, did: DefId, args: &[GenericArg<'tcx>], ctx: &RustCContext<'vm,'tcx>) -> ItemWithSubs<'vm> {
        let item = if let Some(item) = ctx.items.find_by_did(did) {
            item
        } else {
            let def_path = ctx.tcx.def_path(did);
            let item_path = path_from_rustc(&def_path);
            if item_path.can_find() {
                assert!(did.krate != rustc_hir::def_id::LOCAL_CRATE);
                let trait_crate_id = ctx.items.find_crate_id(ctx.tcx, did.krate);
                let crate_items = ctx.vm.get_crate_items(trait_crate_id);
    
                if let Some(item) = crate_items.find_by_path(&item_path) {
                    item
                } else {
                    panic!("couldn't find item: {:?} -> {:?}",def_path,item_path)
                }
            } else if let Some(parent_impl) = ctx.tcx.impl_of_method(did) {
                let subject = ctx.tcx.impl_subject(parent_impl).skip_binder();
                let ident = ctx.tcx.item_name(did);
                match subject {
                    ImplSubject::Inherent(ty) => {
                        let ty = self.type_from_rustc(ty, ctx);
                        if let Some((crate_id,item_id)) = ty.find_impl_member(ident.as_str()) {
                            let crate_items = ctx.vm.get_crate_items(crate_id);
                            crate_items.get(item_id)
                        } else {
                            panic!("missing type impl");
                        }
                    }
                    ImplSubject::Trait(trait_ref) => {
                        panic!("todo trait ref");
                    }
                }
            } else {
                panic!("couldn't find item: {:?} -> {:?}",def_path,item_path)
            }
        };

        let subs = self.subs_from_rustc(args, ctx);

        ItemWithSubs{
            item,
            subs
        }
    }

    pub fn intern(&'vm self, kind: TypeKind<'vm>, vm: &'vm VM<'vm>) -> Type<'vm> {
        let mut inner = self.table.lock().unwrap();
        let entry = inner.entry(kind.clone());

        entry.or_insert_with(|| {
            let intern_ref = self.arena.alloc(InternedType{
                kind,
                layout: Default::default(),
                impl_table: Default::default()
            });
            Type(intern_ref,vm)
        }).clone()
    }
}
