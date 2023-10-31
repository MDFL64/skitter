use std::sync::Mutex;

use ahash::AHashMap;
use rustc_hir::def_id::DefId;
use rustc_middle::ty::{
    AliasKind, FloatTy, GenericArg, GenericArgKind, ImplSubject, IntTy, Ty, TyKind, UintTy,
};

use crate::{
    closure::{Closure, ClosureSig},
    impls::find_inherent_impl_crate,
    items::{parent_def_from_rustc, path_from_rustc, AssocValue, FunctionSig},
    rustc_worker::RustCContext,
    types::{AutoTraitSet, ConstGeneric, Sub},
    vm::VM,
};

use colosseum::sync::Arena;

use super::{
    FloatWidth, IntSign, IntWidth, InternedType, ItemWithSubs, Mutability, SubList, Type, TypeKind,
};

pub struct TypeContext<'vm> {
    table: Mutex<AHashMap<TypeKind<'vm>, Type<'vm>>>,
    arena: Arena<InternedType<'vm>>,
}

impl<'vm> TypeContext<'vm> {
    pub fn new() -> Self {
        TypeContext {
            table: Default::default(),
            arena: Arena::new(),
        }
    }

    pub fn type_from_rustc<'tcx>(
        &'vm self,
        ty: Ty<'tcx>,
        ctx: &RustCContext<'vm, 'tcx>,
    ) -> Type<'vm> {
        let kind = ty.kind();
        let new_kind = match kind {
            TyKind::Int(int) => match int {
                IntTy::I8 => TypeKind::Int(IntWidth::I8, IntSign::Signed),
                IntTy::I16 => TypeKind::Int(IntWidth::I16, IntSign::Signed),
                IntTy::I32 => TypeKind::Int(IntWidth::I32, IntSign::Signed),
                IntTy::I64 => TypeKind::Int(IntWidth::I64, IntSign::Signed),
                IntTy::I128 => TypeKind::Int(IntWidth::I128, IntSign::Signed),
                IntTy::Isize => TypeKind::Int(IntWidth::ISize, IntSign::Signed),
            },
            TyKind::Uint(int) => match int {
                UintTy::U8 => TypeKind::Int(IntWidth::I8, IntSign::Unsigned),
                UintTy::U16 => TypeKind::Int(IntWidth::I16, IntSign::Unsigned),
                UintTy::U32 => TypeKind::Int(IntWidth::I32, IntSign::Unsigned),
                UintTy::U64 => TypeKind::Int(IntWidth::I64, IntSign::Unsigned),
                UintTy::U128 => TypeKind::Int(IntWidth::I128, IntSign::Unsigned),
                UintTy::Usize => TypeKind::Int(IntWidth::ISize, IntSign::Unsigned),
            },
            TyKind::Float(float) => match float {
                FloatTy::F32 => TypeKind::Float(FloatWidth::F32),
                FloatTy::F64 => TypeKind::Float(FloatWidth::F64),
            },
            TyKind::Bool => TypeKind::Bool,
            TyKind::Char => TypeKind::Char,
            TyKind::Never => TypeKind::Never,

            TyKind::Ref(_, ref_ty, mutability) => {
                let ref_ty = self.type_from_rustc(*ref_ty, ctx);
                let mutability = match mutability {
                    rustc_middle::mir::Mutability::Mut => Mutability::Mut,
                    rustc_middle::mir::Mutability::Not => Mutability::Const,
                };
                TypeKind::Ref(ref_ty, mutability)
            }
            TyKind::RawPtr(ptr) => {
                let ref_ty = self.type_from_rustc(ptr.ty, ctx);
                let mutability = match ptr.mutbl {
                    rustc_middle::mir::Mutability::Mut => Mutability::Mut,
                    rustc_middle::mir::Mutability::Not => Mutability::Const,
                };
                TypeKind::Ptr(ref_ty, mutability)
            }
            TyKind::Tuple(members) => {
                let members = members
                    .iter()
                    .map(|ty| self.type_from_rustc(ty, ctx))
                    .collect();
                TypeKind::Tuple(members)
            }
            TyKind::Array(member_ty, count) => {
                let member_ty = self.type_from_rustc(*member_ty, ctx);
                let (_, count) = self.const_from_rustc(count, ctx);

                TypeKind::Array(member_ty, count)
            }
            TyKind::Slice(member_ty) => {
                let member_ty = self.type_from_rustc(*member_ty, ctx);
                TypeKind::Slice(member_ty)
            }
            TyKind::Str => TypeKind::StringSlice,

            TyKind::Adt(adt, subs) => {
                let item_with_subs = self.def_from_rustc(adt.did(), subs, ctx);
                TypeKind::Adt(item_with_subs)
            }
            TyKind::FnDef(did, subs) => {
                let item_with_subs = self.def_from_rustc(*did, subs, ctx);
                TypeKind::FunctionDef(item_with_subs)
            }
            TyKind::Alias(alias_kind, alias_ty) => match alias_kind {
                AliasKind::Projection => {
                    let item_with_subs = self.def_from_rustc(alias_ty.def_id, alias_ty.substs, ctx);
                    TypeKind::AssociatedType(item_with_subs)
                }
                AliasKind::Opaque => {
                    let (parent_item, sub_id) =
                        self.opaque_type_from_rustc(alias_ty.def_id, alias_ty.substs, ctx);

                    TypeKind::Opaque(parent_item, sub_id)
                }
                AliasKind::Inherent => panic!("inherent alias?"),
            },
            TyKind::Foreign(did) => {
                let def_path = ctx.tcx.def_path(*did);
                let foreign_path = path_from_rustc(&def_path, ctx.vm);

                let crate_id = ctx.find_crate_id(did.krate);
                TypeKind::Foreign(crate_id, foreign_path.as_string())
            }
            TyKind::Dynamic(list, _, kind) => {
                use rustc_middle::ty::{DynKind, ExistentialPredicate};

                let is_dyn_star = match kind {
                    DynKind::Dyn => false,
                    DynKind::DynStar => true,
                };

                let mut primary_trait = None;
                let mut auto_traits = AutoTraitSet::EMPTY;

                for item in list.iter() {
                    match item.skip_binder() {
                        ExistentialPredicate::Trait(trait_ref) => {
                            if primary_trait.is_some() {
                                panic!("multiple primary traits on dyn");
                            }

                            let item = self.def_from_rustc(trait_ref.def_id, trait_ref.substs, ctx);

                            primary_trait = Some(item);
                        }
                        ExistentialPredicate::Projection(_) => {
                            println!("todo projection on dyn???");
                        }
                        ExistentialPredicate::AutoTrait(did) => {
                            let def_path = ctx.tcx.def_path(did);
                            let path = path_from_rustc(&def_path, ctx.vm);
                            let add_trait = match path.as_string() {
                                "::marker::Send" => AutoTraitSet::SEND,
                                "::marker::Sync" => AutoTraitSet::SYNC,
                                _ => panic!("auto trait {}", path.as_string()),
                            };
                            auto_traits.add(add_trait);
                        }
                    }
                }

                TypeKind::Dynamic {
                    primary_trait,
                    auto_traits,
                    is_dyn_star,
                }
            }
            TyKind::FnPtr(rs_sig) => {
                let sig = FunctionSig::from_rustc(&rs_sig.skip_binder(), ctx);
                TypeKind::FunctionPointer(sig)
            }
            TyKind::Closure(did, subs) => {
                let closure = self.closure_from_rustc(*did, ctx);

                let subs = self.subs_from_rustc(subs, ctx);

                let (_, subs) = ClosureSig::from_rustc_sub_repr(&subs);

                TypeKind::Closure(closure, subs)
            }

            TyKind::Param(param) => TypeKind::Param(param.index),

            _ => panic!("convert ty: {:?}", kind),
        };
        //println!("{:?} -> {:?}",ty,new_kind);
        self.intern(new_kind, ctx.vm)
    }

    pub fn subs_from_rustc<'tcx>(
        &'vm self,
        args: &[GenericArg<'tcx>],
        ctx: &RustCContext<'vm, 'tcx>,
    ) -> SubList<'vm> {
        let list = args
            .iter()
            .map(|s| match s.unpack() {
                GenericArgKind::Type(ty) => Sub::Type(self.type_from_rustc(ty, ctx)),
                GenericArgKind::Lifetime(_) => Sub::Lifetime,
                GenericArgKind::Const(ref c) => {
                    let (ty, kind) = self.const_from_rustc(c, ctx);

                    Sub::Const(ty, kind)
                }
            })
            .collect();
        SubList { list }
    }

    pub fn const_from_rustc<'tcx>(
        &'vm self,
        c: &rustc_middle::ty::Const<'tcx>,
        ctx: &RustCContext<'vm, 'tcx>,
    ) -> (Type<'vm>, ConstGeneric) {
        let ty = self.type_from_rustc(c.ty(), ctx);

        use rustc_middle::ty::ConstKind;
        use rustc_middle::ty::ValTree;

        let kind = match c.kind() {
            ConstKind::Expr(_) => {
                panic!("expr");
            }
            ConstKind::Value(ValTree::Leaf(val)) => {
                let n = val.to_bits(val.size()).unwrap();
                ConstGeneric::Value(n as i128)
            }
            ConstKind::Param(n) => ConstGeneric::Param(n.index),
            ConstKind::Unevaluated(..) => ConstGeneric::Error,
            _ => panic!("lower const {:?}", c.kind()),
        };

        (ty, kind)
    }

    pub fn def_from_rustc<'tcx>(
        &'vm self,
        did: DefId,
        args: &[GenericArg<'tcx>],
        ctx: &RustCContext<'vm, 'tcx>,
    ) -> ItemWithSubs<'vm> {
        if ctx.vm.cli_args.debug_local_impls && did.krate == rustc_hir::def_id::LOCAL_CRATE {
            if let Some(parent_impl) = ctx.tcx.impl_of_method(did) {
                let subject = ctx.tcx.impl_subject(parent_impl).skip_binder();
                if let ImplSubject::Inherent(ty) = subject {
                    let ty = self.type_from_rustc(ty, ctx);
                    let ident = ctx.tcx.item_name(did);

                    let res = ctx
                        .find_inherent_impl(ty, ident.as_str())
                        .expect("failed to find local impl");

                    if let AssocValue::Item(item_id) = res {
                        let item = ctx.get_item(item_id).expect("failed to find item");
                        let subs = self.subs_from_rustc(args, ctx);

                        return ItemWithSubs { item, subs };
                    } else {
                        panic!("can't convert value to item, this is a bug");
                    }
                }
            }
        }

        let item = if let Some(item) = ctx.item_by_did(did) {
            item
        } else {
            let def_path = ctx.tcx.def_path(did);
            let item_path = path_from_rustc(&def_path, ctx.vm);
            if item_path.can_lookup() {
                if did.krate == rustc_hir::def_id::LOCAL_CRATE {
                    panic!("attempt to find local item by path: {:?}", item_path);
                }
                let trait_crate_id = ctx.find_crate_id(did.krate);
                let crate_items = ctx.vm.crate_provider(trait_crate_id);

                if let Some(item) = crate_items.item_by_path(&item_path) {
                    item
                } else {
                    panic!("couldn't find item: {:?} -> {:?}", def_path, item_path)
                }
            } else if let Some(parent_impl) = ctx.tcx.impl_of_method(did) {
                let subject = ctx.tcx.impl_subject(parent_impl).skip_binder();
                let ident = ctx.tcx.item_name(did);
                match subject {
                    ImplSubject::Inherent(ty) => {
                        let ty = self.type_from_rustc(ty, ctx);
                        let ty_key = ty
                            .impl_key()
                            .expect("inherent impls must have a valid impl key!");
                        let full_key = format!("{}::{}", ty_key, ident.as_str());

                        let candidate_crate_ids = find_inherent_impl_crate(ty);

                        for crate_id in candidate_crate_ids {
                            let crate_provider = ctx.vm.crate_provider(crate_id);

                            let res = crate_provider.inherent_impl(&full_key, ty);

                            if let Some(AssocValue::Item(item_id)) = res {
                                let item = crate_provider.item_by_id(item_id);
                                let subs = self.subs_from_rustc(args, ctx);

                                return ItemWithSubs { item, subs };
                            }
                        }

                        panic!("failed to find inherent impl {:?}", did);
                    }
                    ImplSubject::Trait(_) => {
                        panic!("todo trait ref");
                    }
                }
            } else {
                panic!("couldn't find item: {:?} -> {:?}", def_path, item_path)
            }
        };

        let subs = self.subs_from_rustc(args, ctx);

        ItemWithSubs { item, subs }
    }

    pub fn closure_from_rustc<'tcx>(
        &'vm self,
        did: DefId,
        ctx: &RustCContext<'vm, 'tcx>,
    ) -> &'vm Closure<'vm> {
        let def_path = ctx.tcx.def_path(did);

        let full_path = path_from_rustc(&def_path, ctx.vm);

        let parent_did = parent_def_from_rustc(did, ctx);
        let parent_item = self.def_from_rustc(parent_did, &[], ctx);

        parent_item.item.child_closure(full_path.as_string())
    }

    pub fn closure_sig_from_rustc<'tcx>(
        &'vm self,
        ty: Ty<'tcx>,
        ctx: &RustCContext<'vm, 'tcx>,
    ) -> ClosureSig<'vm> {
        let kind = ty.kind();
        match kind {
            TyKind::Closure(_, subs) => {
                let subs = self.subs_from_rustc(subs, ctx);

                let (sig, _) = ClosureSig::from_rustc_sub_repr(&subs);
                sig
            }
            _ => panic!("attempt to get closure sig for non-closure"),
        }
    }

    /// This is similar to closure_from_rustc. Sadly we still need multiple path indices, since
    /// impl types can be nested: impl T<S = impl U>
    pub fn opaque_type_from_rustc<'tcx>(
        &'vm self,
        did: DefId,
        args: &[GenericArg<'tcx>],
        ctx: &RustCContext<'vm, 'tcx>,
    ) -> (ItemWithSubs<'vm>, &'vm str) {
        let def_path = ctx.tcx.def_path(did);

        let full_path = path_from_rustc(&def_path, ctx.vm);

        let parent_did = parent_def_from_rustc(did, ctx);
        let parent_item = self.def_from_rustc(parent_did, args, ctx);

        (parent_item, full_path.as_string())
    }

    pub fn intern(&'vm self, kind: TypeKind<'vm>, vm: &'vm VM<'vm>) -> Type<'vm> {
        let mut inner = self.table.lock().unwrap();
        let entry = inner.entry(kind.clone());

        entry
            .or_insert_with(|| {
                let intern_ref = self.arena.alloc(InternedType {
                    kind,
                    layout: Default::default(),
                    persist_id: Default::default(),
                });
                Type(intern_ref, vm)
            })
            .clone()
    }
}
