use std::{collections::HashMap, sync::{Mutex, OnceLock, RwLock}};

use ahash::AHashMap;
use rustc_middle::ty::{Ty, TyKind, IntTy, UintTy, FloatTy, GenericArg, GenericArgKind, ImplSubject};
use rustc_hir::def_id::DefId;

use crate::{vm::VM, rustc_worker::RustCContext, types::Sub, items::{ItemPath, Item, CrateId, ItemId}};

use colosseum::sync::Arena;

use super::{TypeKind, IntWidth, IntSign, FloatWidth, ItemWithSubs, layout::Layout, ArraySize, Mutability};

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

    pub fn sub(&self, subs: &[Sub<'vm>]) -> Self {
        if subs.len() == 0 {
            return *self;
        }
        // todo add a field which indicates whether a type accepts subs, fast return if not
        let vm = self.1;
        match self.kind() {
            TypeKind::Param(n) => {
                let sub = &subs[*n as usize];
                if let Sub::Type(sub_ty) = sub {
                    *sub_ty
                } else {
                    panic!("bad sub");
                }
            }
            TypeKind::Ref(ref_ty,mutability) => {
                let ref_ty = ref_ty.sub(subs);
                vm.types.intern(TypeKind::Ref(ref_ty,*mutability),vm)
            }
            TypeKind::Ptr(ref_ty,mutability) => {
                let ref_ty = ref_ty.sub(subs);
                vm.types.intern(TypeKind::Ptr(ref_ty,*mutability),vm)
            }
            TypeKind::Tuple(fields) => {
                // fast path, will become redundant if above optimization is implemented
                if fields.len() == 0 {
                    return *self;
                }
                let new_fields = fields.iter().map(|field| {
                    field.sub(subs)
                }).collect();
                vm.types.intern(TypeKind::Tuple(new_fields), vm)
            }
            TypeKind::Adt(adt) => {
                // fast path, will become redundant if above optimization is implemented
                if adt.subs.len() == 0 {
                    return *self;
                }
                let new_subs = adt.subs.iter().map(|field| {
                    field.sub(subs)
                }).collect();
                let new_ty = TypeKind::Adt(ItemWithSubs{
                    item: adt.item,
                    subs: new_subs
                });
                vm.types.intern(new_ty, vm)
            }
            TypeKind::FunctionDef(func) => {
                // fast path, will become redundant if above optimization is implemented
                if func.subs.len() == 0 {
                    return *self;
                }
                let new_subs = func.subs.iter().map(|field| {
                    field.sub(subs)
                }).collect();
                let new_ty = TypeKind::FunctionDef(ItemWithSubs{
                    item: func.item,
                    subs: new_subs
                });
                vm.types.intern(new_ty, vm)
            }
            // These types never accept subs.
            TypeKind::Int(..) |
            TypeKind::Float(_) |
            TypeKind::Bool |
            TypeKind::Char => *self,
            _ => panic!("todo sub {:?} with {:?}",self,subs)
        }
    }

    pub fn add_impl(&self, crate_id: CrateId, children: Vec<(String,ItemId)>) {
        if self.kind().is_dummy() {
            //println!("skip impl {:?}",self.kind());
            return;
        }

        let mut impl_table = self.0.impl_table.write().unwrap();
        for (name,item_id) in children {
            let old = impl_table.insert(name,(crate_id,item_id));
            assert!(old.is_none());
        }
    }

    pub fn find_impl_member(&self, name: &str) -> Option<(CrateId,ItemId)> {
        let impl_table = self.0.impl_table.read().unwrap();
        impl_table.get(name).copied()
    }
}

impl<'vm> std::fmt::Debug for Type<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.kind.fmt(f)
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
                TypeKind::Str
            }

            TyKind::Adt(adt,subs) => {
                let item_with_subs = self.def_from_rustc(adt.did(),subs,ctx);
                TypeKind::Adt(item_with_subs)
            }
            TyKind::FnDef(did,subs) => {
                let item_with_subs = self.def_from_rustc(*did,subs,ctx);
                TypeKind::FunctionDef(item_with_subs)
            }
            TyKind::Alias(_,_) => {
                TypeKind::Alias
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

    pub fn subs_from_rustc<'tcx>(&'vm self, args: &[GenericArg<'tcx>], ctx: &RustCContext<'vm,'tcx>) -> Vec<Sub<'vm>> {
        args.iter().map(|s| {
            match s.unpack() {
                GenericArgKind::Type(ty) => Sub::Type(self.type_from_rustc(ty, ctx)),
                GenericArgKind::Lifetime(_) => Sub::Lifetime,
                GenericArgKind::Const(_) => Sub::Const,
            }
        }).collect()
    }

    fn path_from_rustc(in_path: &rustc_hir::definitions::DefPath) -> Option<ItemPath> {
        use rustc_hir::definitions::DefPathData;

        // we only handle trivial paths
        for elem in in_path.data.iter() {
            match elem.data {
                DefPathData::ValueNs(_) |
                DefPathData::TypeNs(_) => (),
                _ => return None
            }
        }

        if let Some(last_elem) = in_path.data.last() {
            match last_elem.data {
                DefPathData::ValueNs(_) => Some(ItemPath::new_value(in_path.to_string_no_crate_verbose())),
                _ => panic!("? {:?}",last_elem.data)
            }
        } else {
            panic!("zero element path?");
        }
    }

    fn def_from_rustc<'tcx>(&'vm self, did: DefId, args: &[GenericArg<'tcx>], ctx: &RustCContext<'vm,'tcx>) -> ItemWithSubs<'vm> {
        let item = if let Some(item) = ctx.items.find_by_did(did) {
            item
        } else {
            let def_path = ctx.tcx.def_path(did);
            if let Some(item_path) = Self::path_from_rustc(&def_path) {
                let trait_crate_id = ctx.items.find_crate_id(ctx.tcx, did.krate);
                let crate_items = ctx.vm.get_crate_items(trait_crate_id);
    
                if let Some(item) = crate_items.find_by_path(&item_path) {
                    item
                } else {
                    panic!("couldn't find item: {:?} / {:?}",item_path,def_path)
                }
            } else {
                if let Some(parent_impl) = ctx.tcx.impl_of_method(did) {
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
                    panic!("can't resolve path: {:?}",did);
                }
            }
        };

        let subs = self.subs_from_rustc(args, ctx);

        ItemWithSubs{
            item,
            subs
        }
    }

    fn intern(&'vm self, kind: TypeKind<'vm>, vm: &'vm VM<'vm>) -> Type<'vm> {
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

#[derive(Debug)]
struct InternedType<'vm> {
    kind: TypeKind<'vm>,
    layout: OnceLock<Layout>,
    impl_table: RwLock<AHashMap<String,(CrateId,ItemId)>>
}
