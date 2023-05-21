use std::{fmt::{Display, Write}, sync::{OnceLock, RwLock}};

use ahash::AHashMap;

use crate::{items::{Item, CrateId, ItemId}, vm::VM};

use super::{layout::Layout, Type, subs::{Sub, SubList}};

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum TypeKind<'vm> {
    Int(IntWidth,IntSign),
    Float(FloatWidth),
    Char,
    Bool,
    Never,

    Ref(Type<'vm>,Mutability),
    Ptr(Type<'vm>,Mutability),

    Tuple(Vec<Type<'vm>>),
    Array(Type<'vm>,ArraySize),
    Slice(Type<'vm>),
    Str,

    FunctionDef(ItemWithSubs<'vm>),
    Adt(ItemWithSubs<'vm>),
    AssociatedType(ItemWithSubs<'vm>),
    Foreign,
    Dynamic,
    FunctionPointer,

    Param(u32),
    Error
}

impl<'vm> TypeKind<'vm> {
    pub fn is_dummy(&self) -> bool {
        match self {
            TypeKind::Dynamic => true,
            _ => false
        }
    }
}

#[derive(Debug,Copy,Clone,Hash,PartialEq,Eq)]
pub enum IntSign {
    Signed,
    Unsigned
}

#[derive(Debug,Copy,Clone,Hash,PartialEq,Eq)]
pub enum IntWidth {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize
}

#[derive(Debug,Copy,Clone,Hash,PartialEq,Eq)]
pub enum FloatWidth {
    F32,
    F64
}

#[derive(Debug,Copy,Clone,Hash,PartialEq,Eq)]
pub enum Mutability {
    Mut,
    Const
}

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub struct ItemWithSubs<'vm> {
    pub item: &'vm Item<'vm>,
    pub subs: SubList<'vm>
}

#[derive(Debug,Hash,PartialEq,Eq,Clone)]
pub enum ArraySize {
    Static(u32),
    ConstParam(u32)
}

impl ArraySize {
    pub fn assert_static(&self) -> u32 {
        if let ArraySize::Static(n) = self {
            *n
        } else {
            panic!("expected static sized array, got const param");
        }
    }
}

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

    pub fn ref_to(&self, m: Mutability) -> Self {
        let ref_kind = TypeKind::Ref(*self, m);
        self.1.types.intern(ref_kind, self.1)
    }

    /// Check if a type is sized.
    /// This must exist because without it, we would need to recursively generate layouts,
    /// which is a non-terminating loop for self-referencing types.
    pub fn is_sized(&self) -> bool {
        match self.kind() {
            TypeKind::Slice(_) => false,
            _ => true
        }
    }

    pub fn sub(&self, subs: &SubList<'vm>) -> Self {
        if subs.list.len() == 0 {
            return *self;
        }
        // todo add a field which indicates whether a type accepts subs, fast return if not
        let vm = self.1;
        match self.kind() {
            TypeKind::Param(n) => {
                let sub = &subs.list[*n as usize];
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
                if adt.subs.list.len() == 0 {
                    return *self;
                }
                let new_subs = adt.subs.sub(subs);
                let new_ty = TypeKind::Adt(ItemWithSubs{
                    item: adt.item,
                    subs: new_subs
                });
                vm.types.intern(new_ty, vm)
            }
            TypeKind::FunctionDef(func) => {
                // fast path, will become redundant if above optimization is implemented
                if func.subs.list.len() == 0 {
                    return *self;
                }
                let new_subs = func.subs.sub(subs);
                let new_ty = TypeKind::FunctionDef(ItemWithSubs{
                    item: func.item,
                    subs: new_subs
                });
                vm.types.intern(new_ty, vm)
            }
            TypeKind::AssociatedType(assoc_ty) => {
                // todo gadt's ???

                let new_subs = assoc_ty.subs.sub(subs);

                assoc_ty.item.resolve_associated_ty(&new_subs)
            }
            // These types never accept subs.
            TypeKind::Int(..) |
            TypeKind::Float(_) |
            TypeKind::Bool |
            TypeKind::Char => *self,
            _ => panic!("todo sub {:?} with {:?}",self,subs)
        }
    }

    pub fn add_impl(&self, crate_id: CrateId, child_fn_items: Vec<(String,ItemId)>, child_tys: Vec<(String,Type<'vm>)>) {
        if self.kind().is_dummy() {
            //println!("skip impl {:?}",self.kind());
            return;
        }

        let mut impl_table = self.0.impl_table.write().unwrap();
        for (name,item_id) in child_fn_items {
            let old = impl_table.insert(name,(crate_id,item_id));
            assert!(old.is_none());
        }
    }

    pub fn find_impl_member(&self, name: &str) -> Option<(CrateId,ItemId)> {
        let impl_table = self.0.impl_table.read().unwrap();
        impl_table.get(name).copied()
    }

    pub fn get_adt_discriminator_ty(&self) -> Option<Type<'vm>> {
        let TypeKind::Adt(ItemWithSubs{item,subs}) = self.kind() else {
            panic!("get_adt_discriminator_ty: not an adt");
        };
        let info = item.get_adt_info();
        info.discriminator_ty
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

impl<'vm> Display for Type<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            TypeKind::Adt(item_with_subs) => {
                write!(f,"{}",item_with_subs.item.path.as_string())?;
                if item_with_subs.subs.len() > 0 {
                    write!(f,"{}",sub_list_to_string(&item_with_subs.subs))?;
                }
                Ok(())
            }
            TypeKind::Ref(ty,mutability) => {
                match mutability {
                    Mutability::Const => write!(f,"&{}",ty),
                    Mutability::Mut => write!(f,"&mut {}",ty),
                }
            }
            _ => {
                write!(f,"{:?}",self)
            }
        }
    }
}
