use std::fmt::Display;

use ahash::AHashMap;

use crate::{
    closure::Closure,
    items::{AdtInfo, AssocValue, CrateId, FunctionSig, Item},
};

use super::{
    layout::Layout,
    subs::{Sub, SubList},
    Type,
};

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum TypeKind<'vm> {
    Int(IntWidth, IntSign),
    Float(FloatWidth),
    Char,
    Bool,
    Never,

    Ref(Type<'vm>, Mutability),
    Ptr(Type<'vm>, Mutability),

    Tuple(Vec<Type<'vm>>),
    Array(Type<'vm>, ArraySize),
    Slice(Type<'vm>),
    StringSlice,

    FunctionDef(ItemWithSubs<'vm>),
    Adt(ItemWithSubs<'vm>),
    AssociatedType(ItemWithSubs<'vm>),

    // not properly implemented yet
    Foreign,
    Opaque,
    Dynamic,
    FunctionPointer(FunctionSig<'vm>),
    Closure(&'vm Closure<'vm>, SubList<'vm>),

    Param(u32),
    Unknown, //Error
}

impl<'vm> TypeKind<'vm> {
    pub fn is_dummy(&self) -> bool {
        match self {
            TypeKind::Dynamic => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum IntSign {
    Signed,
    Unsigned,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum IntWidth {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum FloatWidth {
    F32,
    F64,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum Mutability {
    Mut,
    Const,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct ItemWithSubs<'vm> {
    pub item: &'vm Item<'vm>,
    pub subs: SubList<'vm>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub enum ArraySize {
    Static(u32),
    ConstParam(u32),
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
    pub fn kind(&self) -> &'vm TypeKind<'vm> {
        &self.0.kind
    }

    pub fn layout(&self) -> &'vm Layout {
        self.0
            .layout
            .get_or_init(|| super::layout::Layout::from(*self))
    }

    pub fn sign(&self) -> IntSign {
        match self.kind() {
            TypeKind::Int(_, sign) => *sign,
            _ => IntSign::Unsigned,
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
            TypeKind::Slice(_) | TypeKind::StringSlice => false,
            _ => true,
        }
    }

    pub fn sub(&self, subs: &SubList<'vm>) -> Self {
        // do not do this, it just causes confusing bugs
        /*if subs.list.len() == 0 {
            return *self;
        }*/
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
            TypeKind::Ref(ref_ty, mutability) => {
                let ref_ty = ref_ty.sub(subs);
                vm.types.intern(TypeKind::Ref(ref_ty, *mutability), vm)
            }
            TypeKind::Ptr(ref_ty, mutability) => {
                let ref_ty = ref_ty.sub(subs);
                vm.types.intern(TypeKind::Ptr(ref_ty, *mutability), vm)
            }
            TypeKind::Tuple(fields) => {
                // fast path, will become redundant if above optimization is implemented
                if fields.len() == 0 {
                    return *self;
                }
                let new_fields = fields.iter().map(|field| field.sub(subs)).collect();
                vm.types.intern(TypeKind::Tuple(new_fields), vm)
            }
            TypeKind::Array(child_ty, size) => {
                // todo subs for const size
                size.assert_static();
                vm.types
                    .intern(TypeKind::Array(child_ty.sub(subs), *size), vm)
            }
            TypeKind::Slice(child_ty) => vm.types.intern(TypeKind::Slice(child_ty.sub(subs)), vm),
            TypeKind::Adt(adt) => {
                // fast path, will become redundant if above optimization is implemented
                if adt.subs.list.len() == 0 {
                    return *self;
                }
                let new_subs = adt.subs.sub(subs);
                let new_ty = TypeKind::Adt(ItemWithSubs {
                    item: adt.item,
                    subs: new_subs,
                });
                vm.types.intern(new_ty, vm)
            }
            TypeKind::FunctionDef(func) => {
                // fast path, will become redundant if above optimization is implemented
                if func.subs.list.len() == 0 {
                    return *self;
                }
                let new_subs = func.subs.sub(subs);
                let new_ty = TypeKind::FunctionDef(ItemWithSubs {
                    item: func.item,
                    subs: new_subs,
                });
                vm.types.intern(new_ty, vm)
            }
            TypeKind::AssociatedType(assoc_ty) => {
                // todo gadt's ???

                let new_subs = assoc_ty.subs.sub(subs);

                assoc_ty.item.resolve_associated_ty(&new_subs)
            }
            TypeKind::Closure(closure, closure_subs) => {
                let new_subs = closure_subs.sub(subs);

                vm.types.intern(TypeKind::Closure(closure, new_subs), vm)
            }
            TypeKind::FunctionPointer(_) => {
                assert!(subs.list.len() == 0);

                *self
            }
            // These types never accept subs.
            TypeKind::Never
            | TypeKind::Int(..)
            | TypeKind::Float(_)
            | TypeKind::Bool
            | TypeKind::StringSlice
            | TypeKind::Char => *self,
            _ => panic!("todo sub {:?} with {:?}", self, subs),
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self.kind() {
            // primitive types
            TypeKind::Int(..) | TypeKind::Float(..) | TypeKind::Bool | TypeKind::Char => true,
            TypeKind::Tuple(children) => children.iter().all(|child| child.is_concrete()),
            TypeKind::Adt(adt) => adt.subs.is_concrete(),
            TypeKind::FunctionDef(fun) => fun.subs.is_concrete(),
            TypeKind::Closure(_, subs) => subs.is_concrete(),
            TypeKind::Ref(child, _) => child.is_concrete(),
            TypeKind::Slice(child) => child.is_concrete(),
            TypeKind::FunctionPointer(_) => {
                println!("todo function pointer concrete?");
                true
            }
            TypeKind::Param(_) | TypeKind::Unknown => false,
            _ => panic!("is concrete? {}", self),
        }
    }

    pub fn is_interior_mut(&self) -> bool {
        match self.kind() {
            TypeKind::Int(..)
            | TypeKind::Float(..)
            | TypeKind::Bool
            | TypeKind::Char
            | TypeKind::StringSlice
            | TypeKind::FunctionDef(_) => false,
            TypeKind::Tuple(children) => children.iter().any(|child| child.is_interior_mut()),
            TypeKind::Array(child, _) => child.is_interior_mut(),
            TypeKind::Adt(adt) => {
                // TODO check unsafecell
                //let new_subs = adt.subs.sub(subs);
                let adt_info = adt.item.adt_info();

                for variant in &adt_info.variant_fields {
                    for ty in variant {
                        if ty.sub(&adt.subs).is_interior_mut() {
                            return true;
                        }
                    }
                }

                false
            }
            // assume refs are never interior mut, this is how explicit constants work
            TypeKind::Ref(..) => false,
            // how would we even handle this? assume not
            TypeKind::Slice(..) => false,
            _ => panic!("is_interior_mut? {}", self),
        }
    }

    pub fn set_impl(&self, new_assoc_items: AHashMap<String, (CrateId, AssocValue<'vm>)>) {
        let res = self.0.assoc_values.set(new_assoc_items);
        assert!(res.is_ok());
    }

    pub fn find_assoc_value(&self, name: &str) -> Option<(CrateId, AssocValue<'vm>)> {
        if let Some(assoc_values) = self.0.assoc_values.get() {
            assoc_values.get(name).cloned()
        } else {
            let impl_crate_id = self.find_impl_crate();
            let impl_crate = self.1.crate_provider(impl_crate_id);

            impl_crate.fill_inherent_impls(*self);

            // Try one more time:
            if let Some(assoc_values) = self.0.assoc_values.get() {
                assoc_values.get(name).cloned()
            } else {
                None
            }
        }
    }

    /// Find the crate that the type is defined in. Used to request inherent impl setup.
    fn find_impl_crate(&self) -> CrateId {
        match self.kind() {
            // primitive types have impls in core
            TypeKind::Int(..) => *self.1.core_crate.get().unwrap(),

            TypeKind::Adt(item) => item.item.crate_id,
            _ => panic!("find_impl_crate {:?}", self.kind()),
        }
    }

    pub fn adt_info(&self) -> &AdtInfo<'vm> {
        let TypeKind::Adt(ItemWithSubs{item,..}) = self.kind() else {
            panic!("adt_info: not an adt");
        };
        let info = item.adt_info();
        info
    }

    pub fn func_item(&self) -> Option<ItemWithSubs<'vm>> {
        match self.kind() {
            TypeKind::FunctionDef(item) => Some(item.clone()),
            _ => None,
        }
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
            TypeKind::Adt(item_with_subs) | TypeKind::FunctionDef(item_with_subs) => {
                write!(f, "{}", item_with_subs.item.path.as_string())?;
                if item_with_subs.subs.list.len() > 0 {
                    write!(f, "{}", item_with_subs.subs)?;
                }
                Ok(())
            }
            TypeKind::Tuple(children) => {
                write!(f, "(")?;
                for (i, ty) in children.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            }
            TypeKind::Int(width, sign) => match (width, sign) {
                (IntWidth::I8, IntSign::Signed) => write!(f, "i8"),
                (IntWidth::I16, IntSign::Signed) => write!(f, "i16"),
                (IntWidth::I32, IntSign::Signed) => write!(f, "i32"),
                (IntWidth::I64, IntSign::Signed) => write!(f, "i64"),
                (IntWidth::I128, IntSign::Signed) => write!(f, "i128"),
                (IntWidth::ISize, IntSign::Signed) => write!(f, "isize"),

                (IntWidth::I8, IntSign::Unsigned) => write!(f, "u8"),
                (IntWidth::I16, IntSign::Unsigned) => write!(f, "u16"),
                (IntWidth::I32, IntSign::Unsigned) => write!(f, "u32"),
                (IntWidth::I64, IntSign::Unsigned) => write!(f, "u64"),
                (IntWidth::I128, IntSign::Unsigned) => write!(f, "u128"),
                (IntWidth::ISize, IntSign::Unsigned) => write!(f, "usize"),
            },
            TypeKind::StringSlice => {
                write!(f, "str")
            }
            TypeKind::Ref(ty, mutability) => match mutability {
                Mutability::Const => write!(f, "&{}", ty),
                Mutability::Mut => write!(f, "&mut {}", ty),
            },
            TypeKind::Ptr(ty, mutability) => match mutability {
                Mutability::Const => write!(f, "*const {}", ty),
                Mutability::Mut => write!(f, "*mut {}", ty),
            },
            _ => {
                write!(f, "{:?}", self)
            }
        }
    }
}
