use std::fmt::Display;

use crate::{
    closure::ClosureRef,
    items::{AdtInfo, CrateId, FunctionSig, Item, ItemId},
    vm::VM,
};

use super::{
    layout::Layout,
    subs::{Sub, SubList},
    ConstGeneric, Type,
};

use skitter_macro::Persist;

#[derive(Debug, Hash, PartialEq, Eq, Clone, Persist)]
pub enum TypeKind<'vm> {
    Int(IntWidth, IntSign),
    Float(FloatWidth),
    Char,
    Bool,
    Never,

    Ref(Type<'vm>, Mutability),
    Ptr(Type<'vm>, Mutability),

    Tuple(Vec<Type<'vm>>),
    Array(Type<'vm>, ConstGeneric),
    Slice(Type<'vm>),
    StringSlice,

    FunctionDef(ItemWithSubs<'vm>),
    Adt(ItemWithSubs<'vm>),
    AssociatedType(ItemWithSubs<'vm>),

    // stores it's own def path
    // these are implemented as items in rustc
    // we currently skip them, but it might be nicer just to have consistency with most other types
    Foreign(CrateId, &'vm str),

    Opaque(ItemWithSubs<'vm>, &'vm str),
    FunctionPointer(FunctionSig<'vm>),
    Closure(ClosureRef<'vm>, SubList<'vm>),

    // not properly implemented yet
    Dynamic {
        primary_trait: Option<ItemWithSubs<'vm>>,
        auto_traits: AutoTraitSet,
        is_dyn_star: bool,
    },

    Param(u32),
    Unknown, //Error
}
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, Persist)]

pub enum IntSign {
    Signed,
    Unsigned,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, Persist)]
pub enum IntWidth {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, Persist)]
pub enum FloatWidth {
    F32,
    F64,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, Persist)]
pub enum Mutability {
    Mut,
    Const,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct ItemWithSubs<'vm> {
    pub item: &'vm Item<'vm>,
    pub subs: SubList<'vm>,
}

impl<'vm> Display for ItemWithSubs<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.item.path.as_string(), self.subs)
    }
}

impl<'vm> Type<'vm> {
    pub fn kind(&self) -> &'vm TypeKind<'vm> {
        &self.0.kind
    }

    pub fn vm(&self) -> &'vm VM<'vm> {
        self.1
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
            TypeKind::Dynamic { is_dyn_star, .. } => {
                assert!(!is_dyn_star);
                false
            }
            // unsized structs:
            TypeKind::Adt(item) => {
                let adt_info = item.item.adt_info();
                if adt_info.is_struct() {
                    let fields = adt_info.variant_fields.assert_single();
                    if let Some(field) = fields.last() {
                        let end_ty = field.sub(&item.subs);
                        return end_ty.is_sized();
                    }
                }
                true
            }
            _ => true,
        }
    }

    pub fn is_dyn_receiver(&self, trait_crate: CrateId, trait_id: ItemId) -> bool {
        if let TypeKind::Dynamic {
            primary_trait,
            is_dyn_star,
            ..
        } = self.kind()
        {
            let primary_trait = primary_trait.as_ref().expect("no primary trait");
            assert!(!is_dyn_star);
            if primary_trait.item.crate_id == trait_crate && primary_trait.item.item_id == trait_id
            {
                return true;
            }
        }
        /*match self.kind() {
            TypeKind::Ref(child,_) => {
                if let TypeKind::Dynamic { primary_trait, is_dyn_star, .. } = child.kind() {
                    assert!(!is_dyn_star);
                    if primary_trait.item == for_trait {
                        return true;
                    }
                }
            }
            _ => ()
        }*/
        false
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
                let size = size.sub(subs, vm.common_types().usize);
                vm.types
                    .intern(TypeKind::Array(child_ty.sub(subs), size), vm)
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

                vm.types.intern(TypeKind::Closure(*closure, new_subs), vm)
            }
            TypeKind::FunctionPointer(sig) => {
                let new_sig = sig.sub(subs);

                vm.types.intern(TypeKind::FunctionPointer(new_sig), vm)
            }

            TypeKind::Dynamic {
                primary_trait,
                is_dyn_star,
                ..
            } => {
                let primary_trait = primary_trait.as_ref().expect("no primary trait");
                assert!(!is_dyn_star);
                assert!(primary_trait.subs.list.len() == 0);

                *self
            }

            TypeKind::Opaque(item_with_subs, full_path) => {
                let new_subs = item_with_subs.subs.sub(subs);

                //println!("new subs {}",new_subs);

                let (ir, ir_subs) = item_with_subs.item.ir(&new_subs);

                for candidate in ir.opaque_types.iter() {
                    // TODO is this always true?
                    assert!(candidate.source_item.item == item_with_subs.item);

                    if candidate.source_full_path == *full_path {
                        let final_subs = candidate.source_item.subs.sub(&ir_subs);

                        // TODO just use new_subs if this assumption is correct
                        assert_eq!(final_subs, new_subs);

                        let res = candidate.destination_ty.sub(&final_subs);
                        //println!("res = {}",res);
                        return res;
                    }
                }

                panic!("failed to lookup opaque type");
            }

            // These types never accept subs.
            TypeKind::Never
            | TypeKind::Int(..)
            | TypeKind::Float(_)
            | TypeKind::Bool
            | TypeKind::StringSlice
            | TypeKind::Foreign(..)
            | TypeKind::Char => *self,
            _ => panic!("todo sub {:?} with {:?}", self, subs),
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self.kind() {
            // primitive types
            TypeKind::Int(..)
            | TypeKind::Float(..)
            | TypeKind::Bool
            | TypeKind::Char
            | TypeKind::StringSlice => true,
            TypeKind::Tuple(children) => children.iter().all(|child| child.is_concrete()),
            TypeKind::Adt(adt) => adt.subs.is_concrete(),
            TypeKind::FunctionDef(fun) => fun.subs.is_concrete(),
            TypeKind::Closure(_, subs) => subs.is_concrete(),
            TypeKind::Dynamic { primary_trait, .. } => {
                if let Some(primary_trait) = primary_trait {
                    primary_trait.subs.is_concrete()
                } else {
                    true
                }
            }

            TypeKind::Ref(child, _) | TypeKind::Ptr(child, _) | TypeKind::Slice(child) => {
                child.is_concrete()
            }

            TypeKind::FunctionPointer(sig) => {
                let args_concrete = sig.inputs.iter().all(|arg| arg.is_concrete());
                args_concrete && sig.output.is_concrete()
            }
            TypeKind::Array(child, size) => child.is_concrete() && size.is_concrete(),
            TypeKind::Param(_) | TypeKind::Unknown => false,
            TypeKind::AssociatedType(_) => false,
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
            | TypeKind::FunctionDef(_)
            | TypeKind::FunctionPointer(_) => false,
            TypeKind::Tuple(children) => children.iter().any(|child| child.is_interior_mut()),
            TypeKind::Closure(closure, subs) => {
                let env = closure.env(subs);
                assert!(env.is_concrete());
                env.is_interior_mut()
            }
            TypeKind::Array(child, _) => child.is_interior_mut(),
            TypeKind::Adt(adt) => {
                // TODO check unsafecell
                //let new_subs = adt.subs.sub(subs);
                let adt_info = adt.item.adt_info();

                for (_, fields) in adt_info.variant_fields.iter() {
                    for ty in fields {
                        if ty.sub(&adt.subs).is_interior_mut() {
                            return true;
                        }
                    }
                }

                false
            }
            // assume refs are never interior mut, this is how explicit constants work
            TypeKind::Ref(..) => false,
            TypeKind::Ptr(..) => false,
            // how would we even handle this? assume not
            TypeKind::Slice(..) => false,
            TypeKind::Foreign(..) => false,
            // TODO? assume true?
            TypeKind::Dynamic { .. } => true,
            _ => panic!("is_interior_mut? {}", self),
        }
    }

    pub fn impl_key(&self) -> Option<&'vm str> {
        match self.kind() {
            TypeKind::Bool => Some("@bool"),
            TypeKind::Char => Some("@char"),
            TypeKind::Int(..) => Some("@int"),
            TypeKind::Float(_) => Some("@float"),
            TypeKind::Never => Some("@never"),

            TypeKind::StringSlice => Some("@str"),
            TypeKind::Array(..) => Some("@array"),
            TypeKind::Slice(_) => Some("@slice"),
            TypeKind::Tuple(..) => Some("@tuple"),
            TypeKind::Ptr(..) => Some("@ptr"),
            // TODO impls on refs may be common enough to special-case
            TypeKind::Ref(..) => Some("@ref"),
            TypeKind::Dynamic { .. } => Some("@dyn"),
            TypeKind::FunctionPointer(..) => Some("@fn-ptr"),

            TypeKind::Adt(item) => Some(item.item.path.as_string()),

            // impls on closures shouldn't be possible
            TypeKind::Closure(..) | TypeKind::Param(_) => None,

            _ => panic!("todo impl key: {}", self),
        }
    }

    pub fn adt_info(&self) -> &AdtInfo<'vm> {
        let TypeKind::Adt(ItemWithSubs { item, .. }) = self.kind() else {
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

    /// Attempts to re-interpret this type as a reference or pointer, and get the referenced type.
    ///
    /// DO NOT USE if you just want to get the referenced type from a reference or pointer.
    pub fn try_get_ref_ty(&self) -> Option<Type<'vm>> {
        match self.kind() {
            TypeKind::Ptr(ref_ty, _) | TypeKind::Ref(ref_ty, _) => Some(*ref_ty),
            TypeKind::Adt(adt_item) => {
                let adt_info = adt_item.item.adt_info();
                if adt_info.is_struct() {
                    if let Some(field) = adt_info.variant_fields.assert_single().first() {
                        let field_ty = field.sub(&adt_item.subs);
                        return field_ty.try_get_ref_ty();
                    }
                }
                None
            }
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
            TypeKind::Slice(child) => {
                write!(f, "[{}]", child)
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
            TypeKind::Float(width) => match width {
                FloatWidth::F32 => write!(f, "f32"),
                FloatWidth::F64 => write!(f, "f64"),
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

#[derive(Debug, Clone, Eq, PartialEq, Hash, Persist)]
pub struct AutoTraitSet(u16);

impl AutoTraitSet {
    pub const EMPTY: AutoTraitSet = AutoTraitSet(0);
    pub const SEND: AutoTraitSet = AutoTraitSet(1);
    pub const SYNC: AutoTraitSet = AutoTraitSet(2);

    pub fn add(&mut self, other: Self) {
        self.0 |= other.0;
    }
}
