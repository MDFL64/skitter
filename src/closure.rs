use std::sync::{Arc, Mutex, OnceLock};

use ahash::AHashMap;

use crate::{
    ir::{FieldPattern, IRFunction, PatternKind},
    items::{CrateId, FunctionSig, ItemId},
    persist::Persist,
    types::{IntSign, IntWidth, Mutability, SubList, Type, TypeKind},
    vm::{Function, FunctionSource, VM},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[repr(u8)]
pub enum FnTrait {
    Fn,
    FnMut,
    FnOnce,
}

impl<'vm> Persist<'vm> for FnTrait {
    fn persist_read(reader: &mut crate::persist::PersistReader<'vm>) -> Self {
        panic!("read fn trait");
    }

    fn persist_write(&self, writer: &mut crate::persist::PersistWriter<'vm>) {
        writer.write_byte(*self as u8);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ClosureSig<'vm> {
    /// What is the most general trait we can implement?
    pub kind: FnTrait,
    /// The function pointer type we can (possibly) cast to. Still present even if casting is impossible.
    pub fn_ptr_ty: Type<'vm>,
    /// A tuple representing the captured upvars
    pub env_ty: Type<'vm>,
}

impl<'vm> ClosureSig<'vm> {
    pub fn from_rustc_sub_repr(subs: &SubList<'vm>) -> (Self, SubList<'vm>) {
        let mut subs = subs.clone();

        let env_ty = subs
            .list
            .pop()
            .expect("closure subs are too short")
            .assert_ty();
        let fn_ptr_ty = subs
            .list
            .pop()
            .expect("closure subs are too short")
            .assert_ty();
        let kind_disc = subs
            .list
            .pop()
            .expect("closure subs are too short")
            .assert_ty();

        // rustc uses a moderately insane repr for closures,
        // the most insane part being an integer type to indicate the kind of closure

        let kind = match kind_disc.kind() {
            TypeKind::Int(IntWidth::I8, IntSign::Signed) => FnTrait::Fn,
            TypeKind::Int(IntWidth::I16, IntSign::Signed) => FnTrait::FnMut,
            TypeKind::Int(IntWidth::I32, IntSign::Signed) => FnTrait::FnOnce,
            _ => panic!("invalid closure kind"),
        };

        let sig = ClosureSig {
            kind,
            fn_ptr_ty,
            env_ty,
        };

        (sig, subs)
    }
}

/// Plays a similar role to function items. Contains IR and a table of monomorphizations.
/// FUTURE CONSIDERATIONS: Unlike function items, the IR cannot be mutated once set.
/// Closures will NOT support hot loading. Functions will create new closures on being hot loaded.
pub struct Closure<'vm> {
    pub vm: &'vm VM<'vm>,
    unique_id: u32,

    // the following fields are used to locate closures in foreign crates
    // they should NOT be used when interning closures, since multiple closures
    // may exist for the same (crate,item,indices) key
    pub def_crate: CrateId,
    pub def_item: ItemId,
    pub def_path_indices: Vec<u32>,

    abstract_sig: OnceLock<ClosureSig<'vm>>,

    ir_base: OnceLock<Arc<IRFunction<'vm>>>,
    ir_fn: OnceLock<Arc<IRFunction<'vm>>>,
    ir_mut: OnceLock<Arc<IRFunction<'vm>>>,
    ir_once: OnceLock<Arc<IRFunction<'vm>>>,

    mono_instances: Mutex<AHashMap<SubList<'vm>, &'vm Function<'vm>>>,
}

impl<'vm> Closure<'vm> {
    pub fn new(
        unique_id: u32,
        def_crate: CrateId,
        def_item: ItemId,
        def_path_indices: Vec<u32>,
        vm: &'vm VM<'vm>,
    ) -> Self {
        Self {
            vm,
            unique_id,

            def_crate,
            def_item,
            def_path_indices,

            abstract_sig: Default::default(),

            ir_base: Default::default(),
            ir_fn: Default::default(),
            ir_mut: Default::default(),
            ir_once: Default::default(),

            mono_instances: Default::default(),
        }
    }

    pub fn ir_base(&self) -> Arc<IRFunction<'vm>> {
        self.ir_base.get().expect("no ir for closure").clone()
    }

    pub fn set_ir_base(&self, ir: IRFunction<'vm>) {
        self.ir_base.set(Arc::new(ir)).ok();
    }

    pub fn abstract_sig(&self) -> ClosureSig<'vm> {
        self.abstract_sig.get().expect("no sig for closure").clone()
    }

    pub fn set_abstract_sig(&self, sig: ClosureSig<'vm>) {
        self.abstract_sig.set(sig).ok();
    }

    pub fn ir_for_trait(&self, kind: FnTrait, self_ty: Type<'vm>) -> Arc<IRFunction<'vm>> {
        match kind {
            FnTrait::Fn => self
                .ir_fn
                .get_or_init(|| {
                    Arc::new(build_ir_for_trait(self.vm, &self.ir_base(), kind, self_ty))
                })
                .clone(),
            FnTrait::FnMut => self
                .ir_mut
                .get_or_init(|| {
                    Arc::new(build_ir_for_trait(self.vm, &self.ir_base(), kind, self_ty))
                })
                .clone(),
            FnTrait::FnOnce => self
                .ir_once
                .get_or_init(|| {
                    Arc::new(build_ir_for_trait(self.vm, &self.ir_base(), kind, self_ty))
                })
                .clone(),
        }
    }

    /// Get a monomorphic VM function from a function item.
    pub fn func_mono(&'vm self, subs: &SubList<'vm>) -> &'vm Function<'vm> {
        let mut mono_instances = self.mono_instances.lock().unwrap();
        let result_func = mono_instances.entry(subs.clone()).or_insert_with(|| {
            self.vm
                .alloc_function(FunctionSource::Closure(self), subs.clone())
        });

        result_func
    }
}

impl<'vm> std::fmt::Debug for Closure<'vm> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Closure")
    }
}

impl<'vm> std::hash::Hash for Closure<'vm> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u32(self.unique_id)
    }
}

impl<'vm> PartialEq for Closure<'vm> {
    fn eq(&self, other: &Self) -> bool {
        self.unique_id == other.unique_id
    }
}

impl<'vm> Eq for Closure<'vm> {}

/// We make the following transformations to the IR:
///
/// 1. Set the `closure_kind`.
/// 2. Replace the signature's `(args)` with `(self,(args))`
/// 3. Replace the param pattern's `(args)` with `(self,(args))`
fn build_ir_for_trait<'vm>(
    vm: &'vm VM<'vm>,
    ir_in: &IRFunction<'vm>,
    kind: FnTrait,
    self_ty: Type<'vm>,
) -> IRFunction<'vm> {
    let mut new_ir = ir_in.clone_ir();

    new_ir.closure_kind = Some(kind);

    let self_ty = match kind {
        FnTrait::Fn => self_ty.ref_to(Mutability::Const),
        FnTrait::FnMut => self_ty.ref_to(Mutability::Mut),
        FnTrait::FnOnce => self_ty,
        _ => panic!("todo self ty"),
    };

    let args_tuple = vm.ty_tuple(ir_in.sig.inputs.clone());

    new_ir.sig.inputs = vec![self_ty, args_tuple];

    let self_pattern = new_ir.insert_pattern(PatternKind::Hole, self_ty);

    let param_fields: Vec<_> = new_ir
        .params
        .iter()
        .enumerate()
        .map(|(index, id)| FieldPattern {
            field: index as u32,
            pattern: *id,
        })
        .collect();

    let args_pattern = new_ir.insert_pattern(
        PatternKind::Struct {
            fields: param_fields,
        },
        args_tuple,
    );

    new_ir.params = vec![self_pattern, args_pattern];

    new_ir
}
