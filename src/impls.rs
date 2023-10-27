use std::{hash::Hash, sync::Arc};

use ahash::AHashMap;

use crate::{
    crate_provider::TraitImpl,
    items::{AssocValue, BoundKind, CrateId, GenericCounts, Item, ItemId},
    lazy_collections::{LazyArray, LazyItem, LazyKey, LazyTable},
    persist::{PersistReader, PersistWriter},
    types::{ConstGeneric, Sub, SubList, Type, TypeKind},
    vm::VM,
};

use skitter_macro::Persist;

/// Find candidate crates for a trait impl on a list of types.
///
/// TODO smallvec
pub fn find_trait_impl_crate(for_tys: &SubList, trait_crate: CrateId) -> Vec<CrateId> {
    let mut res = vec![trait_crate];

    for sub in for_tys.list.iter() {
        match sub {
            Sub::Type(ty) => {
                let loc = find_source_crate(*ty);
                match loc {
                    ImplLocation::Crate(cid) => {
                        if !res.contains(&cid) {
                            res.push(cid);
                        }
                    }
                    ImplLocation::Internal => {
                        let cid_list = get_internal_crates(ty.vm());
                        for cid in cid_list {
                            if !res.contains(&cid) {
                                res.push(cid);
                            }
                        }
                    }
                }
            }
            _ => (),
        }
    }

    res
}

/// Find candidate crates for an inherent impl on a type.
///
/// TODO smallvec
pub fn find_inherent_impl_crate(ty: Type) -> Vec<CrateId> {
    match find_source_crate(ty) {
        ImplLocation::Crate(cid) => {
            vec![cid]
        }
        ImplLocation::Internal => get_internal_crates(ty.vm()).into(),
    }
}

fn get_internal_crates(vm: &VM) -> [CrateId; 2] {
    [
        *vm.core_crate.get().unwrap(),
        *vm.alloc_crate.get().unwrap(),
        // TODO STD
    ]
}

enum ImplLocation {
    Crate(CrateId),
    Internal,
}

fn find_source_crate(ty: Type) -> ImplLocation {
    match ty.kind() {
        TypeKind::Adt(item) |
        TypeKind::FunctionDef(item) => ImplLocation::Crate(item.item.crate_id),

        TypeKind::Ref(child, _) => find_source_crate(*child),

        TypeKind::Bool
        | TypeKind::Char
        | TypeKind::Ptr(..) // TODO is this correct?
        | TypeKind::Closure(..) // TODO is this correct?
        | TypeKind::Float(_)
        | TypeKind::Int(..)
        | TypeKind::Tuple(_)
        | TypeKind::StringSlice
        | TypeKind::Slice(_)
        | TypeKind::Array(..) => ImplLocation::Internal,
        _ => panic!("source crate {}", ty),
    }
}

pub trait ImplTable<'vm> {
    fn get_crate_id(&self) -> CrateId;

    fn get_bounds(&self, i: u32) -> &ImplBounds<'vm>;

    fn get_inherent_table(&self, full_key: &str) -> Option<&[InherentMember<'vm>]>;

    fn get_trait_table(
        &self,
        trait_key: &TraitKey,
        type_key: Option<&'vm str>,
    ) -> Option<&[TraitValue<'vm>]>;

    fn find_inherent(&self, full_key: &str, ty: Type<'vm>) -> Option<AssocValue<'vm>> {
        if let Some(list) = self.get_inherent_table(full_key) {
            for value in list {
                let bounds = self.get_bounds(value.bounds_id);
                if bounds.check_inherent(ty, ty.vm()) {
                    return Some(value.value.clone());
                }
            }
        }
        None
    }

    fn find_inherent_raw(&self, full_key: &str) -> Option<AssocValue<'vm>> {
        if let Some(list) = self.get_inherent_table(full_key) {
            if list.len() == 1 {
                return Some(list[0].value.clone());
            }
        }
        None
    }

    fn find_trait(&self, trait_item: &Item<'vm>, for_tys: &SubList<'vm>) -> Option<TraitImpl<'vm>> {
        let trait_key = TraitKey {
            crate_id: trait_item.crate_id,
            item_id: trait_item.item_id,
        };

        let type_key = for_tys.list[0].assert_ty().impl_key();

        if let Some(list) = self.get_trait_table(&trait_key, type_key) {
            for value in list {
                let bounds = self.get_bounds(value.bounds_id);
                if let Some(impl_subs) = bounds.check(for_tys, trait_item.vm) {
                    return Some(TraitImpl {
                        crate_id: self.get_crate_id(),
                        assoc_values: value.values.clone(),
                        impl_subs,
                    });
                }
            }
        }

        // retry with a 'None' key
        if type_key != None {
            if let Some(list) = self.get_trait_table(&trait_key, None) {
                for value in list {
                    let bounds = self.get_bounds(value.bounds_id);
                    if let Some(impl_subs) = bounds.check(for_tys, trait_item.vm) {
                        return Some(TraitImpl {
                            crate_id: self.get_crate_id(),
                            assoc_values: value.values.clone(),
                            impl_subs,
                        });
                    }
                }
            }
        }

        None
    }
}

#[derive(Debug, Persist)]
pub struct ImplBounds<'vm> {
    pub for_tys: SubList<'vm>,
    pub bounds: Vec<BoundKind<'vm>>,
    pub generic_counts: GenericCounts,
}

impl<'vm> LazyItem<'vm> for ImplBounds<'vm> {
    type Input = Self;

    fn build(input: Self::Input, _: &'vm VM<'vm>) -> Self {
        input
    }
}

pub struct ImplTableSimple<'vm> {
    crate_id: CrateId,
    inherent_table: AHashMap<String, Vec<InherentMember<'vm>>>,
    trait_table: AHashMap<TraitKey, AHashMap<Option<&'vm str>, Vec<TraitValue<'vm>>>>,
    bounds_table: Vec<ImplBounds<'vm>>,
}

#[derive(Debug, Clone, Persist)]
pub struct InherentMember<'vm> {
    bounds_id: u32,
    value: AssocValue<'vm>,
}

#[derive(PartialEq, Eq, Clone, Debug, Persist)]
pub struct TraitKey {
    crate_id: CrateId,
    item_id: ItemId,
}

impl Hash for TraitKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.crate_id.index());
        state.write_usize(self.item_id.index());
    }
}

#[derive(Clone, Debug, Persist)]
pub struct TraitValue<'vm> {
    bounds_id: u32,
    values: Arc<[Option<AssocValue<'vm>>]>,
}

#[derive(Debug, Persist)]
struct TraitKeyValue<'vm> {
    key: (TraitKey, Option<&'vm str>),
    value: Vec<TraitValue<'vm>>,
}

impl<'vm> LazyItem<'vm> for TraitKeyValue<'vm> {
    type Input = Self;

    fn build(input: Self::Input, _: &'vm VM<'vm>) -> Self {
        input
    }
}

impl<'vm> LazyKey<'vm> for TraitKeyValue<'vm> {
    type Key = (TraitKey, Option<&'vm str>);

    fn key(&self) -> Option<&Self::Key> {
        Some(&self.key)
    }

    fn key_for_input(input: &Self::Input) -> Option<&Self::Key> {
        Some(&input.key)
    }
}

#[derive(Debug, Persist)]
struct InherentKeyValue<'vm> {
    key: String,
    list: Vec<InherentMember<'vm>>,
}

impl<'vm> LazyItem<'vm> for InherentKeyValue<'vm> {
    type Input = Self;

    fn build(input: Self::Input, _: &'vm VM<'vm>) -> Self {
        input
    }
}

impl<'vm> LazyKey<'vm> for InherentKeyValue<'vm> {
    type Key = String;

    fn key(&self) -> Option<&Self::Key> {
        Some(&self.key)
    }

    fn key_for_input(input: &Self::Input) -> Option<&Self::Key> {
        Some(&input.key)
    }
}

impl<'vm> ImplTableSimple<'vm> {
    pub fn new(crate_id: CrateId) -> Self {
        Self {
            crate_id,
            inherent_table: Default::default(),
            trait_table: Default::default(),
            bounds_table: Default::default(),
        }
    }

    pub fn add_bounds(&mut self, bounds: ImplBounds<'vm>) -> u32 {
        let index = self.bounds_table.len() as u32;
        self.bounds_table.push(bounds);
        index
    }

    pub fn add_inherent(&mut self, key: String, bounds_id: u32, value: AssocValue<'vm>) {
        let list = self.inherent_table.entry(key).or_default();
        list.push(InherentMember { bounds_id, value });
    }

    pub fn add_trait(
        &mut self,
        trait_item: &Item,
        key: Option<&'vm str>,
        bounds_id: u32,
        assoc_values: Vec<Option<AssocValue<'vm>>>,
    ) {
        let trait_key = TraitKey {
            crate_id: trait_item.crate_id,
            item_id: trait_item.item_id,
        };

        let sub_table = self.trait_table.entry(trait_key).or_default();
        let list = sub_table.entry(key).or_default();

        list.push(TraitValue {
            bounds_id,
            values: assoc_values.into(),
        });
    }

    pub fn write(&self, writer: &mut PersistWriter<'vm>) {
        LazyArray::<ImplBounds<'vm>>::write(writer, self.bounds_table.iter());

        // what a disaster
        let trait_pairs: Vec<_> = self
            .trait_table
            .iter()
            .flat_map(|(key_1, sub_table)| {
                sub_table.iter().map(|(key_2, val)| TraitKeyValue {
                    key: (key_1.clone(), *key_2),
                    value: val.clone(),
                })
            })
            .collect();

        LazyTable::<TraitKeyValue>::write(writer, trait_pairs.iter());

        let inherent_pairs: Vec<_> = self
            .inherent_table
            .iter()
            .map(|(k, v)| InherentKeyValue {
                key: k.clone(),
                list: v.clone(),
            })
            .collect();

        LazyTable::<InherentKeyValue>::write(writer, inherent_pairs.iter());
    }
}

impl<'vm> ImplTable<'vm> for ImplTableSimple<'vm> {
    fn get_crate_id(&self) -> CrateId {
        self.crate_id
    }

    fn get_bounds(&self, i: u32) -> &ImplBounds<'vm> {
        &self.bounds_table[i as usize]
    }

    fn get_inherent_table(&self, full_key: &str) -> Option<&[InherentMember<'vm>]> {
        if let Some(val) = self.inherent_table.get(full_key) {
            Some(val)
        } else {
            None
        }
    }

    fn get_trait_table(
        &self,
        trait_key: &TraitKey,
        type_key: Option<&'vm str>,
    ) -> Option<&[TraitValue<'vm>]> {
        if let Some(sub_table) = self.trait_table.get(trait_key) {
            if let Some(list) = sub_table.get(&type_key) {
                return Some(list);
            }
        }
        None
    }
}

pub struct ImplTableLazy<'vm> {
    crate_id: CrateId,
    bounds_table: LazyArray<'vm, ImplBounds<'vm>>,
    trait_table: LazyTable<'vm, TraitKeyValue<'vm>>,
    inherent_table: LazyTable<'vm, InherentKeyValue<'vm>>,
}

impl<'vm> ImplTableLazy<'vm> {
    pub fn new(reader: &mut PersistReader<'vm>) -> Self {
        let bounds_table = LazyArray::read(reader);
        let trait_table = LazyTable::read(reader);
        let inherent_table = LazyTable::read(reader);

        Self {
            crate_id: reader.context.this_crate,
            bounds_table,
            trait_table,
            inherent_table,
        }
    }
}

impl<'vm> ImplTable<'vm> for ImplTableLazy<'vm> {
    fn get_bounds(&self, i: u32) -> &ImplBounds<'vm> {
        self.bounds_table.get(i as usize)
    }

    fn get_crate_id(&self) -> CrateId {
        self.crate_id
    }

    fn get_trait_table(
        &self,
        trait_key: &TraitKey,
        type_key: Option<&'vm str>,
    ) -> Option<&[TraitValue<'vm>]> {
        let key = (trait_key.clone(), type_key);

        self.trait_table.get(&key).map(|kv| kv.value.as_slice())
    }

    fn get_inherent_table(&self, full_key: &str) -> Option<&[InherentMember<'vm>]> {
        self.inherent_table
            .get(full_key)
            .map(|kv| kv.list.as_slice())
    }
}

impl<'vm> ImplBounds<'vm> {
    pub fn check(&self, for_tys: &SubList<'vm>, vm: &'vm VM<'vm>) -> Option<SubList<'vm>> {
        let mut sub_map = SubMap::new(self.generic_counts.clone(), vm);
        if Self::match_subs(&self.for_tys, for_tys, &mut sub_map) {
            // this is an ATTEMPT to deal with GATs -- it may not be entirely correct
            if for_tys.list.len() > self.for_tys.list.len() {
                let result_subs = sub_map.get_result();

                let start = self.for_tys.list.len();
                let end = for_tys.list.len();
                for i in start..end {
                    let sub = for_tys.list[i].clone();
                    result_subs.list.push(sub);
                }
            }

            for bound in self.bounds.iter() {
                match bound {
                    BoundKind::Trait(trait_bound) => {
                        let types_to_check = trait_bound.subs.sub(sub_map.get_result());

                        let has_impl = trait_bound.item.trait_has_impl(&types_to_check); // &mut Some(&mut result_subs));

                        if !has_impl {
                            return None;
                        }
                    }
                    BoundKind::Projection(assoc_ty, eq_ty) => {
                        let types_to_check = assoc_ty.subs.sub(sub_map.get_result());

                        let resolved_assoc_ty =
                            assoc_ty.item.resolve_associated_ty(&types_to_check);

                        if !Self::match_types(*eq_ty, resolved_assoc_ty, &mut sub_map) {
                            panic!("unmatched {} = {}", resolved_assoc_ty, eq_ty);
                        }
                    }
                }
            }

            Some(sub_map.result_subs)
        } else {
            None
        }
    }

    /// This relaxed check method tests against a single type,
    /// and does not actually check the stored bounds.
    /// TODO: It may be necessary to check the bounds???
    pub fn check_inherent(&self, ty: Type<'vm>, vm: &'vm VM<'vm>) -> bool {
        let mut sub_map = SubMap::new(self.generic_counts.clone(), vm);

        let check_ty = self.for_tys.list[0].assert_ty();

        Self::match_types(ty, check_ty, &mut sub_map)
    }

    fn match_subs(lhs: &SubList<'vm>, rhs: &SubList<'vm>, res_map: &mut SubMap<'vm>) -> bool {
        // leave the length unchecked, GAT lookups may have differing lengths
        for pair in lhs.list.iter().zip(&rhs.list) {
            match pair {
                (Sub::Type(lhs_ty), Sub::Type(rhs_ty)) => {
                    if !Self::match_types(*lhs_ty, *rhs_ty, res_map) {
                        return false;
                    }
                }
                (Sub::Const(lhs_ty, lhs_const), Sub::Const(rhs_ty, rhs_const)) => {
                    assert!(lhs_ty.is_concrete() && lhs_ty == rhs_ty);
                    if !Self::match_const(lhs_const, rhs_const, res_map, *lhs_ty) {
                        return false;
                    }
                }
                _ => {
                    if pair.0 != pair.1 {
                        return false;
                    }
                }
            }
        }
        true
    }

    fn match_types(lhs: Type<'vm>, rhs: Type<'vm>, res_map: &mut SubMap<'vm>) -> bool {
        if lhs.is_concrete() && lhs == rhs {
            return true;
        }

        match (lhs.kind(), rhs.kind()) {
            (TypeKind::Adt(a), TypeKind::Adt(b)) => {
                (a.item == b.item) && Self::match_subs(&a.subs, &b.subs, res_map)
            }
            (TypeKind::Ptr(in_ref, in_mut), TypeKind::Ptr(trait_ref, trait_mut)) => {
                (in_mut == trait_mut) && Self::match_types(*in_ref, *trait_ref, res_map)
            }
            (TypeKind::Ref(in_ref, in_mut), TypeKind::Ref(trait_ref, trait_mut)) => {
                (in_mut == trait_mut) && Self::match_types(*in_ref, *trait_ref, res_map)
            }
            (TypeKind::Slice(in_elem), TypeKind::Slice(trait_elem)) => {
                Self::match_types(*in_elem, *trait_elem, res_map)
            }
            (TypeKind::Tuple(lhs_children), TypeKind::Tuple(rhs_children)) => {
                if lhs_children.len() != rhs_children.len() {
                    false
                } else {
                    for (lhs_child, rhs_child) in lhs_children.iter().zip(rhs_children) {
                        if !Self::match_types(*lhs_child, *rhs_child, res_map) {
                            return false;
                        }
                    }
                    true
                }
            }
            (TypeKind::Array(lhs_child, lhs_size), TypeKind::Array(rhs_child, rhs_size)) => {
                let ty_usize = lhs.vm().common_types().usize;
                Self::match_types(*lhs_child, *rhs_child, res_map)
                    && Self::match_const(lhs_size, rhs_size, res_map, ty_usize)
            }

            /*(TypeKind::Param(lhs_param), TypeKind::Param(rhs_param)) => {
                panic!("bidirectional bound not permitted");
                /*if res_map.allow_bidirectional {
                    res_map.set(SubSide::Rhs, *rhs_param, lhs);
                    res_map.set(SubSide::Lhs, *lhs_param, rhs);
                    true
                } else {
                    panic!("bidirectional bound not permitted");
                }*/
            }*/
            (TypeKind::Param(param_num), _) => res_map.set_param(*param_num, rhs),
            (_, TypeKind::Param(_)) => panic!("param rhs"),

            // NOTE: whether this is resolvable is probably dependant on eval order
            (TypeKind::AssociatedType(item), _) => {
                let for_tys = item.subs.sub(&res_map.result_subs);
                let at = item.item.resolve_associated_ty(&for_tys);
                Self::match_types(at, rhs, res_map)
            }
            (_, TypeKind::AssociatedType(_)) => panic!("assoc ty rhs"),

            (TypeKind::Adt(_), _)
            | (_, TypeKind::Adt(_))
            | (TypeKind::Ptr(..), _)
            | (_, TypeKind::Ptr(..))
            | (TypeKind::Ref(..), _)
            | (_, TypeKind::Ref(..))
            | (TypeKind::Slice(..), _)
            | (_, TypeKind::Slice(..))
            | (TypeKind::Bool, _)
            | (_, TypeKind::Bool)
            | (TypeKind::Char, _)
            | (_, TypeKind::Char)
            | (TypeKind::Never, _)
            | (_, TypeKind::Never)
            | (TypeKind::Int(..), _)
            | (_, TypeKind::Int(..))
            | (TypeKind::Float(..), _)
            | (_, TypeKind::Float(..)) => false,
            _ => {
                panic!("match types {} == {}", lhs, rhs)
            }
        }
    }

    fn match_const(
        lhs: &ConstGeneric,
        rhs: &ConstGeneric,
        res_map: &mut SubMap<'vm>,
        const_ty: Type<'vm>,
    ) -> bool {
        if lhs.is_concrete() && lhs == rhs {
            return true;
        }

        match (lhs, rhs) {
            (ConstGeneric::Param(param_num), _) => {
                res_map.set_param_const(*param_num, rhs, const_ty)
            }
            _ => {
                println!("match const {:?} {:?}", lhs, rhs);
                panic!();
            }
        }
    }
}

#[derive(Debug)]
struct SubMap<'vm> {
    result_subs: SubList<'vm>,
}

impl<'vm> SubMap<'vm> {
    fn new(generic_counts: GenericCounts, vm: &'vm VM<'vm>) -> Self {
        Self {
            result_subs: SubList::from_summary(&generic_counts, vm),
        }
    }

    fn get_result(&mut self) -> &mut SubList<'vm> {
        &mut self.result_subs
    }
}

impl<'vm> SubMap<'vm> {
    fn set_param(&mut self, n: u32, val: Type<'vm>) -> bool {
        let entry = &mut self.result_subs.list[n as usize];

        if let Sub::Type(current_val) = entry {
            if current_val != &val {
                if current_val.kind() == &TypeKind::Unknown {
                    *current_val = val;
                } else {
                    return false;
                }
            }
        } else {
            panic!("attempt to fill non-type sub");
        }
        true
    }

    fn set_param_const(&mut self, n: u32, val: &ConstGeneric, ty: Type<'vm>) -> bool {
        let entry = &mut self.result_subs.list[n as usize];

        if let Sub::Const(current_ty, current_val) = entry {
            if current_val != val || current_ty != &ty {
                if current_val == &ConstGeneric::Unknown && current_ty.kind() == &TypeKind::Unknown
                {
                    *current_val = val.clone();
                    *current_ty = ty;
                } else {
                    println!("{:?} {:?} / {} {}", current_val, val, current_ty, ty);
                    panic!("failed to set const param");
                }
            }
        } else {
            panic!("attempt to fill non-const sub");
        }
        true
    }
}
