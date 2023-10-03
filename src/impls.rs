use std::{hash::Hash, sync::Arc};

use ahash::AHashMap;

use crate::{
    crate_provider::TraitImplResult,
    items::{AssocValue, BoundKind, CrateId, GenericCounts, Item, ItemId},
    lazy_collections::LazyKey,
    types::{Sub, SubList, Type, TypeKind},
    vm::VM,
};

/// Find candidate crates for a trait impl on a list of types.
///
/// TODO smallvec
pub fn find_trait_impl_crate(for_tys: &SubList, trait_crate: CrateId) -> Vec<CrateId> {
    let mut res = vec![trait_crate];

    for sub in for_tys.list.iter() {
        match sub {
            Sub::Type(ty) => {
                let cid = find_source_crate(*ty);
                if !res.contains(&cid) {
                    res.push(cid);
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
    vec![find_source_crate(ty)]
}

fn find_source_crate(ty: Type) -> CrateId {
    match ty.kind() {
        TypeKind::Adt(item) => item.item.crate_id,
        TypeKind::FunctionDef(item) => item.item.crate_id,

        TypeKind::Ref(child, _) => find_source_crate(*child),

        TypeKind::Bool
        | TypeKind::Float(_)
        | TypeKind::Int(..)
        | TypeKind::Tuple(_)
        | TypeKind::StringSlice
        | TypeKind::Slice(_) => *ty.vm().core_crate.get().unwrap(),
        _ => panic!("source crate {}", ty),
    }
}

pub struct ImplBounds<'vm> {
    pub for_tys: SubList<'vm>,
    pub bounds: Vec<BoundKind<'vm>>,
    pub generic_counts: GenericCounts,
}

pub struct Impls<'vm> {
    crate_id: CrateId,
    inherent_table: AHashMap<String, Vec<InherentMember<'vm>>>,
    trait_table: AHashMap<TraitKey, AHashMap<Option<&'vm str>, Vec<TraitValue<'vm>>>>,
    bounds_table: Vec<ImplBounds<'vm>>,
}

impl<'vm> Impls<'vm> {
    pub fn new(crate_id: CrateId) -> Self {
        Self {
            crate_id,
            inherent_table: Default::default(),
            trait_table: Default::default(),
            bounds_table: Default::default(),
        }
    }
}

struct InherentMember<'vm> {
    bounds_id: u32,
    value: AssocValue<'vm>,
}

#[derive(PartialEq, Eq)]
struct TraitKey {
    crate_id: CrateId,
    item_id: ItemId,
}

impl Hash for TraitKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.crate_id.index());
        state.write_usize(self.item_id.index());
    }
}

struct TraitValue<'vm> {
    bounds_id: u32,
    values: Arc<[Option<AssocValue<'vm>>]>,
}

impl<'vm> Impls<'vm> {
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

    pub fn find_trait(
        &self,
        trait_item: &Item<'vm>,
        for_tys: &SubList<'vm>,
    ) -> Option<TraitImplResult<'vm>> {
        let key_trait = TraitKey {
            crate_id: trait_item.crate_id,
            item_id: trait_item.item_id,
        };

        if let Some(sub_table) = self.trait_table.get(&key_trait) {
            let key_ty = for_tys.list[0].assert_ty().impl_key();

            if let Some(list) = sub_table.get(&key_ty) {
                for value in list {
                    let bounds = &self.bounds_table[value.bounds_id as usize];
                    if let Some(impl_subs) = bounds.check(for_tys, trait_item.vm) {
                        return Some(TraitImplResult {
                            crate_id: self.crate_id,
                            assoc_values: value.values.clone(),
                            impl_subs,
                        });
                    }
                }
            }

            // retry with a 'None' key
            if key_ty != None {
                if let Some(list) = sub_table.get(&None) {
                    for value in list {
                        let bounds = &self.bounds_table[value.bounds_id as usize];
                        if let Some(impl_subs) = bounds.check(for_tys, trait_item.vm) {
                            return Some(TraitImplResult {
                                crate_id: self.crate_id,
                                assoc_values: value.values.clone(),
                                impl_subs,
                            });
                        }
                    }
                }
            }
        }
        None
    }

    pub fn find_inherent(&self, full_key: &str, ty: Type<'vm>) -> Option<AssocValue<'vm>> {
        if let Some(list) = self.inherent_table.get(full_key) {
            for value in list {
                let bounds = &self.bounds_table[value.bounds_id as usize];
                if bounds.check_inherent(ty) {
                    return Some(value.value.clone());
                }
            }
        }
        None
    }
}

impl<'vm> ImplBounds<'vm> {
    pub fn check(&self, for_tys: &SubList<'vm>, vm: &'vm VM<'vm>) -> Option<SubList<'vm>> {
        let mut sub_map = SubMap::default();

        if Self::match_subs(&self.for_tys, for_tys, &mut sub_map) {
            let mut result_subs = SubList::from_summary(&self.generic_counts, vm);

            sub_map.apply_to(SubSide::Lhs, &mut result_subs);
            sub_map.assert_empty(SubSide::Rhs);

            // this is an ATTEMPT to deal with GATs -- it may not be entirely correct
            if for_tys.list.len() > self.for_tys.list.len() {
                let start = self.for_tys.list.len();
                let end = for_tys.list.len();
                for i in (start..end) {
                    let sub = for_tys.list[i].clone();
                    result_subs.list.push(sub);
                }
            }

            for bound in self.bounds.iter() {
                match bound {
                    BoundKind::Trait(trait_bound) => {
                        let types_to_check = trait_bound.subs.sub(&result_subs);

                        let has_impl = trait_bound.item.trait_has_impl(&types_to_check); // &mut Some(&mut result_subs));

                        if !has_impl {
                            return None;
                        }
                    }
                    BoundKind::Projection(assoc_ty, eq_ty) => {
                        let types_to_check = assoc_ty.subs.sub(&result_subs);

                        let resolved_assoc_ty =
                            assoc_ty.item.resolve_associated_ty(&types_to_check);

                        let mut proj_sub_map = Default::default();
                        if !Self::match_types(resolved_assoc_ty, *eq_ty, &mut proj_sub_map) {
                            panic!("unmatched {} = {}", resolved_assoc_ty, eq_ty);
                        }

                        proj_sub_map.assert_empty(SubSide::Lhs);
                        proj_sub_map.apply_to(SubSide::Rhs, &mut result_subs);
                    }
                }
            }

            Some(result_subs)
        } else {
            None
        }
    }

    /// This relaxed check method tests against a single type,
    /// and does not actually check the stored bounds.
    pub fn check_inherent(&self, ty: Type<'vm>) -> bool {
        let mut map = SubMap::default();
        map.allow_bidirectional = true;

        let check_ty = self.for_tys.list[0].assert_ty();

        Self::match_types(ty, check_ty, &mut map)
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

            (TypeKind::Param(lhs_param), TypeKind::Param(rhs_param)) => {
                if res_map.allow_bidirectional {
                    res_map.set(SubSide::Rhs, *rhs_param, lhs);
                    res_map.set(SubSide::Lhs, *lhs_param, rhs);
                    true
                } else {
                    panic!("bidirectional bound not permitted");
                }
            }

            (_, TypeKind::Param(param_num)) => res_map.set(SubSide::Rhs, *param_num, lhs),
            (TypeKind::Param(param_num), _) => res_map.set(SubSide::Lhs, *param_num, rhs),

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
}

#[derive(PartialEq, Debug)]
enum SubSide {
    Lhs,
    Rhs,
}

#[derive(Default, Debug)]
struct SubMap<'vm> {
    map: Vec<((SubSide, u32), Type<'vm>)>,
    pub allow_bidirectional: bool,
}

impl<'vm> SubMap<'vm> {
    fn set(&mut self, side: SubSide, n: u32, val: Type<'vm>) -> bool {
        let key = (side, n);
        for (ek, ev) in &self.map {
            if *ek == key {
                assert!(*ev == val);
                return true;
            }
        }
        self.map.push((key, val));
        true
    }

    fn apply_to(&self, target_side: SubSide, target_subs: &mut SubList<'vm>) {
        for ((side, n), val) in &self.map {
            if *side == target_side {
                target_subs.list[*n as usize] = Sub::Type(*val);
            }
        }
    }

    fn assert_empty(&self, target_side: SubSide) {
        for ((side, _), _) in &self.map {
            if *side == target_side {
                for entry in &self.map {
                    println!(" - {:?}", entry);
                }
                panic!("SubMap::assert_empty failed");
            }
        }
    }
}
