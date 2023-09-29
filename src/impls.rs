use std::{hash::Hash, sync::Arc};

use ahash::AHashMap;

use crate::{
    crate_provider::TraitImplResult,
    items::{AssocValue, BoundKind, CrateId, GenericCounts, Item, ItemId},
    lazy_collections::LazyKey,
    types::{Sub, SubList, Type, TypeKind},
    vm::VM,
};

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
                    _ => panic!("todo check bound {:?}", bound),
                }
            }

            Some(result_subs)
        } else {
            None
        }
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
                panic!("fixme? this looks annoying");
            }

            (_, TypeKind::Param(param_num)) => res_map.set(SubSide::Rhs, *param_num, lhs),
            (TypeKind::Param(param_num), _) => res_map.set(SubSide::Lhs, *param_num, rhs),

            (TypeKind::Adt(_), _)
            | (_, TypeKind::Adt(_))
            | (TypeKind::Ptr(..), _)
            | (_, TypeKind::Ptr(..))
            | (TypeKind::Ref(..), _)
            | (_, TypeKind::Ref(..))
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
