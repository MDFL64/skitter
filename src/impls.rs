use std::hash::Hash;

use ahash::AHashMap;

use crate::{items::{BoundKind, GenericCounts, AssocValue, CrateId, ItemId, Item}, types::{SubList, Sub, Type}, lazy_collections::LazyKey};

pub struct ImplBounds<'vm> {
    pub for_tys: SubList<'vm>,
    pub bounds: Vec<BoundKind<'vm>>,
    pub generic_counts: GenericCounts,
}

#[derive(Default)]
pub struct Impls<'vm> {
    inherent_table: AHashMap<String,Vec<InherentMember<'vm>>>,
    trait_table: AHashMap<TraitKey,AHashMap<Option<&'vm str>,Vec<TraitValue<'vm>>>>,
    bounds_table: Vec<ImplBounds<'vm>>,
}

struct InherentMember<'vm> {
    bounds_id: u32,
    value: AssocValue<'vm>
}

#[derive(PartialEq, Eq)]
struct TraitKey {
    crate_id: CrateId,
    item_id: ItemId
}

impl Hash for TraitKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.crate_id.index());
        state.write_usize(self.item_id.index());
    }
}

struct TraitValue<'vm> {
    bounds_id: u32,
    values: Vec<Option<AssocValue<'vm>>>,
}

impl<'vm> Impls<'vm> {
    pub fn add_bounds(&mut self, bounds: ImplBounds<'vm>) -> u32 {
        let index = self.bounds_table.len() as u32;
        self.bounds_table.push(bounds);
        index
    }

    pub fn add_inherent(&mut self, key: String, bounds_id: u32, value: AssocValue<'vm>) {
        let list = self.inherent_table.entry(key).or_default();
        list.push(InherentMember{
            bounds_id,
            value
        });
    }

    pub fn add_trait(&mut self, trait_item: &Item, key: Option<&'vm str>, bounds_id: u32, assoc_values: Vec<Option<AssocValue<'vm>>>) {
        let trait_key = TraitKey{
            crate_id: trait_item.crate_id,
            item_id: trait_item.item_id
        };

        let sub_table = self.trait_table.entry(trait_key).or_default();
        let list = sub_table.entry(key).or_default();

        list.push(TraitValue {
            bounds_id,
            values: assoc_values
        });
    }

    pub fn find_trait(&self, trait_item: &Item, for_tys: &SubList<'vm>) -> Option<&[Option<AssocValue<'vm>>]> {
        let key_trait = TraitKey{
            crate_id: trait_item.crate_id,
            item_id: trait_item.item_id
        };

        if let Some(sub_table) = self.trait_table.get(&key_trait) {
            let key_ty = for_tys.list[0].assert_ty().impl_key();
            if let Some(list) = sub_table.get(&key_ty) {
                for value in list {
                    let bounds = &self.bounds_table[value.bounds_id as usize];
                    if bounds.check(for_tys) {
                        return Some(&value.values);
                    }
                }
            }
        }
        None
    }
}

impl<'vm> ImplBounds<'vm> {
    pub fn check(&self, for_tys: &SubList<'vm>) -> bool {
        let mut sub_map = SubMap::default();

        if Self::match_subs(&self.for_tys,for_tys, &mut sub_map) {
            sub_map.assert_empty(SubSide::Lhs);
            sub_map.assert_empty(SubSide::Rhs);

            if self.bounds.len() > 0 {
                panic!("todo impl bounds");
            }

            if self.generic_counts.total() > 0 {
                panic!("todo generics");
            }

            true
        } else {
            false
        }
    }

    fn match_subs(lhs: &SubList<'vm>, rhs: &SubList<'vm>, res_map: &mut SubMap<'vm>) -> bool {
        if lhs.list.len() != rhs.list.len() {
            return false;
        }
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

        println!("{} = {}",lhs,rhs);
        panic!();
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
