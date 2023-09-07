use std::sync::{Mutex, Arc, OnceLock};

use ahash::AHashMap;

use crate::{ir::IRFunction, types::SubList, vm::Function};


/// Plays a similar role to function items. Contains IR and a table of monomorphizations.
/// FUTURE CONSIDERATIONS: Unlike function items, the IR cannot be mutated once set.
/// Closures will NOT support hot loading. Functions will create new closures on being hot loaded.
pub struct Closure<'vm> {
    ir: OnceLock<Arc<IRFunction<'vm>>>,
    mono_instances: Mutex<AHashMap<SubList<'vm>, &'vm Function<'vm>>>,
    unique_id: u32
}

impl<'vm> Closure<'vm> {
    pub fn new(unique_id: u32) -> Self {
        Self {
            ir: Default::default(),
            mono_instances: Default::default(),
            unique_id
        }
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
