mod layout;
mod subs;
mod type_context;
mod type_persist;
mod types;
mod common_types;

use std::sync::OnceLock;

use ahash::AHashMap;
pub use subs::*;
pub use type_context::TypeContext;
pub use types::*;
pub use common_types::CommonTypes;

use crate::{
    items::{AssocValue, CrateId},
    vm::VM,
};

#[derive(Copy, Clone)]
pub struct Type<'vm>(&'vm InternedType<'vm>, &'vm VM<'vm>);

struct InternedType<'vm> {
    kind: TypeKind<'vm>,
    layout: OnceLock<layout::Layout>,
    assoc_values: OnceLock<AHashMap<String, (CrateId, AssocValue<'vm>)>>,
    persist_id: OnceLock<u32>,
}
