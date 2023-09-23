mod common_types;
mod layout;
mod subs;
mod type_context;
mod type_persist;
mod types;

use std::sync::OnceLock;

use ahash::AHashMap;
pub use common_types::CommonTypes;
pub use subs::*;
pub use type_context::TypeContext;
pub use type_persist::WriterTypes;
pub use types::*;

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
    /// Serialized form of `assoc_values`.
    impl_data: Option<&'vm [u8]>,
}
