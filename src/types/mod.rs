mod layout;
mod subs;
mod type_context;
mod types;

use std::sync::{OnceLock, RwLock};

use ahash::AHashMap;
pub use subs::*;
pub use type_context::TypeContext;
pub use types::*;

use crate::{
    items::{CrateId, FunctionIRSource},
    vm::VM,
};

#[derive(Copy, Clone)]
pub struct Type<'vm>(&'vm InternedType<'vm>, &'vm VM<'vm>);

struct InternedType<'vm> {
    kind: TypeKind<'vm>,
    layout: OnceLock<layout::Layout>,
    impl_table: RwLock<AHashMap<String, (CrateId, FunctionIRSource<'vm>)>>,
}
