mod common_types;
mod drop;
mod layout;
mod subs;
mod type_context;
mod type_persist;
mod types;

use std::sync::OnceLock;

pub use common_types::CommonTypes;
pub use drop::{DropBit, DropGlue, DropInfo};
pub use subs::*;
pub use type_context::TypeContext;
pub use type_persist::WriterTypes;
pub use types::*;

use crate::vm::VM;

#[derive(Copy, Clone)]
pub struct Type<'vm>(&'vm InternedType<'vm>, &'vm VM<'vm>);

struct InternedType<'vm> {
    kind: TypeKind<'vm>,
    layout: OnceLock<layout::Layout>,
    drop_info: OnceLock<drop::DropInfo<'vm>>,
    persist_id: OnceLock<u32>,
}
