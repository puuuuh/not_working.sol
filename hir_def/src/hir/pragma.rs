use base_db::{BaseDb, Project};
use rowan::ast::AstPtr;
use syntax::ast::nodes;
use vfs::File;

use crate::{hir::{ContractId, HasSourceUnit}, impl_major_item, lazy_field};

#[salsa::tracked(debug)]
pub struct PragmaId<'db> {
    #[tracked]
    pub file: File,

    #[return_ref]
    pub data: String,

    pub node: AstPtr<nodes::Pragma>,
}