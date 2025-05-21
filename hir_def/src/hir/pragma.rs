use base_db::{BaseDb};
use rowan::ast::AstPtr;
use syntax::ast::nodes;
use vfs::File;

use crate::{hir::ContractId, impl_major_item, lazy_field};

#[salsa::tracked(debug)]
pub struct PragmaId<'db> {
    #[id]
    pub file: File,

    #[returns(ref)]
    #[tracked]
    pub data: String,

    pub node: AstPtr<nodes::Pragma>,
}
