use base_db::{BaseDb, Project};
use rowan::ast::AstPtr;
use syntax::ast::nodes;
use vfs::File;

use crate::{hir::{ContractId, HasSourceUnit}, impl_major_item, lazy_field, nameres::scope::ItemScope};

#[salsa::tracked(debug)]
pub struct PragmaId<'db> {
    #[return_ref]
    pub data: String,

    pub node: AstPtr<nodes::Pragma>,
}

#[salsa::tracked]
impl<'db> PragmaId<'db> {
    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb, project: Project, module: File) -> ItemScope<'db> {
        module.source_unit(db).scope(db, project, module)
    }
}