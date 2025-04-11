use rowan::ast::AstPtr;
use syntax::ast::nodes;

use crate::{hir::source_unit::ItemOrigin, impl_major_item, lazy_field};

#[salsa::tracked]
pub struct PragmaId<'db> {
    #[return_ref]
    pub data: String,

    pub node: AstPtr<nodes::Pragma>,
}

lazy_field!(PragmaId<'db>, origin, set_origin, ItemOrigin<'db>);