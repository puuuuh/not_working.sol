use crate::{hir::source_unit::ItemOrigin, impl_has_origin, lazy_field};

#[salsa::tracked]
pub struct PragmaId<'db> {
    #[return_ref]
    pub data: String,
}

lazy_field!(PragmaId<'db>, origin, set_origin, ItemOrigin<'db>);
impl_has_origin!(PragmaId<'db>);