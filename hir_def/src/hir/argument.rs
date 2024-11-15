use crate::hir::data_location::DataLocation;
use crate::hir::ident::Ident;
use crate::hir::TypeRef;
use crate::item_tree::print::HirPrint;
use salsa::Database;
use std::fmt::Write;

#[salsa::tracked]
pub struct ArgumentId<'db> {
    pub ty: TypeRef<'db>,
    pub location: Option<DataLocation>,
    pub name: Option<Ident<'db>>,
}

impl HirPrint for ArgumentId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.ty(db).write(db, w, ident)?;
        if let Some(loc) = self.location(db) {
            write!(w, " {loc}")?;
        }
        if let Some(name) = self.name(db) {
            w.write_str(" ")?;
            name.write(db, w, ident)?;
        }
        Ok(())
    }
}
