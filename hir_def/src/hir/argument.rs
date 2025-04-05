use crate::hir::data_location::DataLocation;
use crate::hir::ident::Ident;
use crate::hir::TypeRef;
use rowan::ast::AstPtr;
use salsa::Database;
use syntax::ast::nodes::{self, CallArguments};
use std::fmt::Write;
use crate::items::HirPrint;

#[salsa::tracked]
pub struct ArgumentId<'db> {
    #[salsa::tracked]
    pub ty: TypeRef<'db>,
    #[salsa::tracked]
    pub location: Option<DataLocation>,
    #[salsa::tracked]
    pub name: Option<Ident<'db>>,

    pub node: AstPtr<nodes::Parameter>
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
