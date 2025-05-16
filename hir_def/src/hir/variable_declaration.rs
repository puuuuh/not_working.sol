use crate::hir::ident::Ident;
use crate::hir::{Item, TypeRefId};
use crate::items::HirPrint;
use crate::{hir::data_location::DataLocation, lazy_field};
use crate::{impl_has_syntax, FileAstPtr, FileExt};
use base_db::BaseDb;
use rowan::ast::AstNode;
use rowan::ast::AstPtr;
use salsa::Database;
use std::fmt::Write;
use syntax::ast::nodes::{self, CallArguments};
use vfs::File;

use super::{HasFile, StatementId};

#[salsa::tracked(debug)]
pub struct VariableDeclaration<'db> {
    #[id]
    pub ty: TypeRefId<'db>,

    #[tracked]
    pub location: Option<DataLocation>,

    #[tracked]
    pub name: Option<Ident<'db>>,

    #[tracked]
    pub node: FileAstPtr<nodes::VariableDeclaration>,
}

impl HirPrint for VariableDeclaration<'_> {
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
