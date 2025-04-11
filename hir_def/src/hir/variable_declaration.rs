use crate::{impl_has_syntax, FileExt};
use crate::{hir::data_location::DataLocation, lazy_field};
use crate::hir::ident::Ident;
use crate::hir::{Item, TypeRef};
use base_db::BaseDb;
use rowan::ast::AstPtr;
use rowan::ast::AstNode;
use salsa::Database;
use syntax::ast::nodes::{self, CallArguments};
use vfs::File;
use std::fmt::Write;
use crate::items::HirPrint;

use super::{HasFile, StatementId};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, salsa::Update)]
pub enum VariableDeclarationOwner<'db> {
    // Parameter
    Item(Item<'db>),
    // VariableDeclarationStmt
    Statement(StatementId<'db>),
}

#[salsa::tracked]
pub struct VariableDeclaration<'db> {
    #[salsa::tracked]
    pub ty: TypeRef<'db>,
    #[salsa::tracked]
    pub location: Option<DataLocation>,
    #[salsa::tracked]
    pub name: Option<Ident<'db>>,

    #[salsa::tracked]
    pub node: AstPtr<nodes::VariableDeclaration>
}

impl<'db> VariableDeclaration<'db> {
    pub fn syntax(self, db: &'db dyn BaseDb, file: File) -> nodes::VariableDeclaration {
        self.node(db).to_node(&file.tree(db).syntax())
    }
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
