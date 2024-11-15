use crate::hir::ident::Ident;
use crate::hir::type_name::TypeRef;
use crate::item_tree::print::HirPrint;
use crate::item_tree::DefSite;
use crate::{lazy_field, FileAstPtr};
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes;

#[tracked]
pub struct UserDefinedValueTypeId<'db> {
    pub name: Ident<'db>,
    pub ty: TypeRef<'db>,

    pub node: FileAstPtr<nodes::UserDefinedValueTypeDefinition>,
}

lazy_field!(UserDefinedValueTypeId<'db>, def_site, set_def_site, DefSite<'db>);

impl HirPrint for UserDefinedValueTypeId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("type ")?;
        self.name(db).write(db, w, ident)?;
        w.write_str(" is ")?;
        self.ty(db).write(db, w, ident)?;
        w.write_str(";")
    }
}
