use crate::hir::ident::Ident;
use crate::hir::source_unit::ItemOrigin;
use crate::hir::type_name::TypeRef;
use crate::items::HirPrint;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes;

#[tracked]
pub struct UserDefinedValueTypeId<'db> {
    pub name: Ident<'db>,
    pub ty: TypeRef<'db>,

    pub node: AstPtr<nodes::UserDefinedValueTypeDefinition>,
}

lazy_field!(UserDefinedValueTypeId<'db>, origin, set_origin, ItemOrigin<'db>);

impl HirPrint for UserDefinedValueTypeId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("type ")?;
        self.name(db).write(db, w, ident)?;
        w.write_str(" is ")?;
        self.ty(db).write(db, w, ident)?;
        w.write_str(";")
    }
}
