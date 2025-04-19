use crate::hir::ident::Ident;
use crate::hir::type_name::TypeRef;
use crate::hir::{ContractId, HasSourceUnit};
use crate::items::HirPrint;
use crate::nameres::scope::ItemScope;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use base_db::{BaseDb, Project};
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use vfs::File;
use std::fmt::Write;
use syntax::ast::nodes;

#[tracked(debug)]
pub struct UserDefinedValueTypeId<'db> {
    pub name: Ident<'db>,
    pub ty: TypeRef<'db>,

    pub node: AstPtr<nodes::UserDefinedValueTypeDefinition>,
}

lazy_field!(UserDefinedValueTypeId<'db>, origin, set_origin, Option<ContractId<'db>>);

#[salsa::tracked]
impl<'db> UserDefinedValueTypeId<'db> {
    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb, project: Project, module: File) -> ItemScope<'db> {
        self.origin(db)
            .map(|c| c.scope(db, project, module))
            .unwrap_or_else(|| module.source_unit(db).scope(db, project, module))
    }
}

impl HirPrint for UserDefinedValueTypeId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("type ")?;
        self.name(db).write(db, w, ident)?;
        w.write_str(" is ")?;
        self.ty(db).write(db, w, ident)?;
        w.write_str(";")
    }
}
