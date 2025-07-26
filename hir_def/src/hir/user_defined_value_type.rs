use crate::hir::ident::Ident;
use crate::hir::type_name::TypeRefId;
use crate::hir::ContractId;
use crate::items::HirPrint;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use base_db::BaseDb;
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes;
use vfs::File;

#[tracked(debug)]
#[derive(PartialOrd, Ord)]
pub struct UserDefinedValueTypeId<'db> {
    pub file: File,
    pub name: Ident<'db>,

    #[tracked]
    pub ty: TypeRefId<'db>,

    #[tracked]
    pub node: AstPtr<nodes::UserDefinedValueTypeDefinition>,
}

lazy_field!(UserDefinedValueTypeId<'db>, origin, set_origin, Option<ContractId<'db>>);

impl HirPrint for UserDefinedValueTypeId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("type ")?;
        self.name(db).write(db, w, ident)?;
        w.write_str(" is ")?;
        self.ty(db).write(db, w, ident)?;
        w.write_str(";")
    }
}
