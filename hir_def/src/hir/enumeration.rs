use crate::hir::ident::Ident;
use crate::hir::ContractId;
use crate::items::HirPrint;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use base_db::{BaseDb, Project};
use rowan::ast::AstPtr;
use salsa::Database;
use vfs::File;
use std::fmt::Write;
use syntax::ast::nodes;

#[salsa::tracked(debug)]
pub struct EnumerationId<'db> {
    #[tracked]
    pub file: File,

    #[id]
    pub name: Ident<'db>,
    pub fields: Vec<EnumerationVariantId<'db>>,

    pub node: AstPtr<nodes::EnumDefinition>,
}

lazy_field!(EnumerationId<'db>, origin, set_origin, Option<ContractId<'db>>, None);

impl HirPrint for EnumerationId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        let my_ident_str = "\t".repeat(ident);
        let ident_str = "\t".repeat(ident + 1);
        w.write_str("enum ")?;
        self.name(db).write(db, w, ident)?;
        w.write_str(" {\n")?;
        w.write_str(&ident_str)?;
        for (i, f) in self.fields(db).iter().enumerate() {
            if i > 0 {
                w.write_str(",\n")?;
                w.write_str(&ident_str)?;
            }
            f.write(db, w, ident + 1)?;
        }
        write!(w, "\n{my_ident_str}}}")?;
        Ok(())
    }
}

#[salsa::tracked(debug)]
pub struct EnumerationVariantId<'db> {
    pub name: Ident<'db>,
}

lazy_field!(EnumerationVariantId<'db>, parent, set_parent, EnumerationId<'db>);

impl HirPrint for EnumerationVariantId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.name(db).write(db, w, ident)
    }
}
