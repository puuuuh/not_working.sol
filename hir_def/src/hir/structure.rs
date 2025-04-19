use crate::hir::ident::Ident;
use crate::hir::type_name::TypeRef;
use crate::hir::{ContractId, HasSourceUnit};
use crate::items::HirPrint;
use crate::nameres::body::Definition;
use crate::nameres::scope::ItemScope;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use base_db::{BaseDb, Project};
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use vfs::File;
use std::fmt::Write;
use syntax::ast::nodes;

use super::HasDefs;

#[tracked(debug)]
pub struct StructureId<'db> {
    pub name: Ident<'db>,
    pub fields: Vec<StructureFieldId<'db>>,

    pub node: AstPtr<nodes::StructDefinition>,
}

lazy_field!(StructureId<'db>, origin, set_origin, Option<ContractId<'db>>);

#[salsa::tracked]
impl<'db> StructureId<'db> {
    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb, project: Project, module: File) -> ItemScope<'db> {
        self.origin(db)
            .map(|c| c.scope(db, project, module))
            .unwrap_or_else(|| module.source_unit(db).scope(db, project, module))
    }
}

#[salsa::tracked]
impl<'db> HasDefs<'db> for StructureId<'db> {
    #[salsa::tracked]
    fn defs(self, db: &'db dyn BaseDb, module: File) -> Vec<(Ident<'db>, Definition<'db>)> {
        self.fields(db)
            .iter()
            .map(|item| (item.name(db), Definition::Field(*item)))
            .collect()
    }
}

impl HirPrint for StructureId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        let my_ident_str = "\t".repeat(ident);
        let ident_str = "\t".repeat(ident + 1);
        w.write_str("struct ")?;
        self.name(db).write(db, w, ident)?;
        w.write_str(" {\n")?;
        w.write_str(&ident_str)?;
        for (i, f) in self.fields(db).iter().enumerate() {
            if i > 0 {
                w.write_str(",\n")?;
                w.write_str(&ident_str)?;
            }
            f.write(db, w, ident)?;
        }
        write!(w, "\n{my_ident_str}}}")?;

        Ok(())
    }
}

#[tracked(debug)]
pub struct StructureFieldId<'db> {
    pub name: Ident<'db>,
    pub ty: TypeRef<'db>,
}

impl HirPrint for StructureFieldId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.name(db).write(db, w, ident)?;
        w.write_str(": ")?;
        self.ty(db).write(db, w, ident)?;
        Ok(())
    }
}

lazy_field!(StructureFieldId<'db>, parent, set_parent, StructureId<'db>);
