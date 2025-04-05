use crate::hir::ident::Ident;
use crate::hir::source_unit::ItemOrigin;
use crate::hir::type_name::TypeRef;
use crate::items::HirPrint;
use crate::{impl_has_origin, lazy_field, FileAstPtr};
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes;

#[tracked]
pub struct StructureId<'db> {
    pub name: Ident<'db>,
    pub fields: Vec<StructureFieldId<'db>>,

    pub node: AstPtr<nodes::StructDefinition>,
}

lazy_field!(StructureId<'db>, origin, set_origin, ItemOrigin<'db>);
impl_has_origin!(StructureId<'db>);

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

#[tracked]
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
