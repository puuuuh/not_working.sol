use crate::hir::ident::Ident;
use crate::hir::ContractId;
use crate::items::HirPrint;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use base_db::BaseDb;
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::{Error, Write};
use syntax::ast::nodes;
use vfs::File;

use super::TypeRefId;

#[tracked(debug)]
#[derive(PartialOrd, Ord)]
pub struct ErrorId<'db> {
    #[id]
    pub file: File,

    #[id]
    pub name: Ident<'db>,

    #[tracked]
    pub parameters: Vec<ErrorParameterId<'db>>,

    #[tracked]
    pub node: AstPtr<nodes::ErrorDefinition>,
}

lazy_field!(ErrorId<'db>, origin, set_origin, Option<ContractId<'db>>, None);

impl HirPrint for ErrorId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("error ")?;
        self.name(db).write(db, w, ident)?;
        w.write_str("(")?;
        for (i, p) in self.parameters(db).iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?;
            }
            p.write(db, w, ident)?;
        }
        w.write_str(");")
    }
}

#[tracked(debug)]
pub struct ErrorParameterId<'db> {
    pub info: ErrorParameter<'db>,
}

impl HirPrint for ErrorParameterId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.info(db).write(db, w, ident)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct ErrorParameter<'db> {
    pub name: Option<Ident<'db>>,
    pub ty: TypeRefId<'db>,
}

impl HirPrint for ErrorParameter<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.ty.write(db, w, ident)?;
        if let Some(name) = self.name {
            w.write_str(" ")?;
            name.write(db, w, ident)?;
        }
        Ok(())
    }
}
