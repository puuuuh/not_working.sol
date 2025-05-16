use crate::hir::ident::Ident;
use crate::hir::ContractId;
use crate::items::HirPrint;
use crate::{impl_major_item, lazy_field, FileAstPtr};
use base_db::{BaseDb, Project};
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes;
use vfs::File;

use super::TypeRefId;

#[tracked(debug)]
pub struct EventId<'db> {
    #[id]
    pub file: File,

    #[id]
    pub name: Ident<'db>,

    #[tracked]
    pub is_anon: bool,

    #[tracked]
    pub parameters: Vec<EventParameterId<'db>>,

    #[tracked]
    pub node: AstPtr<nodes::EventDefinition>,
}

lazy_field!(EventId<'db>, origin, set_origin, Option<ContractId<'db>>, None);

impl HirPrint for EventId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("event ")?;
        self.name(db).write(db, w, ident)?;
        w.write_str("(")?;
        for (i, p) in self.parameters(db).iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?
            }
            p.write(db, w, ident)?;
        }
        w.write_str(")")?;
        if self.is_anon(db) {
            w.write_str(" anonymous;")?;
        } else {
            w.write_str(";")?;
        }
        Ok(())
    }
}

#[tracked(debug)]
pub struct EventParameterId<'db> {
    pub info: EventParameter<'db>,
}

impl HirPrint for EventParameterId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.info(db).write(db, w, ident)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct EventParameter<'db> {
    pub name: Option<Ident<'db>>,
    pub is_indexed: bool,
    pub ty: TypeRefId<'db>,
}

impl HirPrint for EventParameter<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.ty.write(db, w, ident)?;
        if self.is_indexed {
            w.write_str(" indexed ")?;
        }
        if let Some(name) = self.name {
            w.write_str(" ")?;
            name.write(db, w, ident)?;
        }
        Ok(())
    }
}
