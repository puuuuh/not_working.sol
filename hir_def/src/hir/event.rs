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
pub struct EventId<'db> {
    #[id]
    pub name: Ident<'db>,
    pub is_anon: bool,
    pub parameters: Vec<EventParameterId<'db>>,

    pub node: AstPtr<nodes::EventDefinition>,
}

lazy_field!(EventId<'db>, origin, set_origin, ItemOrigin<'db>);

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

#[tracked]
pub struct EventParameterId<'db> {
    pub info: EventParameter<'db>,
}

lazy_field!(EventParameterId<'db>, parent, set_parent, EventId<'db>);

impl HirPrint for EventParameterId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.info(db).write(db, w, ident)
    }
}

#[derive(Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct EventParameter<'db> {
    pub name: Option<Ident<'db>>,
    pub is_indexed: bool,
    pub ty: TypeRef<'db>,
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
