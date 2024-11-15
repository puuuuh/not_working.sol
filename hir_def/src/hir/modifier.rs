use crate::hir::argument::ArgumentId;
use crate::hir::ident::{Ident, IdentPath};
use crate::hir::statement::StatementId;
use crate::item_tree::print::HirPrint;
use crate::item_tree::DefSite;
use crate::{lazy_field, FileAstPtr};
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes;

#[tracked]
pub struct ModifierId<'db> {
    #[id]
    pub name: Ident<'db>,
    pub info: Modifier<'db>,
    pub body: StatementId<'db>,

    pub node: FileAstPtr<nodes::ModifierDefinition>,
}

impl HirPrint for ModifierId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("modifier ")?;
        self.name(db).write(db, w, ident)?;
        self.info(db).write(db, w, ident)?;
        self.body(db).write(db, w, ident)
    }
}

lazy_field!(ModifierId<'db>, def_site, set_def_site, DefSite<'db>);

#[derive(Eq, PartialEq, Debug, Clone, Hash, salsa::Update)]
pub struct Modifier<'db> {
    pub args: Vec<ArgumentId<'db>>,
    pub overrides: Option<Vec<IdentPath<'db>>>,
    pub is_virtual: bool,
}

impl HirPrint for Modifier<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("(")?;
        for (i, a) in self.args.iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?;
            }
            a.write(db, w, ident)?;
        }
        w.write_str(")")?;
        if let Some(over) = &self.overrides {
            w.write_str(" override(")?;
            for (i, o) in over.iter().enumerate() {
                if i > 0 {
                    w.write_str(", ")?;
                }
                o.write(db, w, ident)?;
            }
            w.write_str(")")?;
        }
        if self.is_virtual {
            w.write_str(" virtual")?;
        }

        Ok(())
    }
}
