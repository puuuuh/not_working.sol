use crate::hir::argument::ArgumentId;
use crate::hir::function::ModifierInvocation;
use crate::hir::statement::StatementId;
use crate::hir::{StateMutability, Visibility};
use crate::item_tree::print::HirPrint;
use crate::item_tree::DefSite;
use crate::{lazy_field, FileAstPtr};
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes;

#[tracked]
pub struct ConstructorId<'db> {
    pub info: Constructor<'db>,
    pub body: Option<StatementId<'db>>,

    pub node: FileAstPtr<nodes::ConstructorDefinition>,
}

lazy_field!(ConstructorId<'db>, def_site, set_def_site, DefSite<'db>);

impl HirPrint for ConstructorId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.info(db).write(db, w, ident)?;
        if let Some(body) = self.body(db) {
            body.write(db, w, ident)?;
        } else {
            w.write_str(";")?;
        }
        Ok(())
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Hash, salsa::Update)]
pub struct Constructor<'db> {
    pub args: Vec<ArgumentId<'db>>,
    pub modifiers: Vec<ModifierInvocation<'db>>,
    pub vis: Visibility,
    pub mutability: StateMutability,
}

impl HirPrint for Constructor<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("constructor(")?;
        for (i, a) in self.args.iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?
            }
            a.write(db, w, ident)?
        }
        w.write_str(")")?;
        write!(w, " {} {} ", self.vis, self.mutability)?;
        for (i, a) in self.modifiers.iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?
            }
            a.write(db, w, ident)?;
        }
        Ok(())
    }
}
