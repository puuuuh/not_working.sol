use crate::hir::argument::ArgumentId;
use crate::hir::function::ModifierInvocation;
use crate::hir::source_unit::ItemOrigin;
use crate::hir::statement::StatementId;
use crate::hir::{StateMutability, Visibility};
use crate::items::HirPrint;
use crate::{impl_has_origin, lazy_field, FileAstPtr};
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes::{self, ConstructorDefinition};

#[tracked]
pub struct ConstructorId<'db> {
    #[salsa::tracked]
    pub info: Constructor<'db>,
    
    #[salsa::tracked]
    pub body: Option<AstPtr<nodes::Block>>,

    #[salsa::tracked]
    pub node: AstPtr<nodes::ConstructorDefinition>,
}

lazy_field!(ConstructorId<'db>, origin, set_origin, ItemOrigin<'db>);

impl_has_origin!(ConstructorId<'db>);

impl HirPrint for ConstructorId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.info(db).write(db, w, ident)?;
        if let Some(body) = self.body(db) {
            w.write_str("{}")?;
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
