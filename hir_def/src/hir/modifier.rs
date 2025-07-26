use crate::hir::ident::{Ident, IdentPath};
use crate::hir::statement::StatementId;
use crate::hir::variable_declaration::VariableDeclaration;
use crate::hir::{ContractId, HasFile, Item};
use crate::items::HirPrint;
use crate::lower::LowerCtx;
use crate::source_map::item_source_map::ItemSourceMap;
use crate::{impl_major_item, lazy_field, FileAstPtr, FileExt};
use base_db::BaseDb;
use rowan::ast::AstNode;
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes::{self, Stmt};
use vfs::File;

#[tracked(debug)]
#[derive(PartialOrd, Ord)]
pub struct ModifierId<'db> {
    pub file: File,
    pub name: Ident<'db>,

    #[tracked]
    pub info: Modifier<'db>,

    #[tracked]
    pub body_node: Option<AstPtr<nodes::Block>>,

    #[tracked]
    pub node: AstPtr<nodes::ModifierDefinition>,
}

lazy_field!(ModifierId<'db>, origin, set_origin, Option<ContractId<'db>>, None);

#[salsa::tracked]
impl<'db> ModifierId<'db> {
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn BaseDb) -> Option<(StatementId<'db>, ItemSourceMap<'db>)> {
        let file = self.file(db);
        let mut origin = self.origin(db);
        let tree = file.node(db);
        let root = tree.syntax();
        let node = self.body_node(db)?;

        let expr = node.to_node(root);

        let mut lowerer = LowerCtx::new(db, file);

        let res = lowerer.lower_stmt(Stmt::Block(expr));

        Some((
            res,
            ItemSourceMap::new(
                db,
                (lowerer.exprs, lowerer.stmts),
            ),
        ))
    }
}

impl HirPrint for ModifierId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("modifier ")?;
        self.name(db).write(db, w, ident)?;
        self.info(db).write(db, w, ident)

        //self.body(db).write(db, w, ident)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, salsa::Update)]
pub struct Modifier<'db> {
    pub args: Vec<VariableDeclaration<'db>>,
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
