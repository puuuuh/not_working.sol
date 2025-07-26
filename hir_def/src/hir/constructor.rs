use crate::hir::function::ModifierInvocation;
use crate::hir::statement::StatementId;
use crate::hir::variable_declaration::VariableDeclaration;
use crate::hir::{ContractId, Item};
use crate::items::HirPrint;
use crate::lower::LowerCtx;
use crate::source_map::item_source_map::ItemSourceMap;
use crate::{impl_major_item, lazy_field, FileAstPtr, FileExt};
use base_db::BaseDb;
use rowan::ast::AstNode;
use rowan::ast::AstPtr;
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes::{self, ConstructorDefinition, UnitSource};
use vfs::File;

use super::state_mutability::StateMutability;
use super::visibility::Visibility;
use super::{HasFile, SourceUnit};

#[tracked(debug)]
#[derive(PartialOrd, Ord)]
pub struct ConstructorId<'db> {
    pub file: File,
    pub info: Constructor<'db>,

    #[tracked]
    pub body_node: Option<AstPtr<nodes::Block>>,

    #[tracked]
    pub node: AstPtr<nodes::ConstructorDefinition>,
}

lazy_field!(ConstructorId<'db>, origin, set_origin, Option<ContractId<'db>>, None);

#[salsa::tracked]
impl<'db> ConstructorId<'db> {
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn BaseDb) -> Option<(StatementId<'db>, ItemSourceMap<'db>)> {
        let mut origin = self.origin(db);
        let file = self.file(db);
        let node = self.body_node(db)?;
        let root = file.node(db);
        let root = root.syntax();

        let expr = node.to_node(root);

        let mut lowerer = LowerCtx::new(db, file);

        let res = lowerer.lower_stmt(nodes::Stmt::Block(expr));

        Some((
            res,
            ItemSourceMap::new(
                db,
                (lowerer.exprs, lowerer.stmts),
            ),
        ))
    }
}

impl HirPrint for ConstructorId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.info(db).write(db, w, ident)?;
        if let Some(body) = self.body_node(db) {
            w.write_str("{}")?;
        } else {
            w.write_str(";")?;
        }
        Ok(())
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, salsa::Update)]
pub struct Constructor<'db> {
    pub args: Vec<VariableDeclaration<'db>>,
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
