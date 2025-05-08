use crate::hir::ident::{Ident, IdentPath};
use crate::hir::statement::StatementId;
use crate::hir::variable_declaration::VariableDeclaration;
use crate::hir::{ContractId, HasFile};
use crate::items::HirPrint;
use crate::lower::LowerCtx;
use crate::source_map::item_source_map::ItemSourceMap;
use crate::{impl_major_item, lazy_field, lower, FileAstPtr, FileExt};
use base_db::{BaseDb, Project};
use rowan::ast::{AstNode, AstPtr};
use salsa::Database;
use std::fmt::Write;
use syntax::ast::nodes::{self, Stmt};
use vfs::File;

use super::expr::ExprId;
use super::state_mutability::StateMutability;
use super::visibility::Visibility;
use super::Item;

#[salsa::tracked(debug)]
pub struct FunctionId<'db> {
    #[tracked]
    pub file: File,

    #[id]
    pub name: Option<Ident<'db>>,
    pub info: Function<'db>,

    pub body_node: Option<AstPtr<nodes::Block>>,

    pub node: AstPtr<nodes::FunctionDefinition>,
}

lazy_field!(FunctionId<'db>, origin, set_origin, Option<ContractId<'db>>, None);

#[salsa::tracked]
impl<'db> FunctionId<'db> {
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn BaseDb) -> Option<(StatementId<'db>, ItemSourceMap<'db>)> {
        let file = self.file(db);
        let mut origin = self.origin(db);
        let node = self.body_node(db)?;
        let root = file.node(db);
        let root = root.syntax();

        let expr = node.to_node(&root);

        let mut lowerer = LowerCtx::new(db, file);

        let res = lowerer.lower_stmt(Stmt::Block(expr));

        return Some((
            res,
            ItemSourceMap::new(
                crate::IndexMapUpdate(lowerer.exprs),
                crate::IndexMapUpdate(lowerer.stmts),
            ),
        ));
    }
}

impl HirPrint for FunctionId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("function ")?;
        if let Some(name) = self.name(db) {
            name.write(db, w, ident)?
        };

        self.info(db).write(db, w, ident)?;
        if let Some(b) = self.body_node(db) {
            //b.write(db, w, ident)
            w.write_str("{ ... }")
        } else {
            w.write_char(';')
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, salsa::Update)]
pub struct Function<'db> {
    pub args: Vec<VariableDeclaration<'db>>,
    pub returns: Option<Vec<VariableDeclaration<'db>>>,
    pub modifiers: Vec<ModifierInvocation<'db>>,
    pub overrides: Option<Vec<IdentPath<'db>>>,
    pub vis: Visibility,
    pub mutability: StateMutability,
    pub is_virtual: bool,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, salsa::Update)]
pub struct ModifierInvocation<'db> {
    pub path: IdentPath<'db>,
    pub args: Option<Vec<(Option<Ident<'db>>, ExprId<'db>)>>,
}

impl HirPrint for ModifierInvocation<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.path.write(db, w, ident)?;
        if let Some(args) = &self.args {
            w.write_str("(")?;
            for (i, a) in args.iter().enumerate() {
                if i > 0 {
                    w.write_str(", ")?;
                }
                a.1.write(db, w, ident)?;
            }
            w.write_str(")")?;
        }
        Ok(())
    }
}

impl HirPrint for Function<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        w.write_str("(")?;
        for (i, a) in self.args.iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?
            }
            a.write(db, w, ident)?
        }
        w.write_str(")")?;
        if self.is_virtual {
            w.write_str(" virtual")?;
        }
        write!(w, " {} {} ", self.vis, self.mutability)?;
        if let Some(o) = &self.overrides {
            w.write_str(" override(")?;
            for (i, a) in o.iter().enumerate() {
                if i > 0 {
                    w.write_str(", ")?
                }
                a.write(db, w, ident)?
            }
            w.write_str(")")?;
        }
        for (i, a) in self.modifiers.iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?
            }
            a.write(db, w, ident)?;
        }
        Ok(())
    }
}
