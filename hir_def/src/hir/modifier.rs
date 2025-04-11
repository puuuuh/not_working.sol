use crate::hir::variable_declaration::VariableDeclaration;
use crate::hir::ident::{Ident, IdentPath};
use crate::hir::source_unit::ItemOrigin;
use crate::hir::statement::StatementId;
use crate::hir::{HasFile, HasOrigin};
use crate::items::HirPrint;
use crate::lower::LowerCtx;
use crate::resolver::resolve_scopes;
use crate::scope::ExprScopeRoot;
use crate::source_map::item_source_map::ItemSourceMap;
use crate::{impl_major_item, lazy_field, FileAstPtr, FileExt};
use base_db::{BaseDb, Project};
use rowan::ast::AstPtr;
use rowan::ast::AstNode;
use salsa::{tracked, Database};
use std::fmt::Write;
use syntax::ast::nodes::{self, Stmt};

#[tracked]
pub struct ModifierId<'db> {
    #[id]
    pub name: Ident<'db>,
    pub info: Modifier<'db>,
    pub body_node: Option<AstPtr<nodes::Block>>,

    pub node: AstPtr<nodes::ModifierDefinition>,
}

#[salsa::tracked]
impl<'db> ModifierId<'db> {
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn BaseDb) -> Option<(StatementId<'db>, ItemSourceMap<'db>)> {
        let mut origin = self.origin(db);
        let file = self.file(db);
        let tree = file.tree(db);
        let root = tree.syntax();
        let node = self.body_node(db)?;
        
        let expr = node.to_node(&root);

        let mut lowerer = LowerCtx::new(db, file);

        let res = lowerer.lower_stmt(Stmt::Block(expr));

        return Some((res, ItemSourceMap::new(lowerer.exprs, lowerer.stmts)));
    }

    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb, project: Project) -> ExprScopeRoot<'db> {
        resolve_scopes(
            db, 
            project, 
            self.item_origin(db).scope(db, project), 
            self.info(db).args.iter().copied(), 
            self.body(db).map(|a| a.0)
        )
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

lazy_field!(ModifierId<'db>, origin, set_origin, ItemOrigin<'db>);

#[derive(Eq, PartialEq, Clone, Hash, salsa::Update)]
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
