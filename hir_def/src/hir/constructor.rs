use crate::hir::variable_declaration::VariableDeclaration;
use crate::hir::function::ModifierInvocation;
use crate::hir::source_unit::ItemOrigin;
use crate::hir::statement::StatementId;
use crate::hir::HasOrigin;
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
use syntax::ast::nodes::{self, ConstructorDefinition};

use super::state_mutability::StateMutability;
use super::visibility::Visibility;
use super::HasFile;

#[tracked]
pub struct ConstructorId<'db> {
    #[salsa::tracked]
    pub info: Constructor<'db>,
    
    #[salsa::tracked]
    pub body_node: Option<AstPtr<nodes::Block>>,

    #[salsa::tracked]
    pub node: AstPtr<nodes::ConstructorDefinition>,
}

lazy_field!(ConstructorId<'db>, origin, set_origin, ItemOrigin<'db>);

#[salsa::tracked]
impl<'db> ConstructorId<'db> {
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
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn BaseDb) -> Option<(StatementId<'db>, ItemSourceMap<'db>)> {
        let mut origin = self.origin(db);
        let node = self.body_node(db)?;
        let file = self.file(db);
        let root = file.tree(db);
        let root = root.syntax();
        
        let expr = node.to_node(&root);

        let mut lowerer = LowerCtx::new(db, file);

        let res = lowerer.lower_stmt(nodes::Stmt::Block(expr));

        return Some((res, ItemSourceMap::new(lowerer.exprs, lowerer.stmts)));
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

#[derive(Eq, PartialEq, Clone, Hash, salsa::Update)]
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
