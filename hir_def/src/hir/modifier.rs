use crate::hir::variable_declaration::VariableDeclaration;
use crate::hir::ident::{Ident, IdentPath};
use crate::hir::statement::StatementId;
use crate::hir::{ContractId, HasFile, HasSourceUnit, Item};
use crate::items::HirPrint;
use crate::lower::LowerCtx;
use crate::nameres::scope::BodyScope;
use crate::source_map::item_source_map::ItemSourceMap;
use crate::{impl_major_item, lazy_field, FileAstPtr, FileExt};
use base_db::{BaseDb, Project};
use rowan::ast::AstPtr;
use rowan::ast::AstNode;
use salsa::{tracked, Database};
use vfs::File;
use std::fmt::Write;
use syntax::ast::nodes::{self, Stmt};

#[tracked(debug)]
pub struct ModifierId<'db> {
    #[id]
    pub name: Ident<'db>,
    pub info: Modifier<'db>,
    pub body_node: Option<AstPtr<nodes::Block>>,

    pub node: AstPtr<nodes::ModifierDefinition>,
}

lazy_field!(ModifierId<'db>, origin, set_origin, Option<ContractId<'db>>, None);

#[salsa::tracked]
impl<'db> ModifierId<'db> {
    #[salsa::tracked]
    pub fn body(self, db: &'db dyn BaseDb, module: File) -> Option<(StatementId<'db>, ItemSourceMap<'db>)> {
        let mut origin = self.origin(db);
        let tree = module.node(db);
        let root = tree.syntax();
        let node = self.body_node(db)?;
        
        let expr = node.to_node(&root);

        let mut lowerer = LowerCtx::new(db, module);

        let res = lowerer.lower_stmt(Stmt::Block(expr));

        return Some((res, ItemSourceMap::new(crate::IndexMapUpdate(lowerer.exprs), crate::IndexMapUpdate(lowerer.stmts))));
    }

    #[salsa::tracked]
    pub fn scope(self, db: &'db dyn BaseDb, project: Project, module: File) -> BodyScope<'db> {
        BodyScope::from_body(
            db, 
            project, 
            self.origin(db)
                .map(|c| c.scope(db, project, module))
                .unwrap_or_else(|| module.source_unit(db).scope(db, project, module)), 
            Item::Modifier(self),
            self.info(db).args.iter().copied(), 
            self.body(db, module).map(|a| a.0)
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
