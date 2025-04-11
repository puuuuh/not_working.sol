use crate::hir::VariableDeclaration;
use crate::hir::ExprId;
use crate::hir::Ident;
use crate::hir::Item;
use crate::hir::StatementId;
use crate::scope::item::ItemScope;
use crate::scope::IndexMapUpdate;
use salsa::Database;

#[derive(Clone, Copy, Eq, PartialEq, Hash, salsa::Update)]
pub enum DefinitionSite<'db> {
    Item(Item<'db>),
    Local(VariableDeclaration<'db>),
}

#[derive(Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct ExprScopeData<'db> {
    pub parent: Option<usize>,
    pub items: Vec<(Ident<'db>, DefinitionSite<'db>)>,
}

#[salsa::tracked]
pub struct ExprScopeRoot<'db> {
    pub parent: ItemScope<'db>,
    #[return_ref]
    pub expr_scopes: Vec<ExprScopeData<'db>>,
    #[return_ref]
    pub scope_by_expr: IndexMapUpdate<ExprId<'db>, usize>,
    #[return_ref]
    pub scope_by_stmt: IndexMapUpdate<StatementId<'db>, usize>,
}

#[salsa::tracked]
impl<'db> ExprScopeRoot<'db> {
    #[salsa::tracked]
    pub fn lookup(
        self,
        db: &'db dyn Database,
        expr: ExprId<'db>,
        name: Ident<'db>,
    ) -> Option<DefinitionSite<'db>> {
        let scope = *self.scope_by_expr(db).0.get(&expr)?;
        self.lookup_in_scope(db, scope, name)
    }

    #[salsa::tracked]
    pub fn lookup_in_scope(
        self,
        db: &'db dyn Database,
        scope: usize,
        name: Ident<'db>,
    ) -> Option<DefinitionSite<'db>> {
        let mut scope = Some(scope);
        while let Some(scope_id) = scope {
            let s = &self.expr_scopes(db)[scope_id];
            dbg!(&s.parent);
            for t in &s.items {
                dbg!(t.0.data(db));
            }
            if let Some(t) = s.items.iter().find(|(n, _)| *n == name) {
                return Some(t.1);
            };
            scope = s.parent;
        }

        self.parent(db).lookup(db, name).map(DefinitionSite::Item)
    }
}
