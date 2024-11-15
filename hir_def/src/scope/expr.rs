use crate::hir::argument::ArgumentId;
use crate::hir::expr::ExprId;
use crate::hir::ident::Ident;
use crate::hir::statement::StatementId;
use crate::scope::item::Scope;
use crate::scope::IndexMapUpdate;
use salsa::Database;

#[derive(Clone, Copy, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub enum DefinitionSite<'db> {
    Statement(StatementId<'db>),
    Argument(ArgumentId<'db>),
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub struct ExprScopeData<'db> {
    pub parent: Option<usize>,
    pub items: Vec<(Ident<'db>, DefinitionSite<'db>)>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub struct ExprScopeRoot<'db> {
    pub parent: Scope<'db>,
    pub expr_scopes: Vec<ExprScopeData<'db>>,
    pub scope_by_expr: IndexMapUpdate<ExprId<'db>, usize>,
    pub scope_by_stmt: IndexMapUpdate<StatementId<'db>, usize>,
}

impl<'db> ExprScopeRoot<'db> {
    pub fn lookup(
        &self,
        _db: &'db dyn Database,
        scope: usize,
        name: Ident<'db>,
    ) -> Option<DefinitionSite<'db>> {
        let mut scope = Some(scope);
        while let Some(scope_id) = scope {
            let s = &self.expr_scopes[scope_id];
            if let Some(t) = s.items.iter().find(|(n, _)| *n == name) {
                return Some(t.1);
            };
            scope = s.parent;
        }

        None
    }
}
