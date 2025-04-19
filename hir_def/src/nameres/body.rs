use crate::hir::EnumerationVariantId;
use crate::hir::ExprId;
use crate::hir::FunctionId;
use crate::hir::HasDefs;
use crate::hir::HasSyntax;
use crate::hir::Ident;
use crate::hir::Item;
use crate::hir::StructureFieldId;
use crate::hir::VariableDeclaration;
use crate::hir::{Statement, StatementId};
use crate::walk::walk_stmt;
use crate::walk::Visitor;
use crate::IndexMapUpdate;
use base_db::{BaseDb, Project};
use indexmap::IndexMap;
use salsa::Database;
use vfs::File;

use super::scope::ItemScope;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, salsa::Update)]
pub enum StmtOrItem<'db> {
    Stmt(StatementId<'db>),
    Item(Item<'db>)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, salsa::Update)]
pub enum Definition<'db> {
    Item((File, Item<'db>)),
    Local((StmtOrItem<'db>, VariableDeclaration<'db>)),
    Field(StructureFieldId<'db>),
    EnumVariant(EnumerationVariantId<'db>)
}

impl<'db> HasDefs<'db> for Definition<'db> {
    fn defs(self, db: &'db dyn BaseDb, module: File) -> Vec<(Ident<'db>, Definition<'db>)> {
        match self {
            Definition::Item((module, item)) => item.defs(db, module),
            _ => vec![]
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct ExprScope<'db> {
    pub parent: Option<usize>,
    pub items: Vec<(Ident<'db>, Definition<'db>)>,
}

#[salsa::tracked(debug)]
pub struct BodyScope<'db> {
    pub parent: ItemScope<'db>,
    #[return_ref]
    pub expr_scopes: Vec<ExprScope<'db>>,
    #[return_ref]
    pub scope_by_expr: IndexMapUpdate<ExprId<'db>, usize>,
    #[return_ref]
    pub scope_by_stmt: IndexMapUpdate<StatementId<'db>, usize>,
}

#[salsa::tracked]
impl<'db> BodyScope<'db> {
    pub fn from_body(db: &'db dyn BaseDb, project: Project, parent: ItemScope<'db>, item: Item<'db>, args: impl Iterator<Item = VariableDeclaration<'db>>, body: Option<StatementId<'db>>) -> BodyScope<'db> {
        resolve_body(db, project, parent, item, args, body)
    }

    #[salsa::tracked]
    pub fn lookup_in_expr(
        self,
        db: &'db dyn Database,
        expr: ExprId<'db>,
        name: Ident<'db>,
    ) -> Option<Definition<'db>> {
        let scope = *self.scope_by_expr(db).0.get(&expr)?;
        self.lookup_in_scope(db, scope, name)
    }

    #[salsa::tracked]
    pub fn lookup_in_scope(
        self,
        db: &'db dyn Database,
        scope: usize,
        name: Ident<'db>,
    ) -> Option<Definition<'db>> {
        let mut scope = Some(scope);
        while let Some(scope_id) = scope {
            let s = &self.expr_scopes(db)[scope_id];
            if let Some(t) = s.items.iter().find(|(n, _)| *n == name) {
                return Some(t.1);
            };
            scope = s.parent;
        }

        self.parent(db).lookup(db, name).iter().next().copied().map(Definition::Item)
    }
}

pub struct ScopeResolver<'db> {
    db: &'db dyn Database,
    scopes: Vec<ExprScope<'db>>,
    scope_item_cnt: Vec<usize>,
    scope_by_expr: IndexMap<ExprId<'db>, usize>,
    scope_by_stmt: IndexMap<StatementId<'db>, usize>,
}

fn resolve_body<'db>(db: &'db dyn BaseDb, project: Project, parent: ItemScope<'db>, item: Item<'db>, args: impl Iterator<Item = (VariableDeclaration<'db>)>, body: Option<StatementId<'db>>) -> BodyScope<'db> {
    let root_items = args
        .filter_map(|a| a.name(db).map(move |n| (n, Definition::Local((StmtOrItem::Item(item), a)))))
        .collect();

    let mut resolver = ScopeResolver {
        db,
        scopes: vec![ExprScope { parent: None, items: root_items }],
        scope_item_cnt: vec![0],
        scope_by_expr: Default::default(),
        scope_by_stmt: Default::default(),
    };

    if let Some(body) = body {
        walk_stmt(db, body, &mut resolver);
    }

    if resolver.scope_item_cnt.last() == Some(&0) {
        resolver.scopes.pop();
    }
    resolver.scopes.shrink_to_fit();
    resolver.scope_by_expr.shrink_to_fit();
    resolver.scope_by_stmt.shrink_to_fit();

    BodyScope::new(
        db, 
        parent,
        resolver.scopes, 
        IndexMapUpdate(resolver.scope_by_expr), 
        IndexMapUpdate(resolver.scope_by_stmt))
}

impl<'db> ScopeResolver<'db> {
    fn new_scope(&mut self, parent: Option<usize>) {
        if let Some(last_scope) = self.scopes.last_mut() {
            if last_scope.items.is_empty() && self.scope_item_cnt.last() == Some(&0usize) {
                last_scope.parent = parent;
                return;
            }
        }

        self.scopes.push(ExprScope { parent, items: vec![] });
        self.scope_item_cnt.push(0);
    }

}

impl<'db> Visitor<'db> for &mut ScopeResolver<'db> {
    type Ctx = usize;
    type ExprCtx = ();

    fn stmt_start(&mut self, db: &'db dyn BaseDb, s: StatementId<'db>) -> (bool, Self::Ctx) {
        let current_scope = self.scopes.len() - 1;

        self.scope_by_stmt.insert(s, current_scope);
        self.scope_item_cnt[current_scope] += 1;

        match s.kind(self.db) {
            Statement::Block { stmts, is_unchecked: _ } => {
                self.new_scope(Some(current_scope));
            }
            Statement::Try { expr, returns, body, catch } => {
                let new_items = returns
                    .iter()
                    .flatten()
                    .filter_map(|a| {
                        a.name(self.db).map(|name| (name, Definition::Local((StmtOrItem::Stmt(s), *a))))
                    })
                    .collect();

                self.new_scope(Some(current_scope));
                self.scopes.last_mut().unwrap().items = new_items;
            }
            _ => {}
        }
        
        (true, current_scope)
    }
    
    fn stmt_end(&mut self, db: &'db dyn BaseDb, ctx: Self::Ctx, s: StatementId<'db>) {
        match s.kind(self.db) {
            Statement::VarDecl { items, init_expr } => {
                let new_items = items
                    .iter()
                    .flatten()
                    .filter_map(|a| {
                        a.name(self.db).map(|name| (name, Definition::Local((StmtOrItem::Stmt(s), *a))))
                    })
                    .collect();

                self.new_scope(Some(ctx));
                self.scopes.last_mut().unwrap().items = new_items;
            }
            Statement::Block { .. } |
                Statement::Try { .. } => {
                self.new_scope(Some(ctx));
            }
            _ => {}
        }
    }
    
    fn expr_start(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> (bool, Self::ExprCtx) {
        let current_scope = self.scopes.len() - 1;
        self.scope_by_expr.insert(expr, current_scope);
        self.scope_item_cnt[current_scope] += 1;
        return (true, ())
    }
    
    fn expr_end(&mut self, db: &'db dyn BaseDb, ctx: Self::ExprCtx, expr: ExprId<'db>) {}
}