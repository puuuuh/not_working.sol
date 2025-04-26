use std::ops::Range;
use std::sync::Arc;
use std::thread::current;

use hir_def::hir::EnumerationVariantId;
use hir_def::hir::ExprId;
use hir_def::hir::FunctionId;
use hir_def::hir::HasSyntax;
use hir_def::hir::Ident;
use hir_def::hir::Item;
use hir_def::hir::StructureFieldId;
use hir_def::hir::VariableDeclaration;
use hir_def::hir::{Statement, StatementId};
use hir_def::walk::walk_stmt;
use hir_def::walk::Visitor;
use hir_def::IndexMapUpdate;
use base_db::{BaseDb, Project};
use indexmap::IndexMap;
use salsa::Database;
use smallvec::smallvec;
use smallvec::SmallVec;
use vfs::File;

use crate::scope::ItemScope;

use super::item::ItemScopeIter;
use super::Scope;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, salsa::Update)]
pub enum StmtOrItem<'db> {
    Stmt(StatementId<'db>),
    Item(Item<'db>)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, salsa::Update)]
pub enum Definition<'db> {
    Item((Item<'db>)),
    Local((StmtOrItem<'db>, VariableDeclaration<'db>)),
    Field(StructureFieldId<'db>),
    EnumVariant(EnumerationVariantId<'db>)
}

pub struct BodyScopeIter<'db> {
    item: ItemScopeIter<'db>,
    db: &'db dyn Database,
    current: ExprScope,
    scopes: &'db [ExprScope],
    definitions: &'db [(Ident<'db>, Definition<'db>)],
    name: Option<Ident<'db>>
}

impl<'db> BodyScopeIter<'db> {
    #[inline(always)]
    fn next_inner(&mut self) -> Option<(Ident<'db>, Definition<'db>)> {
        while self.current.range.is_empty() {
            if let Some(p) = self.current.parent {
                self.current = self.scopes[p].clone();
            } else {
                return self.item.next().map(|(name, item)| (name, item));
            }
        }

        self.current.range.end -= 1;

        let t = Some(self.definitions[self.current.range.end]);

        t
    }
}

impl<'db> Iterator for BodyScopeIter<'db> {
    type Item = (Ident<'db>, Definition<'db>);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(t) = self.next_inner() {
                if let Some(name) = self.name {
                    if t.0 == name {
                        return Some(t);
                    }
                } else {
                    return Some(t)
                }
            } else {
                return self.item.next().map(|(name, item)| (name, item) );
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct ExprScope {
    pub parent: Option<usize>,
    pub range: Range<usize>,
}

#[salsa::tracked(debug)]
pub struct BodyScope<'db> {
    pub parent: ItemScope<'db>,
    #[return_ref]
    pub definitions: SmallVec<[(Ident<'db>, Definition<'db>); 2]>,
    #[return_ref]
    pub expr_scopes: SmallVec<[ExprScope; 1]>,
    #[return_ref]
    pub scope_by_expr: IndexMapUpdate<ExprId<'db>, usize>,
    #[return_ref]
    pub scope_by_stmt: IndexMapUpdate<StatementId<'db>, usize>,
}

impl<'db> BodyScope<'db> {
    pub fn from_body(
        db: &'db dyn BaseDb, 
        project: Project, 
        parent: ItemScope<'db>, 
        item: Item<'db>, 
        args: impl Iterator<Item = VariableDeclaration<'db>>, 
        body: Option<StatementId<'db>>) -> BodyScope<'db> {

        resolve_body(db, project, parent, item, args, body)
    }

    pub fn lookup_in_expr(
        self,
        db: &'db dyn BaseDb,
        expr: ExprId<'db>,
        name: Ident<'db>,
    ) -> BodyScopeIter<'db> {
        let scope = self.scope_by_expr(db).0.get(&expr).copied().unwrap_or_default();
        self.lookup_in_scope(db, scope, name)
    }

    pub fn iter_in_scope(
        self,
        db: &'db dyn BaseDb,
        scope: usize,
    ) -> BodyScopeIter<'db> {
        BodyScopeIter { 
            name: None, 
            db, 
            current: self.expr_scopes(db)[scope].clone(), 
            scopes: self.expr_scopes(db), 
            definitions: self.definitions(db), 
            item: self.parent(db).iter(db)  
        }
    }

    pub fn lookup_in_scope(
        self,
        db: &'db dyn BaseDb,
        scope: usize,
        name: Ident<'db>,
    ) -> BodyScopeIter<'db> {
        BodyScopeIter { 
            name: Some(name), 
            db, 
            current: self.expr_scopes(db)[scope].clone(), 
            scopes: self.expr_scopes(db), 
            definitions: self.definitions(db), 
            item: self.parent(db).name(db, name)  }
    }
}

pub struct ScopeResolver<'db> {
    db: &'db dyn Database,
    scopes: SmallVec<[ExprScope; 1]>,
    definitions: SmallVec<[(Ident<'db>, Definition<'db>); 2]>,
    scope_item_cnt: Vec<usize>,
    scope_by_expr: IndexMap<ExprId<'db>, usize>,
    scope_by_stmt: IndexMap<StatementId<'db>, usize>,
}

fn resolve_body<'db>(db: &'db dyn BaseDb, project: Project, parent: ItemScope<'db>, item: Item<'db>, args: impl Iterator<Item = (VariableDeclaration<'db>)>, body: Option<StatementId<'db>>) -> BodyScope<'db> {
    let root_items: SmallVec<[(Ident<'_>, Definition<'_>); 2]> = args
        .filter_map(|a| a.name(db).map(move |n| (n, Definition::Local((StmtOrItem::Item(item), a)))))
        .collect();

    let root_items_len = root_items.len();

    let mut resolver = ScopeResolver {
        db,
        definitions: root_items,
        scopes: smallvec![ExprScope { parent: None, range: 0..root_items_len }],
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
        resolver.definitions,
        resolver.scopes, 
        IndexMapUpdate(resolver.scope_by_expr), 
        IndexMapUpdate(resolver.scope_by_stmt))
}

impl<'db> ScopeResolver<'db> {
    fn add_item(&mut self, item: (Ident<'db>, Definition<'db>)) {
        self.definitions.push(item);
        if let Some(s) = self.scopes.last_mut() {
            s.range.end += 1;
        } else {
            unreachable!()
        }
    }

    fn new_scope(&mut self, parent: Option<usize>) {
        /*if let Some(last_scope) = self.scopes.last_mut() {
            if last_scope.range.is_empty() && self.scope_item_cnt.last() == Some(&0usize) {
                //last_scope.parent = parent;
                return;
            }
        }*/

        self.scopes.push(ExprScope { parent, range: self.definitions.len()..self.definitions.len() });
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
                self.new_scope(Some(current_scope));
                for item in returns
                    .iter()
                    .flatten()
                    .filter_map(|a| {
                        a.name(self.db).map(|name| (name, Definition::Local((StmtOrItem::Stmt(s), *a))))
                    }) {
                    self.add_item(item);
                }
            }
            _ => {}
        }
        
        (true, current_scope)
    }
    
    fn stmt_end(&mut self, db: &'db dyn BaseDb, ctx: Self::Ctx, s: StatementId<'db>) {
        match s.kind(self.db) {
            Statement::VarDecl { items, init_expr } => {
                self.new_scope(Some(ctx));
                for item in items
                    .iter()
                    .flatten()
                    .filter_map(|a| {
                        a.name(self.db).map(|name| (name, Definition::Local((StmtOrItem::Stmt(s), *a))))
                    }) {
                    self.add_item(item);
                }
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