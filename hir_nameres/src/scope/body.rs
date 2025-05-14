use std::collections::btree_map;
use std::collections::BTreeMap;
use std::ops::Range;
use std::sync::Arc;
use std::thread::current;

use base_db::{BaseDb, Project};
use hir_def::IndexMapUpdate;
use hir_def::hir::EnumerationVariantId;
use hir_def::hir::ExprId;
use hir_def::hir::FunctionId;
use hir_def::hir::HasSyntax;
use hir_def::hir::Ident;
use hir_def::hir::Item;
use hir_def::hir::StructureFieldId;
use hir_def::hir::VariableDeclaration;
use hir_def::hir::{Statement, StatementId};
use hir_def::walk::Visitor;
use hir_def::walk::walk_stmt;
use indexmap::IndexMap;
use salsa::Database;
use smallvec::SmallVec;
use smallvec::smallvec;
use vfs::File;

use crate::scope::ItemScope;

use super::Scope;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, salsa::Update)]
pub enum Definition<'db> {
    Item((Item<'db>)),
    Local(VariableDeclaration<'db>),
    Field(StructureFieldId<'db>),
    EnumVariant(EnumerationVariantId<'db>),
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub struct ExprScope {
    pub parent: Option<usize>,
    pub range: Range<usize>,
}

#[salsa::tracked(debug)]
pub struct BodyScope<'db> {
    pub parent: ItemScope<'db>,
    #[returns(ref)]
    pub definitions: Vec<(Ident<'db>, Definition<'db>)>,
    #[returns(ref)]
    pub expr_scopes: SmallVec<[ExprScope; 1]>,
    #[returns(ref)]
    pub scope_by_expr: BTreeMap<ExprId<'db>, usize>,
    #[returns(ref)]
    pub scope_by_stmt: BTreeMap<StatementId<'db>, usize>,
}

#[salsa::tracked]
impl<'db> BodyScope<'db> {
    pub fn from_body(
        db: &'db dyn BaseDb,
        project: Project,
        parent: ItemScope<'db>,
        item: Item<'db>,
        args: SmallVec<[VariableDeclaration<'db>; 8]>,
        body: Option<StatementId<'db>>,
    ) -> BodyScope<'db> {
        resolve_body(db, project, parent, item, args, body)
    }

    #[salsa::tracked]
    pub fn all_definitions(self, db: &'db dyn BaseDb, scope: usize) -> BTreeMap<Ident<'db>, SmallVec<[Definition<'db>; 1]>> {
        let mut res = BTreeMap::new();
        let scopes = self.expr_scopes(db);
        let defintions = self.definitions(db);
        let mut s = scopes.get(scope).cloned();
        while let Some(scope) = s {
            let items = &defintions[scope.range];
            for (name, items) in items {
                if let btree_map::Entry::Vacant(v) = res.entry(*name) {
                    v.insert(smallvec![items.clone()]);
                }
            }

            s = scope.parent.map(|scope| scopes[scope].clone());
        }

        for (name, items) in self.parent(db).all_definitions(db) {
            if let btree_map::Entry::Vacant(v) = res.entry(name) {
                v.insert(items.clone());
            }
        }

        res
    }

    #[salsa::tracked]
    pub fn find_in_expr(
        self,
        db: &'db dyn BaseDb,
        expr: ExprId<'db>,
        name: Ident<'db>,
    ) -> Option<Definition<'db>> {
        let scope = self.scope_by_expr(db).get(&expr).copied().unwrap_or_default();
        self.find(db, scope, name)
    }

    #[salsa::tracked]
    pub fn find(
        self,
        db: &'db dyn BaseDb,
        scope: usize,
        name: Ident<'db>,
    ) -> Option<Definition<'db>> {
        let scopes = self.expr_scopes(db);
        let defintions = self.definitions(db);
        let mut s = scopes.get(scope).cloned();
        while let Some(scope) = s {
            let items = &defintions[scope.range];
            if let Some(def) = items.iter().find_map(|(n, item)| (*n == name).then_some(item)) {
                return Some(*def);
            }

            s = scope.parent.map(|scope| scopes[scope].clone());
        }

        return self.parent(db).find(db, name);
    }

    #[salsa::tracked]
    pub fn find_all(
        self,
        db: &'db dyn BaseDb,
        scope: usize,
        name: Ident<'db>,
    ) -> SmallVec<[Definition<'db>; 1]> {
        let mut res = SmallVec::new();
        let scopes = self.expr_scopes(db);
        let defintions = self.definitions(db);
        let mut s = scopes.get(scope).cloned();
        while let Some(scope) = s {
            let items = &defintions[scope.range];
            for i in items.iter().filter_map(|(n, item)| (*n == name).then_some(item)) {
                res.push(*i);
            }

            s = scope.parent.map(|scope| scopes[scope].clone());
        }

        res.extend(self.parent(db).find_all(db, name));

        res
    }
}

pub struct ScopeResolver<'db> {
    db: &'db dyn Database,
    scopes: SmallVec<[ExprScope; 1]>,
    definitions: Vec<(Ident<'db>, Definition<'db>)>,
    scope_item_cnt: Vec<usize>,
    scope_by_expr: BTreeMap<ExprId<'db>, usize>,
    scope_by_stmt: BTreeMap<StatementId<'db>, usize>,
}

#[salsa::tracked]
fn resolve_body<'db>(
    db: &'db dyn BaseDb,
    project: Project,
    parent: ItemScope<'db>,
    item: Item<'db>,
    args: SmallVec<[VariableDeclaration<'db>; 8]>,
    body: Option<StatementId<'db>>,
) -> BodyScope<'db> {
    let root_items: Vec<_> = args
        .into_iter()
        .filter_map(|a| {
            a.name(db).map(move |n| (n, Definition::Local(a)))
        })
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

    BodyScope::new(
        db,
        parent,
        resolver.definitions,
        resolver.scopes,
        resolver.scope_by_expr,
        resolver.scope_by_stmt,
    )
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

        self.scopes
            .push(ExprScope { parent, range: self.definitions.len()..self.definitions.len() });
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
                for item in returns.iter().flatten().filter_map(|a| {
                    a.name(self.db).map(|name| (name, Definition::Local(*a)))
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
                for item in items.iter().flatten().filter_map(|a| {
                    a.name(self.db).map(|name| (name, Definition::Local(*a)))
                }) {
                    self.add_item(item);
                }
            }
            Statement::Block { .. } | Statement::Try { .. } => {
                self.new_scope(Some(ctx));
            }
            _ => {}
        }
    }

    fn expr_start(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> (bool, Self::ExprCtx) {
        let current_scope = self.scopes.len() - 1;
        self.scope_by_expr.insert(expr, current_scope);
        self.scope_item_cnt[current_scope] += 1;
        return (true, ());
    }

    fn expr_end(&mut self, db: &'db dyn BaseDb, ctx: Self::ExprCtx, expr: ExprId<'db>) {}
}
