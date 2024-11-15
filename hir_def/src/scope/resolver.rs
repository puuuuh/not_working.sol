use crate::hir::expr::ExprId;
use crate::hir::function::FunctionId;
use crate::hir::statement::{Statement, StatementId};
use crate::scope::expr::{DefinitionSite, ExprScopeData, ExprScopeRoot};
use crate::scope::IndexMapUpdate;
use base_db::BaseDb;
use indexmap::IndexMap;
use salsa::Database;

pub struct Resolver<'db> {
    db: &'db dyn Database,
    scopes: Vec<ExprScopeData<'db>>,
    scope_item_cnt: Vec<usize>,
    scope_by_expr: IndexMap<ExprId<'db>, usize>,
    scope_by_stmt: IndexMap<StatementId<'db>, usize>,
}

impl<'db> Resolver<'db> {
    pub fn function_scopes(db: &'db dyn BaseDb, f: FunctionId<'db>) -> ExprScopeRoot<'db> {
        let info = f.info(db);
        let root_items = info
            .args
            .iter()
            .filter_map(|a| a.name(db).map(move |n| (n, DefinitionSite::Argument(*a))))
            .collect();
        let mut resolver = Resolver {
            db: db.as_dyn_database(),
            scopes: vec![ExprScopeData { parent: None, items: root_items }],
            scope_item_cnt: vec![0],
            scope_by_expr: Default::default(),
            scope_by_stmt: Default::default(),
        };
        // let stmts = f.body(self.db).iter().map(|a| (*a, 0)).collect();
        if let Some(s) = f.body(db) {
            resolver.walk_stmt(s);
        };

        if resolver.scope_item_cnt.last() == Some(&0) {
            resolver.scopes.pop();
        }
        resolver.scopes.shrink_to_fit();
        resolver.scope_by_expr.shrink_to_fit();
        resolver.scope_by_stmt.shrink_to_fit();

        ExprScopeRoot {
            parent: f.def_site(db.as_dyn_database()).scope(db),
            expr_scopes: resolver.scopes,
            scope_by_expr: IndexMapUpdate(resolver.scope_by_expr),
            scope_by_stmt: IndexMapUpdate(resolver.scope_by_stmt),
        }
    }

    fn new_scope(&mut self, parent: Option<usize>) {
        if let Some(last_scope) = self.scopes.last_mut() {
            if last_scope.items.is_empty() && self.scope_item_cnt.last() == Some(&0usize) {
                last_scope.parent = parent;
                return;
            }
        }

        self.scopes.push(ExprScopeData { parent, items: vec![] });
        self.scope_item_cnt.push(0);
    }

    fn walk_stmt(&mut self, s: StatementId<'db>) {
        let current_scope = self.scopes.len() - 1;

        self.scope_by_stmt.insert(s, current_scope);
        self.scope_item_cnt[current_scope] += 1;

        match s.kind(self.db) {
            Statement::Missing => {}
            Statement::VarDecl { items, init_expr } => {
                if let Some(e) = init_expr {
                    self.scope_by_expr.insert(*e, current_scope);
                    self.scope_item_cnt[current_scope] += 1;
                }
                let new_items = items
                    .iter()
                    .flatten()
                    .filter_map(|a| {
                        a.name(self.db).map(|name| (name, DefinitionSite::Argument(*a)))
                    })
                    .collect();

                self.new_scope(Some(current_scope));
                self.scopes.last_mut().unwrap().items = new_items;
            }
            Statement::Expr { expr } => expr.walk(self.db, &mut |e| {
                self.scope_by_expr.insert(e, current_scope);
                self.scope_item_cnt[current_scope] += 1;
            }),
            Statement::Block { stmts, is_unchecked: _ } => {
                self.new_scope(Some(current_scope));
                for s in stmts {
                    self.walk_stmt(*s);
                }
                self.new_scope(Some(current_scope));
            }
            Statement::If { cond, body, else_body } => {
                cond.walk(self.db, &mut |e| {
                    self.scope_by_expr.insert(e, current_scope);
                    self.scope_item_cnt[current_scope] += 1;
                });
                self.walk_stmt(*body);
                if let Some(else_body) = else_body {
                    self.walk_stmt(*else_body);
                }
            }
            Statement::ForLoop { init, cond, finish_action, body } => {
                if let Some(init) = init {
                    self.walk_stmt(*init);
                }
                if let Some(cond) = cond {
                    cond.walk(self.db, &mut |e| {
                        self.scope_by_expr.insert(e, current_scope);
                        self.scope_item_cnt[current_scope] += 1;
                    });
                }
                if let Some(finish) = finish_action {
                    finish.walk(self.db, &mut |e| {
                        self.scope_by_expr.insert(e, current_scope);
                        self.scope_item_cnt[current_scope] += 1;
                    });
                }
                self.walk_stmt(*body);
            }
            Statement::WhileLoop { cond, body } => {
                cond.walk(self.db, &mut |e| {
                    self.scope_by_expr.insert(e, current_scope);
                    self.scope_item_cnt[current_scope] += 1;
                });
                self.walk_stmt(*body);
            }
            Statement::DoWhileLoop { cond, body } => {
                self.walk_stmt(*body);

                cond.walk(self.db, &mut |e| {
                    self.scope_by_expr.insert(e, current_scope);
                    self.scope_item_cnt[current_scope] += 1;
                });
            }
            Statement::Try { expr, returns, body, catch } => {
                expr.walk(self.db, &mut |e| {
                    self.scope_by_expr.insert(e, current_scope);
                    self.scope_item_cnt[current_scope] += 1;
                });
                let new_items = returns
                    .iter()
                    .flatten()
                    .filter_map(|a| {
                        a.name(self.db).map(|name| (name, DefinitionSite::Argument(*a)))
                    })
                    .collect();

                self.new_scope(Some(current_scope));
                self.scopes.last_mut().unwrap().items = new_items;
                self.walk_stmt(*body);
                for c in catch {
                    self.new_scope(Some(current_scope));
                    let new_items = c
                        .args(self.db)
                        .iter()
                        .flatten()
                        .filter_map(|a| {
                            a.name(self.db).map(|name| (name, DefinitionSite::Argument(*a)))
                        })
                        .collect();
                    self.scopes.last_mut().unwrap().items = new_items;
                    self.walk_stmt(c.body(self.db));
                }
                self.new_scope(Some(current_scope));
            }
            Statement::Return { expr } => {
                if let Some(expr) = expr {
                    expr.walk(self.db, &mut |e| {
                        self.scope_by_expr.insert(e, current_scope);
                        self.scope_item_cnt[current_scope] += 1;
                    });
                }
            }
            Statement::Emit { event, args: _ } => {
                event.walk(self.db, &mut |e| {
                    self.scope_by_expr.insert(e, current_scope);
                    self.scope_item_cnt[current_scope] += 1;
                });
            }
            Statement::Revert { event, args: _ } => {
                event.walk(self.db, &mut |e| {
                    self.scope_by_expr.insert(e, current_scope);
                    self.scope_item_cnt[current_scope] += 1;
                });
            }
            Statement::Assembly {} => {}
            Statement::Continue {} => {}
            Statement::Break {} => {}
        }
    }
}
