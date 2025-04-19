use base_db::BaseDb;
use smallvec::SmallVec;

use crate::hir::{Expr, ExprId, Statement, StatementId};

pub trait Visitor<'db> {
    type Ctx;
    type ExprCtx;

    fn stmt_start(&mut self, db: &'db dyn BaseDb, stmt: StatementId<'db>) -> (bool, Self::Ctx);

    fn stmt_end(&mut self, db: &'db dyn BaseDb, ctx: Self::Ctx, stmt: StatementId<'db>) {}

    fn expr_start(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> (bool, Self::ExprCtx);

    fn expr_end(&mut self, db: &'db dyn BaseDb, ctx: Self::ExprCtx, expr: ExprId<'db>) {}
}

pub enum StmtOrExpr<'db> {
    Stmt(StatementId<'db>),
    Expr(ExprId<'db>)
}

pub struct StmtVisitor<'db, VisitorImpl: Visitor<'db>> {
    stack: SmallVec<[StmtOrExpr<'db>; 24]>,

    handler: VisitorImpl,
}

impl<'db, VisitorImpl: Visitor<'db>> StmtVisitor<'db, VisitorImpl> {
    fn visit_expr(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) {
        let (descend, ctx) = self.handler.expr_start(db, expr);
        if !descend {
            return;
        }
        self.stack.push(StmtOrExpr::Expr(expr));

        match expr.kind(db) {
            Expr::Index { target, index } => {
                self.visit_expr(db, *target);
                self.visit_expr(db, *index);
            }
            Expr::Slice { base, start, end } => {
                self.visit_expr(db, *base);
                if let Some(start) = start {
                    self.visit_expr(db, *start);
                }

                if let Some(end) = end {
                    self.visit_expr(db, *end);
                }
            },
            Expr::MemberAccess { owner, member_name } => {
                self.visit_expr(db, *owner);
            },
            Expr::CallOptions { base, options } => {
                self.visit_expr(db, *base);
                for opt in options {
                    self.visit_expr(db, opt.val);
                }
            },
            Expr::Call { callee, args } => {
                self.visit_expr(db, *callee);
                for (_, a) in args {
                    self.visit_expr(db, *a);
                }
            },
            Expr::PrefixOp { expr, op } => {
                self.visit_expr(db, *expr);
            },
            Expr::PostfixOp { expr, op } => {
                self.visit_expr(db, *expr);
            },
            Expr::BinaryOp { lhs, op, rhs } => {
                self.visit_expr(db, *lhs);
                self.visit_expr(db, *rhs);
            },
            Expr::Ternary { cond, lhs, rhs } => {
                self.visit_expr(db, *cond);
                self.visit_expr(db, *lhs);
                self.visit_expr(db, *rhs);
            }
            Expr::Tuple { content } => {
                for c in content {
                    self.visit_expr(db, *c);
                }
            },
            Expr::Array { content } => {
                for c in content {
                    self.visit_expr(db, *c);
                }
            },
            Expr::Ident { name_ref } => {},
            Expr::Literal { data } => {},
            Expr::ElementaryTypeName { data } => {},
            Expr::New { ty } => {},
            Expr::Missing => {},
        };

        self.stack.pop();

        self.handler.expr_end(db, ctx, expr);
    }

    fn visit_stmt(&mut self, db: &'db dyn BaseDb, stmt: StatementId<'db>) {
        let (descend, ctx) = self.handler.stmt_start(db, stmt);
        if !descend {
            return;
        }

        self.stack.push(StmtOrExpr::Stmt(stmt));

        match stmt.kind(db) {
            Statement::Missing => {},
            Statement::VarDecl { items, init_expr } => {
                if let Some(init) = init_expr {
                    self.visit_expr(db, *init);
                }
            },
            Statement::Expr { expr } => {
                self.visit_expr(db, *expr);
            },
            Statement::Block { stmts, is_unchecked } => {
                for s in stmts {
                    self.visit_stmt(db, *s);
                }
            },
            Statement::If { cond, body, else_body } => {
                self.visit_stmt(db, *body);
                if let Some(else_body) = else_body {
                    self.visit_stmt(db, *body);
                }
            },
            Statement::ForLoop { init, cond, finish_action, body } => {
                if let Some(init) = init {
                    self.visit_stmt(db, *init);
                }
                if let Some(expr) = cond {
                    self.visit_expr(db, *expr)
                }
            },
            Statement::WhileLoop { cond, body } => {
                self.visit_expr(db, *cond);
                self.visit_stmt(db, *body);
            },
            Statement::DoWhileLoop { cond, body } => {
                self.visit_expr(db, *cond);
                self.visit_stmt(db, *body);
            },
            Statement::Try { expr, returns, body, catch } => {
                self.visit_expr(db, *expr);
                self.visit_stmt(db, *body);
                for c in catch {
                    self.visit_stmt(db, c.body(db));
                }
            },
            Statement::Return { expr } => {
                if let Some(expr) = expr {
                    self.visit_expr(db, *expr);
                }
            },
            Statement::Emit { event, args } => {
                self.visit_expr(db, *event);
                for (_, val) in args {
                    self.visit_expr(db, *val);
                }
            },
            Statement::Revert { event, args } => {
                self.visit_expr(db, *event);
                for (_, val) in args {
                    self.visit_expr(db, *val);
                }
            },
            Statement::Assembly {  } => {},
            Statement::Continue {  } => {},
            Statement::Break {  } => {},
        };
        self.stack.pop();

        self.handler.stmt_end(db, ctx, stmt);
    }
}

pub fn walk_expr<'db>(db: &'db dyn BaseDb, expr: ExprId<'db>, handler: impl Visitor<'db>) {
    StmtVisitor {
        stack: Default::default(),
        handler,
    }.visit_expr(db, expr);
}

pub fn walk_stmt<'db>(db: &'db dyn BaseDb, stmt: StatementId<'db>, handler: impl Visitor<'db>) {
    StmtVisitor {
        stack: Default::default(),
        handler
    }.visit_stmt(db, stmt);
}