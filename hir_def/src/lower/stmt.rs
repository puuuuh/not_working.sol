use rowan::ast::{AstNode, AstPtr};
use crate::hir::ident::Ident;
use crate::hir::statement::{CatchClause, Statement, StatementId};
use crate::lower::LowerCtx;
use crate::FileAstPtr;
use syntax::ast::nodes;
use syntax::ast::nodes::VariableDeclarationItem;

impl<'db> LowerCtx<'db> {
    pub fn lower_stmt2(&mut self, expr: Option<nodes::Stmt>) -> StatementId<'db> {
        expr.map(|a| self.lower_stmt(a)).unwrap_or(StatementId::new(
            self.db,
            Statement::Missing,
            None,
        ))
    }

    pub fn lower_stmt(&mut self, expr: nodes::Stmt) -> StatementId<'db> {
        let res = StatementId::new(
            self.db,
            match &expr {
                nodes::Stmt::Block(block) => {
                    let stmts = block.stmts().map(|a| self.lower_stmt(a)).collect();
                    Statement::Block { stmts, is_unchecked: block.unchecked_token().is_some() }
                }
                nodes::Stmt::ExprStmt(a) => Statement::Expr { expr: self.lower_expr2(a.expr()) },
                nodes::Stmt::IfStmt(a) => Statement::If {
                    cond: self.lower_expr2(a.cond()),
                    body: self.lower_stmt2(a.then_body()),
                    else_body: a.else_body().map(|a| self.lower_stmt(a)),
                },
                nodes::Stmt::VariableDeclarationStmt(a) => {
                    let mut elements = Vec::new();
                    match a.variable_declaration_item() {
                        Some(VariableDeclarationItem::Parameter(a)) => {
                            elements.push(Some(self.lower_parameter(a)))
                        }
                        Some(VariableDeclarationItem::VariableTupleDeclaration(a)) => {
                            for p in a.variable_tuple_elements() {
                                elements.push(p.parameter().map(|p| self.lower_parameter(p)));
                            }
                        }
                        None => {}
                    };
                    let init_expr = a.init().map(|a| self.lower_expr(a));
                    Statement::VarDecl { items: elements, init_expr }
                }
                nodes::Stmt::ForStmt(a) => {
                    let iter = a.iterator();
                    Statement::ForLoop {
                        init: iter.0.map(|a| self.lower_stmt(a)),
                        cond: iter.1.map(|a| self.lower_expr(a)),
                        finish_action: iter.2.map(|a| self.lower_expr(a)),

                        body: self.lower_stmt2(a.body()),
                    }
                }
                nodes::Stmt::WhileStmt(a) => Statement::WhileLoop {
                    cond: self.lower_expr2(a.cond()),
                    body: self.lower_stmt2(a.stmt()),
                },
                nodes::Stmt::DoWhileStmt(a) => Statement::DoWhileLoop {
                    cond: self.lower_expr2(a.cond()),
                    body: self.lower_stmt2(a.stmt()),
                },
                nodes::Stmt::ContinueStmt(_) => Statement::Continue {},
                nodes::Stmt::TryStmt(a) => Statement::Try {
                    expr: self.lower_expr2(a.expr()),
                    returns: a
                        .returns()
                        .and_then(|a| a.parameter_list())
                        .map(|a| a.parameters().map(|p| self.lower_parameter(p)).collect()),
                    body: self.lower_stmt2(a.block().map(nodes::Stmt::Block)),
                    catch: a
                        .catch_clauses()
                        .map(|c| {
                            CatchClause::new(
                                self.db,
                                Ident::from_name_ref_opt(self.db, c.name_ref()),
                                c.parameter_list().map(|a| {
                                    a.parameters().map(|p| self.lower_parameter(p)).collect()
                                }),
                                self.lower_stmt2(c.block().map(nodes::Stmt::Block)),
                            )
                        })
                        .collect(),
                },
                nodes::Stmt::ReturnStmt(a) => {
                    Statement::Return { expr: a.expr().map(|a| self.lower_expr(a)) }
                }
                nodes::Stmt::EmitStmt(a) => Statement::Emit {
                    event: self.lower_expr2(a.expr()),
                    args: a
                        .call_argument_list()
                        .map(|a| self.lower_call_argument_list(a))
                        .unwrap_or_default(),
                },
                nodes::Stmt::RevertStmt(a) => Statement::Revert {
                    event: self.lower_expr2(a.expr()),
                    args: a
                        .call_argument_list()
                        .map(|a| self.lower_call_argument_list(a))
                        .unwrap_or_default(),
                },
                nodes::Stmt::AssemblyStmt(_) => Statement::Assembly {},
                nodes::Stmt::BreakStmt(_) => Statement::Break {},
            },
            Some(AstPtr::new(&expr)),
        );

        self.save_stmt(expr, res);
        res
    }
}
