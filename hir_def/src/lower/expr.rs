use crate::hir;
use crate::hir::Ident;
use crate::hir::Literal;
use crate::hir::{Expr, ExprId};
use crate::lower::LowerCtx;
use rowan::ast::{AstNode, AstPtr};
use syntax::ast::nodes;

impl<'db> LowerCtx<'db> {
    pub fn lower_expr2(&mut self, expr: Option<nodes::Expr>) -> ExprId<'db> {
        expr.map(|a| self.lower_expr(a)).unwrap_or(ExprId::new(self.db, Expr::Missing, None))
    }

    pub fn lower_expr(&mut self, expr: nodes::Expr) -> ExprId<'db> {
        let res = ExprId::new(
            self.db,
            match &expr {
                nodes::Expr::IndexExpr(i) => Expr::Index {
                    target: self.lower_expr2(i.base()),
                    index: self.lower_expr2(i.index()),
                },
                nodes::Expr::SliceExpr(i) => Expr::Slice {
                    base: self.lower_expr2(i.expr()),
                    start: i.from().map(|e| self.lower_expr(e)),
                    end: i.to().map(|e| self.lower_expr(e)),
                },
                nodes::Expr::MemberAccessExpr(i) => Expr::MemberAccess {
                    owner: self.lower_expr2(i.expr()),
                    member_name: Ident::from_name_ref(self.db, i.field()),
                },
                nodes::Expr::InfixExpr(i) => Expr::BinaryOp {
                    lhs: self.lower_expr2(i.lhs()),
                    op: i.op().map(hir::BinaryOp::from),
                    rhs: self.lower_expr2(i.rhs()),
                },
                nodes::Expr::PostfixExpr(i) => i
                    .op()
                    .zip(i.expr())
                    .map(|(op, expr)| Expr::PostfixOp {
                        expr: self.lower_expr(expr),
                        op: hir::PostfixOp::from(op),
                    })
                    .unwrap_or(Expr::Missing),
                nodes::Expr::PrefixExpr(i) => i
                    .op()
                    .zip(i.expr())
                    .map(|(op, expr)| Expr::PrefixOp {
                        expr: self.lower_expr(expr),
                        op: hir::PrefixOp::from(op),
                    })
                    .unwrap_or(Expr::Missing),
                nodes::Expr::CallOptionsExpr(i) => Expr::CallOptions {
                    base: self.lower_expr2(i.expr()),
                    options: i.options().map(|o| self.collect_call_options(o)).unwrap_or_default(),
                },
                nodes::Expr::CallExpr(i) => Expr::Call {
                    callee: self.lower_expr2(i.expr()),
                    args: i
                        .call_argument_list()
                        .map(|l| self.collect_argument_list(l))
                        .unwrap_or_default(),
                },
                nodes::Expr::TernaryExpr(i) => Expr::Ternary {
                    cond: self.lower_expr2(i.expr()),
                    lhs: self.lower_expr2(i.if_true()),
                    rhs: self.lower_expr2(i.if_false()),
                },
                nodes::Expr::TupleExpr(i) => {
                    let content = i.exprs().map(|e| self.lower_expr(e)).collect();
                    Expr::Tuple { content }
                }
                nodes::Expr::IdentExpr(i) => {
                    Expr::Ident { name_ref: Ident::from_name_ref(self.db, i.name_ref()) }
                }
                nodes::Expr::LiteralExpr(lit) => Expr::Literal {
                    data: lit.literal().map(|l| self.lower_literal(l)).unwrap_or(Literal::Error),
                },
                nodes::Expr::ElementaryType(ty) => Expr::ElementaryTypeName {
                    data: self.lower_elementary_name(ty.clone()).unwrap(),
                },
                nodes::Expr::NewExpr(new) => Expr::New {
                    ty: self.lower_type_ref2(new.ty()),
                },
            },
            Some(crate::FileAstPtr::new(self.file, &expr)),
        );

        self.save_expr(expr, res);
        res
    }
}
