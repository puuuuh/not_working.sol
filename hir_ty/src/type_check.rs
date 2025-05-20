use base_db::{BaseDb, Project};
use hir_def::{
    walk::{walk_expr, walk_stmt, Visitor}, ElementaryTypeRef, Expr, ExprId, FileAstPtr, Item, StatementId, TypeRefId
};
use hir_nameres::scope::Scope;
use salsa::{Accumulator, Backtrace};

use crate::{
    error::TypeCheckError,
    resolver::{resolve_item, TypeResolution},
    tys::{common_types, unknown, ElementaryTypes, Ty, TyKind},
};

pub struct TypeCheckWalker<'db> {
    project: Project,
    type_resolution: &'db TypeResolution<'db>,
    common_types: ElementaryTypes<'db>,
}

impl<'db> Visitor<'db> for &mut TypeCheckWalker<'db> {
    type Ctx = ();
    type ExprCtx = ();

    fn stmt_end(&mut self, db: &'db dyn BaseDb, _: (), stmt: StatementId<'db>) {
        match stmt.kind(db) {
            hir_def::Statement::Missing => {}
            hir_def::Statement::VarDecl { items, init_expr } => {
                let lhs_ty = items
                    .iter()
                    .copied()
                    .map(|vardecl| {
                        let t = (vardecl)?.ty(db);
                        let expected = self.type_resolution.type_ref(db, t);
                        Some((t, Ty::new_in(expected, vardecl?.location(db).into())))
                    })
                    .collect::<Vec<_>>();

                if let Some(init) = init_expr {
                    let init_ty = self.type_resolution.expr(db, *init);
                    if init_ty.component_count(db) != items.len() as _ {
                        emit_expr_error(db, *init, "different component count".to_string());
                        return;
                    }

                    let init_kind = init_ty.kind(db);
                    let tys = match &init_kind {
                        TyKind::Tuple(tys) => tys.as_slice(),
                        _ => &[init_ty][..],
                    };
                    for (lhs, rhs) in lhs_ty.into_iter().zip(tys.iter()) {
                        if let Some((ty_ref, ty)) = lhs {
                            if !rhs.can_coerce(db, self.project, ty) {
                                emit_typeref_type_mismatch(db, self.project, ty_ref, ty, *rhs);
                            }
                        }
                    }
                }
            }
            hir_def::Statement::Expr { expr } => {}
            hir_def::Statement::Block { stmts, is_unchecked } => {}
            hir_def::Statement::If { cond, body, else_body } => {
                self.check_expr_type(db, self.project, *cond, self.common_types.bool);
            }
            hir_def::Statement::ForLoop { init, cond, finish_action, body } => {
                if let Some(cond) = cond {
                    self.check_expr_type(db, self.project, *cond, self.common_types.bool);
                }
            }
            hir_def::Statement::WhileLoop { cond, body } => {
                self.check_expr_type(db, self.project, *cond, self.common_types.bool);
            }
            hir_def::Statement::DoWhileLoop { cond, body } => {
                self.check_expr_type(db, self.project, *cond, self.common_types.bool);
            }
            hir_def::Statement::Try { expr, returns, body, catch } => {}
            hir_def::Statement::Return { expr } => {}
            hir_def::Statement::Emit { event, args } => {}
            hir_def::Statement::Revert { event, args } => {}
            hir_def::Statement::Assembly {} => {}
            hir_def::Statement::Continue {} => {}
            hir_def::Statement::Break {} => {}
        }
    }

    fn expr_end(&mut self, db: &'db dyn BaseDb, ctx: (), expr: ExprId<'db>) {
        match expr.kind(db) {
            Expr::Index { target, index } => {
                let target_ty = self.type_resolution.expr(db, *target);
                let expected = match target_ty.kind(db) {
                    TyKind::Array(_, _)
                    | TyKind::Elementary(ElementaryTypeRef::FixedBytes { .. } | ElementaryTypeRef::Bytes) => self.common_types.uint256,

                    TyKind::Mapping(ty, ty1) => Ty::new(ty),
                    _ => {
                        emit_expr_error(
                            db,
                            *target,
                            format!(
                                "can't index into {}",
                                target_ty.human_readable(db, self.project)
                            ),
                        );
                        return;
                    }
                };
                self.check_expr_type(db, self.project, *index, expected);
            }
            Expr::Slice { base, start, end } => {}
            Expr::MemberAccess { owner, member_name } => {}
            Expr::CallOptions { base, options } => {}
            Expr::Call { callee, args } => {}
            Expr::PrefixOp { expr, op } => {}
            Expr::PostfixOp { expr, op } => {}
            Expr::BinaryOp { lhs, op, rhs } => {
                let Some(op) = op else {
                    return;
                };
                match op {
                    hir_def::BinaryOp::Assignment { op: None } => {
                        self.check_expr_type(
                            db,
                            self.project,
                            *rhs,
                            self.type_resolution.expr(db, *lhs),
                        );
                    }
                    _ => {}
                }
            }
            Expr::Ternary { cond, lhs, rhs } => {
                self.check_expr_type(db, self.project, *cond, self.common_types.bool);
                self.check_expr_type(db, self.project, *rhs, self.type_resolution.expr(db, *lhs));
            }
            Expr::Tuple { content } => {}
            Expr::Array { content } => {}
            Expr::Ident { name_ref } => {}
            Expr::Literal { data } => {}
            Expr::ElementaryTypeName { data } => {}
            Expr::New { ty } => {}
            Expr::Type { ty } => {}
            Expr::Missing => {}
        }
    }

    fn stmt_start(&mut self, db: &'db dyn BaseDb, stmt: StatementId<'db>) -> (bool, Self::Ctx) {
        (true, ())
    }

    fn expr_start(&mut self, db: &'db dyn BaseDb, expr: ExprId<'db>) -> (bool, Self::ExprCtx) {
        (true, ())
    }
}

impl<'db> TypeCheckWalker<'db> {
    fn check_expr_type(
        &self,
        db: &'db dyn BaseDb,
        project: Project,
        expr: ExprId<'db>,
        expected: Ty<'db>,
    ) {
        let ty = self.type_resolution.expr(db, expr);
        if !ty.can_coerce(db, project, expected) {
            emit_expr_type_mismatch(db, project, expr, expected, ty);
        }
    }
}

pub fn check_item<'db>(db: &'db dyn BaseDb, project: Project, item: Item<'db>) {
    let types = resolve_item(db, project, item);
    let mut ctx = TypeCheckWalker {
        type_resolution: types,
        common_types: common_types(db),
        project,
    };

    if let Some((body, _)) = item.body(db) {
        walk_stmt(db, body, &mut ctx);
    }
}

#[salsa::tracked]
fn emit_typeref_error<'db>(db: &'db dyn BaseDb, expr: TypeRefId<'db>, msg: String) {
    if let Some(node) = expr.node(db) {
        TypeCheckError {
            file: node.file,
            text: msg,
            range: node.ptr.syntax_node_ptr().text_range(),
        }
        .accumulate(db);
    }
}

#[salsa::tracked]
fn emit_expr_error<'db>(db: &'db dyn BaseDb, expr: ExprId<'db>, msg: String) {
    if let Some(node) = expr.node(db) {
        TypeCheckError {
            file: node.file,
            text: msg,
            range: node.ptr.syntax_node_ptr().text_range(),
        }
        .accumulate(db);
    }
}

fn emit_expr_type_mismatch<'db>(
    db: &'db dyn BaseDb,
    project: Project,
    node: ExprId<'db>,
    expected: Ty<'db>,
    found: Ty<'db>,
) {
    emit_expr_error(
        db,
        node,
        format!(
            "can't implicitly cast {} to {}",
            found.human_readable(db, project),
            expected.human_readable(db, project)
        ),
    );
}

fn emit_typeref_type_mismatch<'db>(
    db: &'db dyn BaseDb,
    project: Project,
    node: TypeRefId<'db>,
    expected: Ty<'db>,
    found: Ty<'db>,
) {
    emit_typeref_error(
        db,
        node,
        format!(
            "can't implicitly cast {} to {}",
            found.human_readable(db, project),
            expected.human_readable(db, project)
        ),
    );
}
