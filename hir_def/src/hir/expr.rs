use crate::hir::ident::Ident;
use crate::hir::literal::Literal;
use crate::hir::source_unit::Item;
use crate::hir::type_name::ElementaryTypeRef;
use crate::items::HirPrint;
use crate::{impl_major_item, lazy_field, FileAstPtr, FileExt};
use base_db::BaseDb;
use rowan::ast::AstPtr;
use salsa::Database;
use std::fmt::Write;
use syntax::ast::nodes;

use super::call_options::CallOption;
use super::op::{BinaryOp, PostfixOp, PrefixOp};
use super::type_name::walk_type_ref;
use super::{TypeRefId, TypeRefKind};

#[salsa::tracked(debug)]
#[derive(PartialOrd, Ord)]
pub struct ExprId<'db> {
    #[returns(ref)]
    #[id]
    pub kind: Expr<'db>,

    #[tracked]
    pub node: Option<FileAstPtr<nodes::Expr>>,
}

impl HirPrint for ExprId<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        self.kind(db).write(db, w, ident)
    }
}

fn missing<'db>(db: &'db dyn Database) -> ExprId<'db> {
    ExprId::new(db, Expr::Missing, None)
}

impl<'db> ExprId<'db> {
    pub fn missing(db: &'db dyn Database) -> Self {
        missing(db)
    }

    pub fn walk(self, db: &'db dyn Database, expr_fn: &mut impl FnMut(Self)) {
        expr_fn(self);
        match self.kind(db) {
            Expr::Index { target, index } => {
                target.walk(db, expr_fn);
                index.walk(db, expr_fn);
            }
            Expr::Slice { base, start, end } => {
                base.walk(db, expr_fn);
                if let Some(start) = start {
                    start.walk(db, expr_fn);
                }
                if let Some(end) = end {
                    end.walk(db, expr_fn);
                }
            }
            Expr::MemberAccess { owner: e, .. } => {
                e.walk(db, expr_fn);
            }
            Expr::CallOptions { base, options } => {
                base.walk(db, expr_fn);
                for o in options {
                    o.val.walk(db, expr_fn);
                }
            }
            Expr::Call { callee, args } => {
                callee.walk(db, expr_fn);
                for o in args {
                    o.1.walk(db, expr_fn);
                }
            }
            Expr::PrefixOp { expr, .. } => {
                expr.walk(db, expr_fn);
            }
            Expr::PostfixOp { expr, .. } => {
                expr.walk(db, expr_fn);
            }
            Expr::BinaryOp { lhs, rhs, .. } => {
                lhs.walk(db, expr_fn);
                rhs.walk(db, expr_fn);
            }
            Expr::Ternary { cond, rhs, lhs } => {
                cond.walk(db, expr_fn);
                lhs.walk(db, expr_fn);
                rhs.walk(db, expr_fn);
            }
            Expr::Tuple { content } => {
                for e in content {
                    e.walk(db, expr_fn);
                }
            }
            Expr::Array { content } => {
                for e in content {
                    e.walk(db, expr_fn);
                }
            }
            Expr::Ident { .. } => {}
            Expr::Literal { .. } => {}
            Expr::ElementaryTypeName { .. } => {}
            Expr::New { ty } => match ty.kind(db) {
                TypeRefKind::Elementary(_) => {}
                TypeRefKind::Function { .. } => {}
                TypeRefKind::Mapping { .. } => {}
                TypeRefKind::Path(_) => {}
                TypeRefKind::Array { ty, len } => {
                    walk_type_ref(ty, db, expr_fn);
                    if let Some(t) = len {
                        t.walk(db, expr_fn);
                    }
                }
                TypeRefKind::Error => {}
            },
            Expr::Type { ty } => walk_type_ref(*ty, db, expr_fn),
            Expr::Missing => {}
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, salsa::Update)]
pub enum Expr<'db> {
    Index { target: ExprId<'db>, index: ExprId<'db> },
    Slice { base: ExprId<'db>, start: Option<ExprId<'db>>, end: Option<ExprId<'db>> },
    MemberAccess { owner: ExprId<'db>, member_name: Ident<'db> },
    CallOptions { base: ExprId<'db>, options: Vec<CallOption<'db>> },
    Call { callee: ExprId<'db>, args: Vec<(Option<Ident<'db>>, ExprId<'db>)> },
    PrefixOp { expr: ExprId<'db>, op: PrefixOp },
    PostfixOp { expr: ExprId<'db>, op: PostfixOp },
    BinaryOp { lhs: ExprId<'db>, op: Option<BinaryOp>, rhs: ExprId<'db> },
    Ternary { cond: ExprId<'db>, lhs: ExprId<'db>, rhs: ExprId<'db> },
    Tuple { content: Vec<ExprId<'db>> },
    Array { content: Vec<ExprId<'db>> },
    Ident { name_ref: Ident<'db> },
    Literal { data: Literal },
    ElementaryTypeName { data: ElementaryTypeRef },
    New { ty: TypeRefId<'db> },
    Type { ty: TypeRefId<'db> },
    Missing,
}

impl HirPrint for Expr<'_> {
    fn write<T: Write>(&self, db: &dyn Database, w: &mut T, ident: usize) -> std::fmt::Result {
        match self {
            Expr::Index { target, index } => {
                target.write(db, w, ident)?;
                w.write_str("[")?;
                index.write(db, w, ident)?;
                w.write_str("]")?;
            }
            Expr::Slice { base, start, end } => {
                base.write(db, w, ident)?;
                w.write_str("[")?;
                if let Some(start) = start {
                    start.write(db, w, ident)?;
                }
                w.write_str(":")?;
                if let Some(end) = end {
                    end.write(db, w, ident)?;
                }
                w.write_str("]")?;
            }
            Expr::MemberAccess { owner, member_name } => {
                owner.write(db, w, ident)?;
                w.write_str(".")?;
                member_name.write(db, w, ident)?;
            }
            Expr::CallOptions { base, options } => {
                base.write(db, w, ident)?;
                w.write_str("{")?;
                for (i, o) in options.iter().enumerate() {
                    if i > 0 {
                        w.write_str(", ")?
                    }
                    o.name.write(db, w, ident)?;
                    w.write_str(": ")?;
                    o.val.write(db, w, ident)?;
                }
            }
            Expr::Call { callee, args } => {
                callee.write(db, w, ident)?;
                w.write_str("(")?;
                if matches!(args.first(), Some((Some(_), _))) {
                    w.write_str("{")?;
                }
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        w.write_str(",")?;
                    }
                    if let Some(name) = a.0 {
                        name.write(db, w, ident)?;
                        w.write_str(": ")?;
                    }
                    a.1.write(db, w, ident)?;
                }
                if matches!(args.first(), Some((Some(_), _))) {
                    w.write_str("}")?;
                }
                w.write_str(")")?;
            }
            Expr::PrefixOp { expr, op } => {
                write!(w, "{op}")?;
                expr.write(db, w, ident)?;
            }
            Expr::PostfixOp { expr, op } => {
                expr.write(db, w, ident)?;
                write!(w, "{op}")?;
            }
            Expr::BinaryOp { lhs, op, rhs } => {
                lhs.write(db, w, ident)?;
                if let Some(op) = op {
                    write!(w, "{op}")?;
                }
                rhs.write(db, w, ident)?;
            }
            Expr::Ternary { cond, lhs, rhs } => {
                cond.write(db, w, ident)?;
                w.write_str(" ? ")?;
                lhs.write(db, w, ident)?;
                w.write_str(" : ")?;
                rhs.write(db, w, ident)?;
            }
            Expr::Tuple { content } => {
                w.write_str("(")?;
                for (i, a) in content.iter().enumerate() {
                    if i > 0 {
                        w.write_str(",")?;
                    }
                    a.write(db, w, ident)?;
                }
                w.write_str(")")?;
            }
            Expr::Array { content } => {
                w.write_str("]")?;
                for (i, a) in content.iter().enumerate() {
                    if i > 0 {
                        w.write_str(",")?;
                    }
                    a.write(db, w, ident)?;
                }
                w.write_str("[")?;
            }
            Expr::Ident { name_ref } => {
                name_ref.write(db, w, ident)?;
            }
            Expr::Literal { data } => {
                write!(w, "{data}")?;
            }
            Expr::ElementaryTypeName { data } => {
                data.write(db, w, ident)?;
            }
            Expr::New { ty } => {
                w.write_str("new ")?;
                ty.write(db, w, ident)?;
            }
            Expr::Missing => {
                w.write_str("<missing>")?;
            }
            Expr::Type { ty } => {
                w.write_str("type(")?;
                ty.write(db, w, ident);
                w.write_str(")")?;
            }
        }
        Ok(())
    }
}
