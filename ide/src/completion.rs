mod context;
pub mod item;
pub use context::*;
pub use item::*;

use base_db::BaseDb;
use hir_def::{
    lower_file, source_map::item_source_map::ItemSourceMap, Expr, ExprId, FileExt, FilePosition,
    Item, StatementId,
};
use hir_nameres::{
    container::Container,
    scope::{HasScope, Scope},
};
use hir_ty::{resolver::resolve_item, tys::TyKind};
use rowan::{
    ast::{AstNode, AstPtr},
    TextSize,
};
use syntax::{ast::nodes, SyntaxKind, SyntaxToken};

pub fn completion(db: &dyn BaseDb, pos: FilePosition) -> Option<Vec<Completion>> {
    let t = pos.file.node(db);
    let parsed = lower_file(db, pos.file);
    let ctx = context::CompletionCtx::new(db, pos)?;

    ctx.completions()
}
