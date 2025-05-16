use base_db::{BaseDb, Project};
use hir_def::{lower_file, FileExt, FilePosition};
use hir_ty::resolver::resolve_item;
use rowan::TextRange;
use rowan::ast::{AstNode, AstPtr};
use syntax::ast::nodes;

pub fn hover(db: &dyn BaseDb, project: Project, pos: FilePosition) -> Option<(TextRange, String)> {
    let t = pos.file.node(db);
    let parsed = lower_file(db, pos.file);

    let token = t.syntax().token_at_offset(pos.offset).next()?;

    let e = token.parent_ancestors().find_map(|t| nodes::Expr::cast(t))?;
    let item = parsed.source_map(db).find_pos(pos.offset)?;
    let (stmt, map) = item.body(db)?;

    let expr = map.expr(db, AstPtr::new(&e))?;
    let type_resolution = resolve_item(db, project, item);

    Some((expr.node(db)?.ptr.syntax_node_ptr().text_range(), type_resolution.expr(db, expr).pretty_print(db)))
}