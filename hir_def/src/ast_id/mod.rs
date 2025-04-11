use base_db::{BaseDb, File};
use rowan::ast::AstNode;
use span::AstIdMap;
use std::sync::Arc;

use crate::FileExt;

#[salsa::tracked]
pub fn ast_id_map<'db>(db: &'db dyn BaseDb, file: File) -> Arc<AstIdMap> {
    let f = file.tree(db);
    let span_map = AstIdMap::from_source(f.syntax());
    Arc::new(span_map)
}

/*
#[salsa::tracked]
pub fn span_map(db: &dyn BaseDb, file: File) -> Arc<SpanMap> {
    let mut pairs = vec![];
    let ast_id_map = ast_id_map(db, file);
    let tree = parse(db, file).tree();
    pairs.extend(
        tree.items()
            .map(|item| (item.syntax().text_range().start(), ast_id_map.ast_id(&item).erase())),
    );

    Arc::new(SpanMap::from_file(file, pairs.into_boxed_slice(), tree.syntax().text_range().end()))
}
*/
