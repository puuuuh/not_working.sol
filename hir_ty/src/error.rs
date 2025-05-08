use base_db::File;
use hir_def::{Expr, FileAstPtr};
use syntax::TextRange;

#[salsa::accumulator]
#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct TypeResolutionError {
    pub file: File,
    pub text: String,
    pub range: TextRange,
}
