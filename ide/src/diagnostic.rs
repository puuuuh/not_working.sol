use hir_def::SyntaxError;
use hir_ty::error::TypeResolutionError;
use rowan::TextRange;

#[derive(Debug, Clone, Copy)]
pub enum Diagnostic<'db> {
    Syntax(&'db SyntaxError),
    TypeResolution(&'db TypeResolutionError)
}

impl <'db> Diagnostic<'db> {
    pub fn range(self) -> TextRange {
        match self {
            Diagnostic::Syntax(syntax_error) => syntax_error.range,
            Diagnostic::TypeResolution(type_resolution_error) => type_resolution_error.range,
        }
    }

    pub fn text(self) -> String {
        match self {
            Diagnostic::Syntax(syntax_error) => syntax_error.text.clone(),
            Diagnostic::TypeResolution(type_resolution_error) => type_resolution_error.text.clone(),
        }
    }
}