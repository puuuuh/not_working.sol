use hir_def::SyntaxError;
use hir_ty::error::TypeCheckError;
use rowan::TextRange;

#[derive(Debug, Clone, Copy)]
pub enum Diagnostic<'db> {
    Syntax(&'db SyntaxError),
    TypeCheck(&'db TypeCheckError)
}

impl <'db> Diagnostic<'db> {
    pub fn range(self) -> TextRange {
        match self {
            Diagnostic::Syntax(syntax_error) => syntax_error.range,
            Diagnostic::TypeCheck(type_check_error) => type_check_error.range,
        }
    }

    pub fn text(self) -> String {
        match self {
            Diagnostic::Syntax(syntax_error) => syntax_error.text.clone(),
            Diagnostic::TypeCheck(type_check_error) => type_check_error.text.clone(),
        }
    }
}