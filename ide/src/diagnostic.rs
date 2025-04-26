use hir_def::SyntaxError;

pub enum Diagnostic {
    Syntax(SyntaxError)
}