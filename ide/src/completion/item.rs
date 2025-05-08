use rowan::TextRange;

pub enum CompletionKind {
    Item,
    Variable,
}

pub struct Completion {
    pub label: String,

    pub src_range: TextRange,
    pub text: String,
    pub kind: CompletionKind,
}
