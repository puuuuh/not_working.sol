use rowan::TextRange;

pub enum CompletionKind {
    Item,
    Variable,
    Text,
    Method,
    Function,
    Constructor,
    Field,
    Class,
    Interface,
    Module,
    Property,
    Unit,
    Value,
    Enum,
    Keyword,
    Snippet,
    Color,
    File,
    Reference,
    Folder,
    EnumMember,
    Constant,
    Struct,
    Event,
    Operator,
    TypeParameter,
}

pub struct Completion {
    pub label: String,

    pub src_range: TextRange,
    pub text: String,
    pub kind: CompletionKind,
}
