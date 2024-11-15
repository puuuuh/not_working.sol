use crate::syntax_kind::SyntaxKind;

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

/// Second, implementing the `Language` trait teaches rowan to convert between
/// these two SyntaxKind types, allowing for a nicer SyntaxNode API where
/// "kinds" are values from our `enum SyntaxKind`, instead of plain u16 values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SolidityLang {}
impl rowan::Language for SolidityLang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= SyntaxKind::__LAST as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}
pub type SyntaxNode = rowan::SyntaxNode<SolidityLang>;
pub type SyntaxToken = rowan::SyntaxToken<SolidityLang>;
pub type SyntaxElement = rowan::SyntaxElement<SolidityLang>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<SolidityLang>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<SolidityLang>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<SolidityLang>;
