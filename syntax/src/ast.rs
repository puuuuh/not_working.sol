use crate::{
    SolidityLang, SyntaxElementChildren, SyntaxKind, SyntaxNode, SyntaxNodeChildren, SyntaxToken,
};
use rowan::ast::AstNode;
use std::marker::PhantomData;

mod ext;
mod generated;

pub use generated::*;

/// Like `AstNode`, but wraps tokens rather than interior nodes.
pub trait AstToken {
    fn can_cast(token: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstTokenChildren {
    inner: SyntaxElementChildren,
    kind: SyntaxKind,
}

impl crate::ast::AstTokenChildren {
    fn new(parent: &SyntaxNode, kind: SyntaxKind) -> Self {
        crate::ast::AstTokenChildren { inner: parent.children_with_tokens(), kind }
    }
}

impl Iterator for AstTokenChildren {
    type Item = SyntaxToken;
    fn next(&mut self) -> Option<SyntaxToken> {
        self.inner.find_map(|t| {
            let t = t.into_token()?;
            if t.kind() == self.kind {
                Some(t)
            } else {
                None
            }
        })
    }
}

/// An iterator over `SyntaxNode` children of a particular AST type.
#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildren { inner: parent.children(), ph: PhantomData }
    }
}

impl<N: AstNode<Language = SolidityLang>> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> {
        self.inner.find_map(N::cast)
    }
}

mod support {
    use super::{AstChildren, AstNode, AstTokenChildren, SyntaxKind, SyntaxNode, SyntaxToken};
    use crate::SolidityLang;

    pub(super) fn child<N: AstNode<Language = SolidityLang>>(parent: &SyntaxNode) -> Option<N> {
        parent.children().find_map(N::cast)
    }

    pub(super) fn children<N: AstNode<Language = SolidityLang>>(
        parent: &SyntaxNode,
    ) -> AstChildren<N> {
        AstChildren::new(parent)
    }

    pub(super) fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        parent.children_with_tokens().filter_map(|it| it.into_token()).find(|it| it.kind() == kind)
    }

    pub(super) fn tokens(parent: &SyntaxNode, kind: SyntaxKind) -> AstTokenChildren {
        AstTokenChildren::new(parent, kind)
    }
}
