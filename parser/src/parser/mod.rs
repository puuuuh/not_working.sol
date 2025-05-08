mod assembly;
mod block;
pub mod common;
mod contract;
mod r#enum;
pub mod error;
pub mod event;
pub mod expression;
pub mod function;
mod import;
mod interface;
pub mod lexer;
mod library;
mod literal;
mod modifier;
mod source_unit;
mod stmt;
mod r#struct;
mod type_name;
mod using;

use lexer::Lexer;
use rowan::{GreenNode, GreenNodeBuilder, TextRange};
use syntax::SyntaxKind;
use syntax::SyntaxKind::*;

pub struct Parser<'a> {
    pos: usize,
    start: usize,
    tokens: Vec<(SyntaxKind, &'a str)>,

    /// the in-progress tree.
    builder: GreenNodeBuilder<'static>,
    /// the list of syntax errors we've accumulated
    /// so far.
    errors: Vec<SyntaxError>,
}

#[macro_export]
macro_rules! peek_recover {
    ([$($t:ident),*], $l:lifetime, $self:ident, $recover_token:ident) => {
        if let Some(t) = ($self.at_oneof(&[$($t),*])) {
            t
        } else {
            $self.recover(&[$($t),*], $recover_token);
            break $l;
        }
    };
    ([$($t:ident),*], $l:lifetime, $self:ident) => {
        peek_recover!([$($t),*], $l, $self, SEMICOLON)
    };

    ($t:ident, $l:lifetime, $self:ident) => {
        peek_recover!([$t], $l, $self, SEMICOLON)
    };
}

#[macro_export]
macro_rules! peek {
    ([$($t:ident),*], $l:lifetime, $self:ident) => {
        if let Some(t) = ($self.at_oneof(&[$($t),*])) {
            t
        } else {
            $self.err_unexpected_token(&[$($t),*]);
            break $l;
        }
    };
    ($t: ident, $l:lifetime, $self:ident) => {
        peek!([$t], $l, $self)
    }
}

#[macro_export]
macro_rules! eat {
    ([$($t:ident),*], $l:lifetime, $self:ident) => {
        if let Some(t) = ($self.at_oneof(&[$($t),*])) {
            $self.bump(t);
            t
        } else {
            $self.err_unexpected_token(&[$($t),*]);
            break $l;
        }
    };

    ($t: ident, $l:lifetime, $self:ident) => {
        eat!([$t], $l, $self)
    }
}

use crate::parser::lexer::{is_trivia, is_user_definable_op, USER_DEFINABLE_OP};
pub use peek;
pub use peek_recover;
use syntax::ast::nodes::UnitSource;
use syntax::parse::Parse;
use syntax::syntax_error::SyntaxError;

impl<'a> Parser<'a> {
    fn new(data: &'a [(SyntaxKind, &'a str)]) -> Self {
        Self {
            pos: 0,
            start: data.get(0).map(|s| s.1.as_ptr() as usize).unwrap_or_default(),
            tokens: data.to_vec(),
            builder: Default::default(),
            errors: Vec::new(),
        }
    }

    fn tail(&self) -> Self {
        Self {
            pos: 0,
            start: self.start,
            tokens: self.tokens.get(self.pos..).unwrap_or_default().to_vec(),
            builder: Default::default(),
            errors: Vec::new(),
        }
    }

    pub fn parse(data: &'a [(SyntaxKind, &'a str)]) -> (Parse<UnitSource>, Vec<SyntaxError>) {
        let res = Self::new(data);

        res.parse_data()
    }

    pub(crate) fn current(&self) -> SyntaxKind {
        self.tokens.get(self.pos).map(|f| f.0).unwrap_or(EOF)
    }

    fn current_full(&self) -> Option<(SyntaxKind, &'a str)> {
        self.tokens.get(self.pos).copied()
    }

    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        if let Some(token) = self.current_full() {
            self.builder.token(kind.into(), token.1);
            self.pos += 1;
            self.skip_ws();
        }
    }

    fn skip_ws(&mut self) {
        while let Some((kind, text)) = self.current_full() {
            if !is_trivia(kind) {
                break;
            }
            self.builder.token(kind.into(), text);
            self.pos += 1;
        }
    }

    pub(crate) fn bump_any(&mut self) {
        if let Some(token) = self.current_full() {
            self.builder.token(token.0.into(), token.1);
            self.pos += 1;
            self.skip_ws();
        }
    }

    fn pragma(&mut self) {
        assert!(self.at(PRAGMA_KW));
        self.builder.start_node(PRAGMA.into());
        self.bump(PRAGMA_KW);

        self.builder.start_node(PRAGMA_DATA.into());
        while self.current() != SEMICOLON && self.current() != EOF {
            self.bump(PRAGMA_TOKEN);
        }
        self.builder.finish_node();

        self.eat(SEMICOLON);
        self.builder.finish_node();
    }

    fn identifier_path(&mut self) {
        assert!(self.at(IDENT));
        self.builder.start_node(IDENT_PATH.into());
        'parse: loop {
            peek!(IDENT, 'parse, self);
            self.name_ref();
            if !self.at(DOT) {
                break 'parse;
            }
            self.bump(DOT)
        }

        self.builder.finish_node();
    }

    pub(crate) fn name(&mut self) {
        assert!(self.at(IDENT));
        self.builder.start_node(NAME.into());
        'parse: {
            eat!(IDENT, 'parse, self);
        }
        self.builder.finish_node();
    }

    pub(crate) fn name_ref(&mut self) {
        assert!(self.at(IDENT));
        self.builder.start_node(NAME_REF.into());
        'parse: {
            eat!(IDENT, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn current_offset(&self) -> usize {
        self.tokens
            .get(self.pos)
            .or(self.tokens.last())
            .map(|s| s.1.as_ptr() as usize)
            .unwrap_or(self.start)
            .saturating_sub(self.start)
    }

    fn err_unexpected_token(&mut self, expected: &[SyntaxKind]) {
        let (_, next_token) = self.current_full().unwrap_or_default();

        let start = self.current_offset();
        if self
            .at_oneof(&[
                SyntaxKind::L_CURLY,
                SyntaxKind::R_CURLY,
                SyntaxKind::L_BRACK,
                SyntaxKind::R_BRACK,
                SyntaxKind::L_PAREN,
                SyntaxKind::R_PAREN,
            ])
            .is_none()
        {
            self.bump_any();
        }
        let end = self.current_offset();

        self.errors.push(SyntaxError::new(
            format!("unexpected {next_token:?}"),
            TextRange::new(start.try_into().unwrap_or_default(), end.try_into().unwrap()),
        ));
        self.builder.start_node(ERROR.into());
        self.builder.finish_node();
    }

    fn recover(&mut self, expected: &[SyntaxKind], until: SyntaxKind) {
        let (_, next_token) = self.current_full().unwrap_or_default();
        let start = self.current_offset();
        if self
            .at_oneof(&[
                SyntaxKind::L_CURLY,
                SyntaxKind::R_CURLY,
                SyntaxKind::L_BRACK,
                SyntaxKind::R_BRACK,
                SyntaxKind::L_PAREN,
                SyntaxKind::R_PAREN,
            ])
            .is_none()
        {
            self.bump_any();
        }
        let end = self.current_offset();

        self.errors.push(SyntaxError::new(
            format!("unexpected {next_token:?}"),
            TextRange::new(start.try_into().unwrap(), end.try_into().unwrap()),
        ));
        self.builder.start_node(ERROR.into());
        loop {
            let current = self.current();
            self.bump(current);
            if current == until || current == EOF {
                break;
            }
        }
        self.builder.finish_node();
    }

    #[must_use]
    fn at_oneof(&mut self, tokens: &[SyntaxKind]) -> Option<SyntaxKind> {
        let (c, text) = self.current_full().unwrap_or((EOF, ""));

        if IDENT == c {
            if let Some(c) = SyntaxKind::from_contextual_keyword(text) {
                if tokens.contains(&c) {
                    return Some(c);
                }
            }
        }

        if !tokens.contains(&c) {
            None
        } else {
            Some(c)
        }
    }

    #[must_use]
    fn at(&mut self, token: SyntaxKind) -> bool {
        self.at_oneof(&[token]).is_some()
    }

    fn eat_oneof(&mut self, token: &[SyntaxKind]) -> Option<SyntaxKind> {
        if let Some(t) = self.at_oneof(token) {
            self.bump(t);
            Some(t)
        } else {
            None
        }
    }

    fn eat(&mut self, token: SyntaxKind) -> bool {
        if self.at(token) {
            self.bump(token);
            true
        } else {
            false
        }
    }

    fn nth(&mut self, mut nth: usize) -> SyntaxKind {
        for i in self.pos..self.tokens.len() {
            if self.tokens.get(i).map(|(k, _)| is_trivia(*k)) == Some(true) {
                continue;
            }
            if nth == 0 {
                return self.tokens.get(i).copied().unwrap_or((EOF, "")).0;
            }
            nth -= 1;
        }
        EOF
    }
}
