use crate::parser::Parser;
use crate::{eat, peek};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn library(&mut self) {
        self.builder.start_node(CONTRACT.into());
        'parse: {
            eat!(LIBRARY_KW, 'parse, self);
            peek!(IDENT, 'parse, self);
            self.name();

            self.contract_body();
        }
        self.builder.finish_node();
    }
}
