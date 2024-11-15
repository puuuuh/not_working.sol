use crate::parser::Parser;
use crate::{eat, peek};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn interface(&mut self) {
        self.builder.start_node(CONTRACT.into());
        'parse: {
            eat!(INTERFACE_KW, 'parse, self);
            peek!(IDENT, 'parse, self);
            self.name();
            if self.at(IS_KW) {
                self.inheritance();
            }
            self.contract_body();
        }
        self.builder.finish_node();
    }
}
