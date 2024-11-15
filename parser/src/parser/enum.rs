use crate::parser::Parser;
use crate::{eat, peek};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn enum_definition(&mut self) {
        assert!(self.at(ENUM_KW));
        self.builder.start_node(ENUM_DEFINITION.into());
        'parse: {
            self.bump(ENUM_KW);

            peek!(IDENT, 'parse, self);
            self.name();

            eat!(L_CURLY, 'parse, self);
            if !self.at(R_CURLY) {
                loop {
                    peek!(IDENT, 'parse, self);
                    self.builder.start_node(ENUM_MEMBER.into());
                    self.name();
                    self.builder.finish_node();

                    if !self.eat(COMMA) {
                        break;
                    }
                }
            }
            eat!(R_CURLY, 'parse, self);
        }
        self.builder.finish_node();
    }
}
