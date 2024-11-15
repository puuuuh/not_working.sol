use crate::{eat, peek};

use super::Parser;
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn error(&mut self) {
        assert!(self.at(ERROR_KW));
        self.builder.start_node(ERROR_DEFINITION.into());
        'parse: {
            self.bump(ERROR_KW);
            peek!(IDENT, 'parse, self);
            self.name();
            self.error_parameters();
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn error_parameters(&mut self) {
        'parse: {
            eat!(L_PAREN, 'parse, self);
            if !self.at(R_PAREN) {
                loop {
                    self.error_parameter();
                    if !self.eat(COMMA) {
                        break;
                    }
                }
            }
            eat!(R_PAREN, 'parse, self);
        }
    }

    fn error_parameter(&mut self) {
        self.builder.start_node(ERROR_PARAMETER.into());
        self.type_name();
        if self.at(IDENT) {
            self.name();
        }

        self.builder.finish_node();
    }
}
