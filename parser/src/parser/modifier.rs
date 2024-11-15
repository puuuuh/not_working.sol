use crate::parser::Parser;
use crate::{eat, peek};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn modifier_invocation(&mut self) {
        assert_eq!(self.current(), IDENT);
        self.builder.start_node(MODIFIER_INVOCATION.into());
        'parse: {
            peek!(IDENT, 'parse, self);
            self.identifier_path();
            if self.eat(L_PAREN) {
                self.call_arguments();
                eat!(R_PAREN, 'parse, self);
            }
        }
        self.builder.finish_node();
    }

    pub(crate) fn modifier_definition(&mut self) {
        assert_eq!(self.current(), MODIFIER_KW);
        self.builder.start_node(MODIFIER_DEFINITION.into());
        'parse: {
            eat!(MODIFIER_KW, 'parse, self);
            peek!(IDENT, 'parse, self);
            self.name();
            if self.eat(L_PAREN) {
                if !self.at(R_PAREN) {
                    self.parameter_list();
                }
                eat!(R_PAREN, 'parse, self);
            }
            loop {
                match self.current() {
                    OVERRIDE_KW => {
                        self.builder.start_node(MODIFIER_ATTRIBUTE.into());
                        self.override_specifier();
                        self.builder.finish_node();
                    }
                    VIRTUAL_KW => {
                        self.builder.start_node(MODIFIER_ATTRIBUTE.into());
                        self.bump(VIRTUAL_KW);
                        self.builder.finish_node();
                    }
                    _ => {
                        break;
                    }
                }
            }
            if self.at(L_CURLY) {
                self.block();
            } else {
                eat!(SEMICOLON, 'parse, self);
            }
        }
        self.builder.finish_node();
    }
}
