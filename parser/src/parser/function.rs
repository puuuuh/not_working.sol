use crate::eat;
use crate::peek;

use super::Parser;
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn function(&mut self) {
        assert!(self.at(FUNCTION_KW));
        self.builder.start_node(NAMED_FUNCTION_DEFINITION.into());
        'parse: {
            eat!(FUNCTION_KW, 'parse, self);
            if !self.at(L_PAREN) {
                // solidity 4.0 fallback function syntax
                match peek!([FALLBACK_KW, RECEIVE_KW, IDENT], 'parse, self) {
                    IDENT => {
                        self.name();
                    }
                    t => {
                        self.bump(t);
                    }
                }
            }
            self.function_tail();
        }
        self.builder.finish_node();
    }

    pub(crate) fn fallback_function(&mut self) {
        assert_eq!(self.current(), FALLBACK_KW);

        self.builder.start_node(FALLBACK_FUNCTION_DEFINITION.into());
        'parse: {
            eat!(FALLBACK_KW, 'parse, self);
            self.function_tail();
        }
        self.builder.finish_node();
    }

    pub(crate) fn receive_function(&mut self) {
        assert_eq!(self.current(), RECEIVE_KW);

        self.builder.start_node(RECEIVE_FUNCTION_DEFINITION.into());
        'parse: {
            eat!(RECEIVE_KW, 'parse, self);
            // TODO: use custom parser func for receive function
            self.function_tail();
        }
        self.builder.finish_node();
    }

    pub(crate) fn function_tail(&mut self) {
        'parse: {
            eat!(L_PAREN, 'parse, self);
            if !self.at(R_PAREN) {
                self.parameter_list();
            }
            eat!(R_PAREN, 'parse, self);

            loop {
                let Some(t) = self.at_oneof(&[
                    OVERRIDE_KW,
                    INTERNAL_KW,
                    EXTERNAL_KW,
                    PRIVATE_KW,
                    PUBLIC_KW,
                    PURE_KW,
                    VIEW_KW,
                    CONSTANT_KW,
                    PAYABLE_KW,
                    VIRTUAL_KW,
                    IDENT,
                ]) else {
                    break;
                };
                self.builder.start_node(FUNCTION_ATTRIBUTE.into());
                match t {
                    OVERRIDE_KW => {
                        self.override_specifier();
                    }
                    INTERNAL_KW | EXTERNAL_KW | PRIVATE_KW | PUBLIC_KW => {
                        self.bump(t);
                    }
                    PURE_KW | VIEW_KW | CONSTANT_KW | PAYABLE_KW => self.bump(t),
                    VIRTUAL_KW => {
                        self.bump(t);
                    }
                    IDENT => {
                        self.modifier_invocation();
                    }
                    _ => {
                        unreachable!()
                    }
                }
                self.builder.finish_node();
            }

            if self.at(RETURNS_KW) {
                self.returns();
            }

            match peek!([SEMICOLON, L_CURLY], 'parse, self) {
                SEMICOLON => {
                    self.bump(SEMICOLON);
                }
                _ => {
                    self.block();
                }
            }
        }
    }

    pub(crate) fn override_specifier(&mut self) {
        assert!(self.at(OVERRIDE_KW));
        self.builder.start_node(OVERRIDE_SPECIFIER.into());
        'parse: {
            eat!(OVERRIDE_KW, 'parse, self);
            if self.eat(L_PAREN) {
                if !self.at(R_PAREN) {
                    peek!(IDENT, 'parse, self);
                    self.identifier_path();
                    while self.eat(COMMA) {
                        peek!(IDENT, 'parse, self);
                        self.identifier_path();
                    }
                }
                eat!(R_PAREN, 'parse, self);
            }
        }
        self.builder.finish_node();
    }
}
