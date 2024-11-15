use crate::parser::lexer::{is_user_definable_op, USER_DEFINABLE_OP};
use crate::parser::Parser;
use crate::{eat, peek, peek_recover};
use syntax::SyntaxKind::*;

impl Parser<'_> {
    /// https://docs.soliditylang.org/en/latest/grammar.html#a4.SolidityParser.usingDirective
    pub(crate) fn using(&mut self) {
        assert_eq!(self.current(), USING_KW);
        self.builder.start_node(USING.into());
        self.bump(USING_KW);
        'parse: {
            peek!([IDENT, L_CURLY], 'parse, self);

            self.using_item();

            eat!([FOR_KW], 'parse, self);

            self.using_target();

            self.eat(GLOBAL_KW);

            eat!([SEMICOLON], 'parse, self);
        }

        self.builder.finish_node();
    }

    fn using_target(&mut self) {
        self.builder.start_node(USING_TARGET.into());
        {
            if !self.eat(STAR) {
                self.type_name();
            }
        }
        self.builder.finish_node();
    }

    fn using_item(&mut self) {
        'parse: {
            match peek!([IDENT, L_CURLY], 'parse, self) {
                IDENT => self.identifier_path(),
                L_CURLY => self.using_aliases(),
                _ => unreachable!(),
            }
        }
    }

    // { path as * }
    fn using_aliases(&mut self) {
        assert!(self.at(L_CURLY));
        self.builder.start_node(USING_ALIASES.into());
        self.bump(L_CURLY);

        'parse: loop {
            peek!([IDENT], 'parse, self);
            self.using_alias();
            let token = peek!([COMMA, R_CURLY], 'parse, self);
            self.bump(token);

            if let R_CURLY = token {
                break 'parse;
            }
        }
        self.builder.finish_node();
    }

    fn using_alias(&mut self) {
        self.builder.start_node(USING_ALIAS.into());
        'parse: {
            self.identifier_path();
            if self.eat(AS_KW) {
                let Some(op) = self.at_oneof(USER_DEFINABLE_OP) else {
                    self.err_unexpected_token(USER_DEFINABLE_OP);
                    return;
                };
                self.builder.start_node(USER_DEFINEABLE_OPERATOR.into());
                self.bump(op);
                self.builder.finish_node();
            }
        }
        self.builder.finish_node();
    }
}
