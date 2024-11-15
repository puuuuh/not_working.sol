use crate::eat;
use crate::peek;

use super::Parser;
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn event(&mut self) {
        assert_eq!(self.current(), EVENT_KW);
        self.builder.start_node(EVENT_DEFINITION.into());
        'parse: {
            self.bump(EVENT_KW);
            peek!(IDENT, 'parse, self);
            self.name();

            self.event_parameters();

            _ = self.eat(ANONYMOUS_KW);

            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn event_parameters(&mut self) {
        'parse: {
            eat!(L_PAREN, 'parse, self);
            if !self.at(R_PAREN) {
                loop {
                    self.event_parameter();
                    if !self.eat(COMMA) {
                        break;
                    }
                }
            }
            eat!(R_PAREN, 'parse, self);
        }
    }

    fn event_parameter(&mut self) {
        self.builder.start_node(EVENT_PARAMETER.into());
        self.type_name();
        self.eat(INDEXED_KW);
        if self.at(IDENT) {
            self.name()
        }
        self.builder.finish_node();
    }
}

#[cfg(test)]
mod tests {
    use rowan::{SyntaxElementChildren, SyntaxNode};

    use crate::parser::common::format_node;
    use crate::parser::lexer::Lexer;
    use syntax::SolidityLang;
    use syntax::SyntaxKind;

    #[test]
    fn test_event() {
        let tests = &["event TestEvent(uint256 indexed tmp, NewStruct help) anonymous;"];
        let mut pos = 0;
        for text in tests {
            dbg!(text);
            let tokens = Lexer::new(text)
                .map(move |t| {
                    let r = (t.kind, &text[pos..pos + t.len]);
                    pos += t.len;
                    r
                })
                .collect::<Vec<_>>();

            let mut res = super::Parser::new(&tokens);
            res.builder.start_node(SyntaxKind::UNIT_SOURCE.into());
            res.event();
            res.builder.finish_node();
            let node = SyntaxNode::<SolidityLang>::new_root(res.builder.finish());
            let mut data = String::new();
            format_node(&mut data, 0, &mut node.children_with_tokens());
            println!("{data}");
            dbg!(&res.errors);
            assert!(res.errors.is_empty());
        }
    }
}
