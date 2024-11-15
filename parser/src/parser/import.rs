use crate::parser::Parser;
use crate::{eat, peek, peek_recover};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn import(&mut self) {
        assert_eq!(self.current(), IMPORT_KW);
        self.builder.start_node(IMPORT.into());
        self.bump(IMPORT_KW);

        'parse: {
            match peek_recover!([STRING, L_CURLY, STAR], 'parse, self) {
                STRING => {
                    self.import_path();
                }
                L_CURLY | STAR => {
                    self.import_symbols();
                }
                _ => {
                    unreachable!()
                }
            }

            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn import_path(&mut self) {
        assert!(self.at(STRING));
        self.builder.start_node(IMPORT_PATH.into());
        'parse: {
            self.path();
            if self.at(AS_KW) {
                self.bump(AS_KW);
                peek!(IDENT, 'parse, self);
                self.name();
            }
        }
        self.builder.finish_node();
    }

    fn import_symbols(&mut self) {
        self.builder.start_node(IMPORT_SYMBOLS.into());
        'parse: {
            match peek!([L_CURLY, STAR], 'parse, self) {
                L_CURLY => self.symbol_aliases(),
                STAR => self.symbol_alias(),
                _ => unreachable!(),
            }
            eat!(FROM_KW, 'parse, self);
            self.path();
        }
        self.builder.finish_node();
    }

    fn path(&mut self) {
        self.builder.start_node(PATH.into());
        'parse: {
            eat!(STRING, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn symbol_alias(&mut self) {
        self.builder.start_node(SYMBOL_ALIAS.into());
        'parse: {
            peek!([IDENT, STAR], 'parse, self);
            self.symbol();
            if self.at(AS_KW) {
                eat!(AS_KW, 'parse, self);
                peek!(IDENT, 'parse, self);
                self.name();
            }
        }
        self.builder.finish_node();
    }

    fn symbol_aliases(&mut self) {
        assert_eq!(self.current(), L_CURLY);
        self.builder.start_node(SYMBOL_ALIASES.into());
        'parse: {
            eat!(L_CURLY, 'parse, self);
            loop {
                self.symbol_alias();
                if !self.eat(COMMA) {
                    break;
                }
            }
            eat!(R_CURLY, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn symbol(&mut self) {
        self.builder.start_node(SYMBOL.into());
        'parse: {
            eat!([IDENT, STAR], 'parse, self);
        }
        self.builder.finish_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::common::format_node;
    use crate::parser::lexer::Lexer;
    use rowan::{SyntaxElementChildren, SyntaxNode};
    use syntax::{SolidityLang, SyntaxKind};

    #[test]
    fn import_tests() {
        let tests = &["import {ident, ident as ident2, global } from \"path\";"];
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
            res.import();
            res.builder.finish_node();
            let node = SyntaxNode::<SolidityLang>::new_root(res.builder.finish());
            let mut data = String::new();
            format_node(&mut data, 0, &mut node.children_with_tokens());
            println!("{data}");
            dbg!(res.errors);
        }
    }
}
