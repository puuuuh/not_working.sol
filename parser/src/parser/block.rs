use crate::parser::Parser;
use crate::{eat, peek, peek_recover};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    /// https://docs.soliditylang.org/en/latest/grammar.html#a4.SolidityParser.block
    pub(crate) fn block(&mut self) {
        assert!(self.at(L_CURLY));
        self.builder.start_node(BLOCK.into());
        'parse: {
            self.bump(L_CURLY);
            loop {
                if self.at(UNCHECKED_KW) {
                    self.block_unchecked();
                } else if !self.at(R_CURLY) && !self.at(EOF) {
                    self.stmt();
                } else {
                    break;
                }
            }
            eat!(R_CURLY, 'parse, self);
        }

        self.builder.finish_node();
    }

    pub(crate) fn block_unchecked(&mut self) {
        assert!(self.at(UNCHECKED_KW));
        self.builder.start_node(UNCHECKED_BLOCK.into());
        self.bump(UNCHECKED_KW);

        'parse: {
            peek!(L_CURLY, 'parse, self);
            self.block();
        }

        self.builder.finish_node();
    }
}
