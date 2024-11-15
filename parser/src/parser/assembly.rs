use crate::parser::Parser;
use crate::{eat, peek};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    // Just skip assembly block
    pub(crate) fn assembly_stmt(&mut self) {
        assert!(self.at(ASSEMBLY_KW));
        self.builder.start_node(YUL_ASSEMBLY.into());
        'parse: {
            eat!(ASSEMBLY_KW, 'parse, self);
            _ = self.eat(STRING);
            if self.eat(L_PAREN) {
                loop {
                    eat!(STRING, 'parse, self);
                    if !self.at(COMMA) {
                        break;
                    }
                }
                eat!(R_PAREN, 'parse, self);
            }
            eat!(L_CURLY, 'parse, self);
            let mut ctr = 0;
            loop {
                match self.current() {
                    EOF => {
                        break;
                    }
                    L_CURLY => {
                        ctr += 1;
                        self.bump(L_CURLY);
                    }
                    R_CURLY => {
                        if ctr > 0 {
                            ctr -= 1;
                            self.bump(R_CURLY);
                        } else {
                            break;
                        }
                    }
                    token => {
                        self.bump(token);
                    }
                }
            }
            eat!(R_CURLY, 'parse, self);
        }

        self.builder.finish_node();
    }
}
