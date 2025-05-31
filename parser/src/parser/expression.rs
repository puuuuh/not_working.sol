use super::Parser;
use crate::{eat, peek};
use rowan::TextRange;
use syntax::syntax_error::SyntaxError;
use syntax::SyntaxKind;
use syntax::SyntaxKind::*;

fn infix_bp(t: SyntaxKind) -> Option<(u8, u8)> {
    Some(match t {
        DOT => (30, 32),
        STARSTAR => (26, 27),
        STAR | SLASH | PERCENT => (24, 25),
        PLUS | MINUS => (22, 23),
        RSHIFT | LSHIFT => (20, 21),
        AMP => (18, 19),
        CARET => (16, 17),
        PIPE => (14, 15),
        GEQ | GT | LEQ | LT => (12, 13),
        NEQ | EQ2 => (10, 11),
        AMPAMP => (8, 9),
        PIPEPIPE => (6, 7),

        EQ | PIPEEQ | CARETEQ | AMPEQ | LSHIFTEQ | RSHIFTEQ | PLUSEQ | MINUSEQ | STAREQ
        | SLASHEQ | PERCENTEQ => (2, 3),

        _ => return None,
    })
}

fn prefix_bp(t: SyntaxKind) -> Option<u8> {
    Some(match t {
        NEW_KW => 30,

        PLUSPLUS | MINUSMINUS | MINUS | DELETE_KW | BANG | TILDE => 28,

        _ => return None,
    })
}

fn postfix_bp(t: SyntaxKind) -> Option<u8> {
    Some(match t {
        PLUSPLUS | MINUSMINUS | L_BRACK | L_PAREN | L_CURLY => 30,
        QUESTION => 4, // Not supported right now

        _ => return None,
    })
}

impl<'a> Parser<'a> {
    pub(crate) fn expr(&mut self) -> bool {
        self.expr_bp(0)
    }
    pub(crate) fn expr_bp(&mut self, min_bp: u8) -> bool {
        let current = self.pos;
        self.try_expr_bp(min_bp);
        if current == self.pos {
            self.builder.start_node(ERROR.into());
            let start = self.current_offset();
            self.bump_any();
            let end = self.current_offset();
            self.builder.finish_node();
            self.errors.push(SyntaxError::new(
                format!(
                    "Expected expression, found {}",
                    self.current_full().map(|(_, a)| a).unwrap_or_default()
                ),
                TextRange::new(start.try_into().unwrap(), end.try_into().unwrap()),
            ));
        }

        current != self.pos
    }

    pub(crate) fn try_expr_bp(&mut self, min_bp: u8) -> bool {
        let start = self.builder.checkpoint();
        match self.current() {
            IDENT if self.at(ELEMENTARY_TYPE_IDENT) => {
                self.builder.start_node_at(start, ELEMENTARY_TYPE.into());
                'parse: {
                    if let Some((_, "address")) = self.current_full() {
                        self.bump(ELEMENTARY_TYPE_IDENT);
                        self.eat(PAYABLE_KW);
                    } else {
                        self.bump(ELEMENTARY_TYPE_IDENT);
                    }
                }
            }
            IDENT => {
                self.builder.start_node_at(start, IDENT_EXPR.into());
                self.name_ref();
            }
            STRING | HEX_STRING | DECIMAL_NUMBER | HEX_NUMBER | TRUE_KW | FALSE_KW => {
                self.builder.start_node_at(start, LITERAL_EXPR.into());
                self.literal();
            }
            L_BRACK => {
                self.builder.start_node_at(start, INLINE_ARRAY_EXPR.into());
                self.bump_any();
                loop {
                    if self.at(R_BRACK) {
                        break;
                    }
                    if !self.try_expr_bp(0) {
                        break;
                    }
                    if !self.eat(COMMA) {
                        break;
                    }
                }
                'parse: {
                    eat!(R_BRACK, 'parse, self);
                }
            }
            L_PAREN => {
                self.builder.start_node_at(start, TUPLE_EXPR.into());
                self.bump_any();
                while self.eat(COMMA) {}
                loop {
                    while self.eat(COMMA) {}
                    if self.at(R_PAREN) {
                        break;
                    }
                    if !self.try_expr_bp(0) {
                        break;
                    }
                    if !self.eat(COMMA) {
                        break;
                    }
                }
                'parse: {
                    eat!(R_PAREN, 'parse, self);
                }
            }
            NEW_KW => {
                self.builder.start_node_at(start, NEW_EXPR.into());
                self.bump(NEW_KW);
                self.type_name();
            }
            TYPE_KW => {
                self.builder.start_node_at(start, TYPE_EXPR.into());
                self.bump(TYPE_KW);

                'parse: {
                    eat!(L_PAREN, 'parse, self);
                    self.type_name();
                    eat!(R_PAREN, 'parse, self);
                }
            }
            op => {
                if let Some(r) = prefix_bp(op) {
                    self.builder.start_node_at(start, PREFIX_EXPR.into());
                    self.bump_any();
                    self.expr_bp(r);
                } else {
                    return false; // Non-expr token
                }
            }
        }
        self.builder.finish_node();

        loop {
            let op = self.current();
            if let Some(l) = postfix_bp(op) {
                if l < min_bp {
                    return true;
                }
                match op {
                    QUESTION => {
                        self.builder.start_node_at(start, TERNARY_EXPR.into());
                        'parse: {
                            eat!(QUESTION, 'parse, self);
                            self.expr();
                            eat!(COLON, 'parse, self);
                            self.expr();
                        }
                        self.builder.finish_node();
                    }
                    L_BRACK => {
                        self.builder.start_node_at(start, INDEX_EXPR.into());
                        'parse: {
                            eat!(L_BRACK, 'parse, self);
                            self.try_expr_bp(0);
                            if self.eat(COLON) {
                                self.try_expr_bp(0);
                            }
                            eat!(R_BRACK, 'parse, self);
                        }
                        self.builder.finish_node();
                    }
                    L_CURLY => {
                        {
                            let mut lookahead = self.tail();
                            // https://github.com/ethereum/solidity/blob/5fe31737472cc593cc9957d24a964ebf5491cbeb/libsolidity/parsing/Parser.cpp#L2228
                            lookahead.bump_any();
                            if !lookahead.at(IDENT) {
                                break;
                            }
                            lookahead.bump_any();
                            if !lookahead.at(COLON) {
                                break;
                            }
                        }
                        self.builder.start_node_at(start, CALL_OPTIONS_EXPR.into());
                        self.named_call_arguments();
                        self.builder.finish_node();
                    }
                    L_PAREN => {
                        self.builder.start_node_at(start, CALL_EXPR.into());
                        'parse: {
                            eat!(L_PAREN, 'parse, self);
                            self.call_arguments();
                            eat!(R_PAREN, 'parse, self);
                        }
                        self.builder.finish_node();
                    }
                    _ => {
                        self.builder.start_node_at(start, POSTFIX_EXPR.into());
                        self.bump_any();
                        self.builder.finish_node();
                    }
                }
                continue;
            }
            if let Some((l, r)) = infix_bp(op) {
                if l < min_bp {
                    return true;
                }

                match op {
                    DOT => {
                        self.builder.start_node_at(start, MEMBER_ACCESS_EXPR.into());
                        self.bump(DOT);
                        'parse: {
                            peek!([IDENT], 'parse, self);
                            self.name_ref();
                        }
                        self.builder.finish_node();
                    }
                    any => {
                        self.builder.start_node_at(start, INFIX_EXPR.into());
                        self.bump(any);
                        self.try_expr_bp(r);
                        self.builder.finish_node();
                    }
                }

                continue;
            }
            break;
        }

        true
    }

    pub(crate) fn call_arguments(&mut self) {
        if self.at(L_CURLY) {
            self.named_call_arguments();
        } else if !self.at(R_PAREN) {
            self.unnamed_call_arguments();
        }
    }

    fn unnamed_call_arguments(&mut self) {
        self.builder.start_node(CALL_ARGUMENTS.into());
        'parse: {
            loop {
                if !self.try_expr_bp(0) {
                    break;
                }
                if !self.eat(COMMA) {
                    break;
                }
            }
        }
        self.builder.finish_node();
    }

    fn named_call_argument(&mut self) {
        assert!(self.at(IDENT));
        self.builder.start_node(NAMED_CALL_ARGUMENT.into());
        'parse: {
            eat!(IDENT, 'parse, self);
            eat!(COLON, 'parse, self);
            self.expr();
        }
        self.builder.finish_node();
    }

    fn named_call_arguments(&mut self) {
        assert_eq!(self.current(), L_CURLY);
        self.builder.start_node(NAMED_CALL_ARGUMENTS.into());
        'parse: {
            self.bump(L_CURLY);
            while let IDENT = peek!([R_CURLY, IDENT], 'parse, self) {
                self.named_call_argument();

                if !self.eat(COMMA) {
                    break;
                }
            }
            eat!(R_CURLY, 'parse, self);
        }
        self.builder.finish_node();
    }
}

/*
impl<'a> Parser<'a> {
    fn expr_bp(&mut self, min_bp: u8) -> S {
        let mut lhs = match self.current() {
            Token::Atom(it) => S::Atom(it),
            t => panic!("bad token: {:?}", t),
        };

        loop {
            let op = match lexer.peek() {
                Token::Eof => break,
                Token::Op(op) => op,
                t => panic!("bad token: {:?}", t),
            };

            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }

            lexer.next();
            let rhs = expr_bp(lexer, r_bp);

            lhs = S::Cons(op, vec![lhs, rhs]);
        }

        lhs
    }
}

pub fn infix_binding_power(op: TokenType) -> (u8, u8) {
    match op {
        _ => todo!(),
    }
}
*/

#[cfg(test)]
mod tests {
    use rowan::SyntaxNode;
    use syntax::{SolidityLang, SyntaxKind};

    use crate::parser::common::format_node;
    use crate::parser::lexer::Lexer;

    #[test]
    fn test_expressions() {
        let tests = &["contr.method{gas: 1+2, unknownopt: (a,b,c)}  (arg, arg2)"];
        let mut pos = 0;
        for text in tests {
            let tokens = Lexer::new(text)
                .map(move |t| {
                    let r = (t.kind, &text[pos..pos + t.len]);
                    pos += t.len;
                    r
                })
                .collect::<Vec<_>>();

            let mut res = super::Parser::new(&tokens);
            res.builder.start_node(SyntaxKind::UNIT_SOURCE.into());
            res.expr();
            res.builder.finish_node();
            let node = SyntaxNode::<SolidityLang>::new_root(res.builder.finish());
            let mut data = String::new();
            format_node(&mut data, 0, &mut node.children_with_tokens());
            println!("{data}");
            dbg!(res.errors);
        }
    }
}
