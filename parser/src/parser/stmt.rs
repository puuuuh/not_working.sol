use crate::eat;
use crate::peek;

use super::Parser;
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn stmt(&mut self) {
        match self.current() {
            L_CURLY => self.block(),
            IDENT if self.at(UNCHECKED_KW) => self.block_unchecked(),
            IDENT if self.at(REVERT_KW) => self.revert_stmt(),
            IF_KW => self.if_stmt(),
            FOR_KW => self.for_stmt(),
            WHILE_KW => self.while_stmt(),
            DO_KW => self.do_stmt(),
            CONTINUE_KW => self.continue_stmt(),
            BREAK_KW => self.break_stmt(),
            TRY_KW => self.try_stmt(),
            RETURN_KW => self.return_stmt(),
            EMIT_KW => self.emit_stmt(),
            ASSEMBLY_KW => self.assembly_stmt(),
            _ => self.vardecl_or_expr_stmt(),
        }
    }

    pub(crate) fn vardecl_or_expr_stmt(&mut self) {
        let mut lookahead = self.tail();
        if lookahead.at(L_PAREN) {
            lookahead.variable_declaration_tuple();
        } else {
            lookahead.variable_declaration();
        }

        if lookahead.errors.is_empty() {
            self.variable_declaration_stmt();
        } else {
            self.expression_stmt();
        }
    }

    fn expression_stmt(&mut self) {
        self.builder.start_node(EXPR_STMT.into());
        'parse: {
            self.expr();
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    pub(crate) fn variable_declaration_stmt(&mut self) {
        self.builder.start_node(VARIABLE_DECLARATION_STMT.into());
        'parse: {
            let init_required = if self.at(L_PAREN) {
                self.variable_declaration_tuple();
                true
            } else {
                self.variable_declaration();
                false
            };
            if init_required || self.at(EQ) {
                eat!(EQ, 'parse, self);
                self.expr();
            }
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn variable_declaration(&mut self) {
        self.builder.start_node(PARAMETER.into());
        'parse: {
            self.type_name();
            match peek!([CALLDATA_KW, MEMORY_KW, STORAGE_KW, CONSTANT_KW, IDENT], 'parse, self) {
                t @ (MEMORY_KW | CALLDATA_KW | STORAGE_KW) => {
                    self.builder.start_node(DATA_LOCATION.into());
                    self.bump(t);
                    self.builder.finish_node()
                }
                t @ CONSTANT_KW => self.bump(t),
                IDENT => {}
                _ => unreachable!(),
            };
            peek!(IDENT, 'parse, self);
            self.name();
        }
        self.builder.finish_node();
    }

    pub(crate) fn variable_declaration_tuple(&mut self) {
        self.builder.start_node(VARIABLE_TUPLE_DECLARATION.into());
        'parse: {
            eat!(L_PAREN, 'parse, self);
            if self.eat(R_PAREN) {
                break 'parse;
            }

            while self.variable_tuple_element() {
                if self.eat(R_PAREN) {
                    break 'parse;
                }
            }
            eat!(R_PAREN, 'parse, self);
        }
        self.builder.finish_node();
    }

    pub(crate) fn variable_tuple_element(&mut self) -> bool {
        self.builder.start_node(VARIABLE_TUPLE_ELEMENT.into());
        let has_next = 'parse: {
            if self.eat(COMMA) {
                break 'parse true;
            }
            self.variable_declaration();
            if !self.eat(COMMA) {
                break 'parse false;
            }
            true
        };
        self.builder.finish_node();
        has_next
    }
    fn if_stmt(&mut self) {
        assert_eq!(self.current(), IF_KW);
        self.builder.start_node(IF_STMT.into());
        'parse: {
            eat!(IF_KW, 'parse, self);
            eat!(L_PAREN, 'parse, self);
            self.expr();
            eat!(R_PAREN, 'parse, self);
            self.stmt();
            if self.eat(ELSE_KW) {
                self.stmt()
            }
        }
        self.builder.finish_node();
    }

    fn for_stmt(&mut self) {
        assert_eq!(self.current(), FOR_KW);
        self.builder.start_node(FOR_STMT.into());
        'parse: {
            eat!(FOR_KW, 'parse, self);
            eat!(L_PAREN, 'parse, self);
            if !self.eat(SEMICOLON) {
                self.vardecl_or_expr_stmt()
            }
            if !self.eat(SEMICOLON) {
                self.expression_stmt()
            }
            if !self.at(R_PAREN) {
                self.expr();
            }
            eat!(R_PAREN, 'parse, self);

            self.stmt();
        }
        self.builder.finish_node();
    }

    fn while_stmt(&mut self) {
        assert_eq!(self.current(), WHILE_KW);
        self.builder.start_node(WHILE_STMT.into());
        'parse: {
            eat!(WHILE_KW, 'parse, self);
            eat!(L_PAREN, 'parse, self);
            self.expr();
            eat!(R_PAREN, 'parse, self);
            self.stmt();
        }
        self.builder.finish_node();
    }

    fn do_stmt(&mut self) {
        assert_eq!(self.current(), DO_KW);
        self.builder.start_node(DO_WHILE_STMT.into());
        'parse: {
            eat!(DO_KW, 'parse, self);
            self.stmt();
            eat!(WHILE_KW, 'parse, self);
            eat!(L_PAREN, 'parse, self);
            self.expr();
            eat!(R_PAREN, 'parse, self);
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn continue_stmt(&mut self) {
        assert_eq!(self.current(), CONTINUE_KW);
        self.builder.start_node(CONTINUE_STMT.into());
        'parse: {
            eat!(CONTINUE_KW, 'parse, self);
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn break_stmt(&mut self) {
        assert_eq!(self.current(), BREAK_KW);
        self.builder.start_node(BREAK_STMT.into());
        'parse: {
            eat!(BREAK_KW, 'parse, self);
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn return_stmt(&mut self) {
        assert_eq!(self.current(), RETURN_KW);
        self.builder.start_node(RETURN_STMT.into());
        'parse: {
            eat!(RETURN_KW, 'parse, self);
            if !self.at(SEMICOLON) {
                self.expr();
            }
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn emit_stmt(&mut self) {
        assert_eq!(self.current(), EMIT_KW);
        self.builder.start_node(EMIT_STMT.into());
        'parse: {
            eat!(EMIT_KW, 'parse, self);
            self.expr();
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn revert_stmt(&mut self) {
        assert!(self.at(REVERT_KW));
        self.builder.start_node(REVERT_STMT.into());
        'parse: {
            eat!(REVERT_KW, 'parse, self);
            self.expr();
            peek!([L_PAREN, SEMICOLON], 'parse, self);
            if self.eat(L_PAREN) {
                self.call_arguments();
                eat!(R_PAREN, 'parse, self);
            }
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn catch_clause(&mut self) {
        assert_eq!(self.current(), CATCH_KW);
        self.builder.start_node(CATCH_CLAUSE.into());
        'parse: {
            eat!(CATCH_KW, 'parse, self);
            peek!([IDENT, L_PAREN, L_CURLY], 'parse, self);
            self.eat(IDENT);
            match peek!([L_PAREN, L_CURLY], 'parse, self) {
                IDENT => self.bump(IDENT),
                L_PAREN => {
                    eat!(L_PAREN, 'parse, self);
                    self.parameter_list();
                    eat!(R_PAREN, 'parse, self);
                }
                L_CURLY => {}
                _ => unreachable!(),
            }
            self.block();
        }
        self.builder.finish_node();
    }

    fn try_stmt(&mut self) {
        assert_eq!(self.current(), TRY_KW);
        self.builder.start_node(TRY_STMT.into());
        'parse: {
            eat!(TRY_KW, 'parse, self);
            self.expr();
            peek!([RETURNS_KW, L_CURLY], 'parse, self);
            match self.current() {
                RETURNS_KW => {
                    self.returns();
                }
                L_CURLY => {}
                _ => unreachable!(),
            }
            peek!(L_CURLY, 'parse, self);
            self.block();
            while self.at(CATCH_KW) {
                self.catch_clause();
            }
        }
        self.builder.finish_node();
    }
}
