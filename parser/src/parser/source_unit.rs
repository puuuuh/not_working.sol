use crate::parser::{Parse, Parser};
use crate::peek_recover;
use syntax::ast::nodes::UnitSource;
use syntax::syntax_error::SyntaxError;
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn parse_data(mut self) -> (Parse<UnitSource>, Vec<SyntaxError>) {
        self.builder.start_node(UNIT_SOURCE.into());
        self.skip_ws();
        while self.pos != self.tokens.len() {
            self.toplevel_item();
        }
        self.builder.finish_node();
        let node = self.builder.finish();
        (Parse::new(node), core::mem::take(&mut self.errors))
    }

    fn toplevel_item(&mut self) {
        'parse: {
            let token = peek_recover!([PRAGMA_KW, IMPORT_KW, USING_KW, ABSTRACT_KW, CONTRACT_KW, LIBRARY_KW, INTERFACE_KW,
                FUNCTION_KW, STRUCT_KW, ENUM_KW, TYPE_KW, ERROR_KW, EVENT_KW, EOF, IDENT, MAPPING_KW, ELEMENTARY_TYPE_IDENT ], 'parse, self);
            match token {
                PRAGMA_KW => {
                    self.pragma();
                }
                IMPORT_KW => {
                    self.import();
                }
                USING_KW => {
                    self.using();
                }
                ABSTRACT_KW | CONTRACT_KW => {
                    self.contract();
                }
                LIBRARY_KW => {
                    self.library();
                }
                INTERFACE_KW => {
                    self.interface();
                }
                FUNCTION_KW => {
                    self.function();
                }
                STRUCT_KW => {
                    self.struct_definition();
                }
                ENUM_KW => {
                    self.enum_definition();
                }
                TYPE_KW => {
                    self.user_defined_value_type();
                }
                ERROR_KW => {
                    self.error();
                }
                EVENT_KW => {
                    self.event();
                }
                IDENT | MAPPING_KW | ELEMENTARY_TYPE_IDENT => {
                    self.variable_declaration_stmt();
                }
                EOF => {}
                _ => unreachable!(),
            }
        }
    }
}

/*
Completed:
 * Pragma
 * Using
 * Import
 */
