use crate::parser::Parser;
use crate::{eat, peek, peek_recover};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn struct_definition(&mut self) {
        assert!(self.at(STRUCT_KW));
        self.builder.start_node(STRUCT_DEFINITION.into());
        'parse: {
            eat!(STRUCT_KW, 'parse, self);

            peek!(IDENT, 'parse, self);
            self.name();

            eat!(L_CURLY, 'parse, self);
            let error_count = self.errors.len();
            while !self.at(R_CURLY) {
                self.struct_member();
                if error_count != self.errors.len() {
                    break;
                }
            }
            eat!(R_CURLY, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn struct_member(&mut self) {
        self.builder.start_node(STRUCT_MEMBER.into());
        'parse: {
            self.type_name();
            peek!(IDENT, 'parse, self);
            self.name();
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }
}
