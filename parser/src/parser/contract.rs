use crate::parser::Parser;
use crate::{eat, peek};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn contract(&mut self) {
        self.builder.start_node(CONTRACT.into());
        'parse: {
            self.eat(ABSTRACT_KW);
            eat!([CONTRACT_KW, LIBRARY_KW, INTERFACE], 'parse, self);
            peek!(IDENT, 'parse, self);
            self.name();

            if self.at(IS_KW) {
                self.inheritance();
            }

            self.contract_body();
        }
        self.builder.finish_node();
    }

    pub(crate) fn contract_body(&mut self) {
        'parse: {
            eat!(L_CURLY, 'parse, self);
            loop {
                if self.at(R_CURLY) {
                    break;
                }
                if self
                    .at_oneof(&[
                        CONSTRUCTOR_KW,
                        EVENT_KW,
                        ERROR_KW,
                        USING_KW,
                        FUNCTION_KW,
                        MODIFIER_KW,
                        FALLBACK_KW,
                        RECEIVE_KW,
                        STRUCT_KW,
                        ENUM_KW,
                        TYPE_KW,
                        IDENT,
                        MAPPING_KW,
                        ELEMENTARY_TYPE,
                    ])
                    .is_some()
                {
                    self.contract_body_item();
                } else {
                    break;
                }
            }
            eat!(R_CURLY, 'parse, self);
        }
    }

    pub(crate) fn contract_body_item(&mut self) {
        'parse: {
            match peek!([CONSTRUCTOR_KW, EVENT_KW, ERROR_KW, USING_KW, FUNCTION_KW, MODIFIER_KW, FALLBACK_KW, RECEIVE_KW, STRUCT_KW, ENUM_KW, TYPE_KW, IDENT, MAPPING_KW, ELEMENTARY_TYPE], 'parse, self)
            {
                CONSTRUCTOR_KW => {
                    self.constructor();
                }
                FUNCTION_KW => {
                    if self.nth(1) == L_CURLY {
                        self.state_variable_declaration()
                    } else {
                        self.function();
                    }
                }
                MODIFIER_KW => {
                    self.modifier_definition();
                }
                FALLBACK_KW => {
                    self.fallback_function();
                }
                RECEIVE_KW => {
                    self.receive_function();
                }
                STRUCT_KW => {
                    self.struct_definition();
                }
                ENUM_KW => {
                    self.enum_definition();
                }
                EVENT_KW => {
                    self.event();
                }
                TYPE_KW => {
                    self.user_defined_value_type();
                }
                ERROR_KW => {
                    self.error();
                }
                USING_KW => {
                    self.using();
                }
                _ => {
                    self.state_variable_declaration();
                }
            }
        }
    }

    fn state_variable_declaration(&mut self) {
        self.builder.start_node(STATE_VARIABLE_DECLARATION.into());
        'parse: {
            self.type_name();
            loop {
                match peek!([PUBLIC_KW, INTERNAL_KW, PRIVATE_KW, CONSTANT_KW, OVERRIDE_KW, IMMUTABLE_KW, IDENT], 'parse, self)
                {
                    IDENT => {
                        self.name();
                        break;
                    }
                    OVERRIDE_KW => {
                        self.builder.start_node(STATE_VARIABLE_ATTRIBUTE.into());
                        self.override_specifier();
                        self.builder.finish_node();
                    }
                    modifier => {
                        self.builder.start_node(STATE_VARIABLE_ATTRIBUTE.into());
                        self.bump(modifier);
                        self.builder.finish_node();
                    }
                }
            }
            if self.eat(EQ) {
                self.expr();
            }
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn constructor(&mut self) {
        assert!(self.at(CONSTRUCTOR_KW));
        self.builder.start_node(CONSTRUCTOR_DEFINITION.into());
        'parse: {
            eat!(CONSTRUCTOR_KW, 'parse, self);
            self.function_tail();
        }
        self.builder.finish_node();
    }

    pub(crate) fn inheritance(&mut self) {
        self.builder.start_node(INHERITANCE.into());
        'parse: {
            eat!(IS_KW, 'parse, self);
            loop {
                peek!(IDENT, 'parse, self);
                self.inheritance_specifier();
                if !self.eat(COMMA) {
                    break;
                }
            }
        }
        self.builder.finish_node();
    }

    pub(crate) fn inheritance_specifier(&mut self) {
        self.builder.start_node(INHERITANCE_SPECIFIER.into());
        'parse: {
            self.identifier_path();
            if self.eat(L_PAREN) {
                self.call_arguments();
                eat!(R_PAREN, 'parse, self);
            }
        }
        self.builder.finish_node();
    }
}

#[cfg(test)]
mod tests {
    use rowan::SyntaxNode;
    use syntax::{SolidityLang, SyntaxKind};

    use crate::parser::common::format_node;
    use crate::parser::lexer::Lexer;

    #[test]
    fn test_contract() {
        let tests = &["contract ContractName is SomeShit(a, b, c) {
                constructor() {
                    (uint256 t) = someobj.somefunc();
                    if (a) {}
                }
                fallback() {
                    uint256 test = 123;
                }
            }"];
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
            res.contract();
            res.builder.finish_node();
            let node = SyntaxNode::<SolidityLang>::new_root(res.builder.finish());
            let mut data = String::new();
            format_node(&mut data, 0, &mut node.children_with_tokens());
            println!("{data}");
            dbg!(res.errors);
        }
    }
}
