use crate::parser::Parser;
use crate::{eat, peek, peek_recover};
use syntax::SyntaxKind;
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn is_elementary_type(text: &str) -> bool {
        if matches!(text, "bool" | "string" | "bytes" | "int" | "uint" | "fixed" | "ufixed") {
            return true;
        }

        if let Some(n) = text.strip_prefix("bytes") {
            if let Ok(n) = n.parse::<u8>() {
                return (1..=32).contains(&n);
            }
        } else {
            let text = text.strip_prefix('u').unwrap_or(text);
            if let Some(n) = text.strip_prefix("int") {
                if let Ok(n) = n.parse::<u16>() {
                    return n % 8 == 0 && (8..=256).contains(&n);
                }
            }
            if let Some(n) = text.strip_prefix("fixed") {
                if let Some((m, n)) = n.split_once('x') {
                    if let Ok((m, n)) =
                        m.parse::<u16>().and_then(|m| n.parse::<u8>().map(|n| (m, n)))
                    {
                        return m % 8 == 0 && (8..=256).contains(&m) && (0..=80).contains(&n);
                    }
                }
            }
        }

        false
    }

    pub(crate) fn user_defined_value_type(&mut self) {
        assert!(self.at(TYPE_KW));
        self.builder.start_node(USER_DEFINED_VALUE_TYPE_DEFINITION.into());
        'parse: {
            eat!(TYPE_KW, 'parse, self);
            peek!(IDENT, 'parse, self);
            self.name();
            eat!(IS_KW, 'parse, self);
            self.type_name();
            eat!(SEMICOLON, 'parse, self);
        }
        self.builder.finish_node();
    }

    // TODO: FIX!!!
    pub(crate) fn type_name(&mut self) {
        let start = self.builder.checkpoint();
        let mut ok = false;
        'parse: {
            match peek!([ELEMENTARY_TYPE, ADDRESS_KW, FUNCTION_KW, MAPPING_KW, IDENT], 'parse, self)
            {
                ELEMENTARY_TYPE | ADDRESS_KW => {
                    self.elementary_type_name();
                }
                FUNCTION_KW => self.function_type_name(),
                MAPPING_KW => self.mapping_type(),
                IDENT => self.ident_path_type(),
                _ => {
                    unreachable!()
                }
            }
            ok = true;
        }
        if !ok {
            return;
        }

        let mut active_node = false;
        'parse: while self.at(L_BRACK) {
            self.builder.start_node_at(start, ARRAY_TYPE.into());
            active_node = true;
            self.bump(L_BRACK);
            if !self.at(R_BRACK) {
                self.expr();
            }
            eat!(R_BRACK, 'parse, self);
            self.builder.finish_node();
            active_node = false;
        }
        if active_node {
            self.builder.finish_node();
        }
    }

    fn ident_path_type(&mut self) {
        assert!(self.at(IDENT));
        self.builder.start_node(IDENT_PATH_TYPE.into());
        'parse: loop {
            peek!(IDENT, 'parse, self);
            self.name_ref();
            if !self.at(DOT) {
                break 'parse;
            }
            self.bump(DOT)
        }

        self.builder.finish_node();
    }

    pub fn elementary_type_name(&mut self) {
        self.builder.start_node(ELEMENTARY_TYPE.into());
        'parse: {
            match peek!([IDENT, ADDRESS_KW], 'parse, self) {
                ADDRESS_KW => {
                    self.bump(ELEMENTARY_TYPE_IDENT);
                    self.eat(PAYABLE_KW);
                }
                _ => {
                    self.bump(ELEMENTARY_TYPE_IDENT);
                }
            }
        }
        self.builder.finish_node();
    }

    fn function_type_name(&mut self) {
        assert_eq!(self.current(), FUNCTION_KW);
        self.builder.start_node(FUNCTION_TYPE.into());
        'parse: {
            eat!(FUNCTION_KW, 'parse, self);
            eat!(L_PAREN, 'parse, self);
            if !self.at(R_PAREN) {
                self.parameter_list();
            }
            eat!(R_PAREN, 'parse, self);

            while let Some(t) = self.at_oneof(&[
                PUBLIC_KW,
                PRIVATE_KW,
                INTERNAL_KW,
                EXTERNAL_KW,
                PURE_KW,
                VIEW_KW,
                PAYABLE_KW,
            ]) {
                self.bump(t);
            }
            if self.at(RETURNS_KW) {
                self.returns();
            }
        }
        self.builder.finish_node();
    }

    pub(crate) fn returns(&mut self) {
        assert_eq!(self.current(), RETURNS_KW);
        self.builder.start_node(RETURNS.into());
        'returns: {
            eat!(RETURNS_KW, 'returns, self);
            eat!(L_PAREN, 'returns, self);
            if !self.at(R_PAREN) {
                self.parameter_list();
            }
            eat!(R_PAREN, 'returns, self);
        }
        self.builder.finish_node();
    }

    /// Parameter list without parens
    pub(crate) fn parameter_list(&mut self) {
        self.builder.start_node(PARAMETER_LIST.into());
        loop {
            self.builder.start_node(VARIABLE_DECLARATION.into());
            self.type_name();
            if let Some(loc) = self.at_oneof(&[MEMORY_KW, CALLDATA_KW, STORAGE_KW]) {
                self.builder.start_node(DATA_LOCATION.into());
                self.bump(loc);
                self.builder.finish_node()
            }
            if self.at(IDENT) {
                self.name();
            }
            self.builder.finish_node();
            if !self.eat(COMMA) {
                break;
            }
        }
        self.builder.finish_node()
    }

    fn mapping_type(&mut self) {
        assert!(self.at(MAPPING_KW));
        self.builder.start_node(MAPPING_TYPE.into());
        'parse: {
            eat!(MAPPING_KW, 'parse, self);
            eat!(L_PAREN, 'parse, self);
            self.mapping_key_type();
            eat!(FAT_ARROW, 'parse, self);
            self.mapping_key_value();
            eat!(R_PAREN, 'parse, self);
        }
        self.builder.finish_node();
    }

    fn mapping_key_value(&mut self) {
        self.builder.start_node(MAPPING_VALUE.into());
        'parse: {
            self.type_name();
            if self.at(IDENT) {
                self.name()
            }
        }
        self.builder.finish_node();
    }

    fn mapping_key_type(&mut self) {
        self.builder.start_node(MAPPING_KEY.into());
        'parse: {
            self.type_name();
            if self.at(IDENT) {
                self.name()
            }
        }
        self.builder.finish_node();
    }
}

#[cfg(test)]
mod tests {
    use rowan::{SyntaxElementChildren, SyntaxNode};

    use crate::parser::lexer::Lexer;
    use syntax::SolidityLang;
    use syntax::SyntaxKind;

    fn format_node(
        buf: &mut String,
        src: &str,
        l: usize,
        list: &mut SyntaxElementChildren<SolidityLang>,
    ) {
        let prefix = "    ".repeat(l);
        for child in list {
            let s: usize = child.text_range().start().into();
            let e: usize = child.text_range().end().into();
            buf.push_str(&format!(
                "{prefix}{:?}@{:?} {data}\n",
                child.kind(),
                child.text_range(),
                data = &src[s..e]
            ));
            if let Some(n) = child.as_node() {
                let mut nodes = n.children_with_tokens();
                format_node(buf, src, l + 1, &mut nodes);
            }
        }
    }

    #[test]
    fn test_basic() {
        let tests = &["(, feedValue, , timestamp, )"];
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
            format_node(&mut data, text, 0, &mut node.children_with_tokens());
            println!("{data}");
            dbg!(res.errors);
        }
    }
}
