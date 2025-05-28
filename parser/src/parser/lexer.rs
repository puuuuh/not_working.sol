use std::str::Chars;
use syntax::syntax_kind::SyntaxKind;

pub const USER_DEFINABLE_OP: &[SyntaxKind] = &[
    SyntaxKind::AMP,
    SyntaxKind::TILDE,
    SyntaxKind::PIPE,
    SyntaxKind::CARET,
    SyntaxKind::PLUS,
    SyntaxKind::SLASH,
    SyntaxKind::PERCENT,
    SyntaxKind::STAR,
    SyntaxKind::MINUS,
    SyntaxKind::EQ,
    SyntaxKind::GT,
    SyntaxKind::GEQ,
    SyntaxKind::LT,
    SyntaxKind::LEQ,
    SyntaxKind::NEQ,
];

pub const DATA_LOCATION: &[SyntaxKind] =
    &[SyntaxKind::CALLDATA_KW, SyntaxKind::MEMORY_KW, SyntaxKind::STORAGE_KW];
pub fn is_user_definable_op(kind: SyntaxKind) -> bool {
    USER_DEFINABLE_OP.contains(&kind)
}

pub fn is_trivia(kind: SyntaxKind) -> bool {
    kind == SyntaxKind::COMMENT || kind == SyntaxKind::WHITESPACE
}

pub fn is_data_location(kind: SyntaxKind) -> bool {
    DATA_LOCATION.contains(&kind)
}
fn with_eq_suffix(kind: SyntaxKind) -> Option<SyntaxKind> {
    Some(match kind {
        SyntaxKind::RSHIFT => SyntaxKind::RSHIFTEQ,
        SyntaxKind::LSHIFT => SyntaxKind::LSHIFTEQ,
        SyntaxKind::STAR => SyntaxKind::STAREQ,
        SyntaxKind::SLASH => SyntaxKind::SLASHEQ,
        SyntaxKind::PLUS => SyntaxKind::PLUSEQ,
        SyntaxKind::MINUS => SyntaxKind::MINUSEQ,
        SyntaxKind::PERCENT => SyntaxKind::PERCENTEQ,
        SyntaxKind::PIPE => SyntaxKind::PIPEEQ,
        SyntaxKind::AMP => SyntaxKind::AMPEQ,
        SyntaxKind::CARET => SyntaxKind::CARETEQ,
        SyntaxKind::EQ => SyntaxKind::EQ2,
        SyntaxKind::GT => SyntaxKind::GEQ,
        SyntaxKind::LT => SyntaxKind::LEQ,
        SyntaxKind::BANG => SyntaxKind::NEQ,
        _ => return None,
    })
}

#[derive(Debug)]
pub struct Token {
    pub kind: SyntaxKind,
    pub len: usize,
}

pub struct Lexer<'a> {
    text: &'a str,
    pos: usize,
    data: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(data: &'a str) -> Self {
        Self { text: data, pos: 0, data: data.chars() }
    }

    fn peek(&self) -> Option<char> {
        self.data.clone().next()
    }

    fn consume(&mut self) -> Option<char> {
        let n = self.data.next();
        self.pos += n.map(|c| c.len_utf8()).unwrap_or_default();
        n
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let pos = self.pos;
        let c = self.consume()?;
        let mut len = c.len_utf8();
        let mut kind = match c {
            c if c.is_whitespace() => {
                while let Some(c1) = self.peek() {
                    if c1.is_whitespace() {
                        self.consume();
                        len += c1.len_utf8()
                    } else {
                        break;
                    }
                }

                SyntaxKind::WHITESPACE
            }
            '{' => SyntaxKind::L_CURLY,
            '}' => SyntaxKind::R_CURLY,
            '(' => SyntaxKind::L_PAREN,
            ')' => SyntaxKind::R_PAREN,
            '[' => SyntaxKind::L_BRACK,
            ']' => SyntaxKind::R_BRACK,
            ';' => SyntaxKind::SEMICOLON,
            '>' => {
                if let Some('>') = self.peek() {
                    self.consume();
                    len += '>'.len_utf8();

                    if let Some('>') = self.peek() {
                        self.consume();
                        len += '>'.len_utf8();
                        SyntaxKind::RSHIFTZERO // >>>
                    } else {
                        SyntaxKind::RSHIFT // >>
                    }
                } else {
                    SyntaxKind::GT
                }
            }
            '<' => {
                if let Some('<') = self.peek() {
                    self.consume();
                    len += '<'.len_utf8();

                    SyntaxKind::LSHIFT
                } else {
                    SyntaxKind::LT
                }
            }
            '/' => {
                // TODO: Refactor
                if let Some(c1) = self.peek() {
                    match c1 {
                        // Comments
                        '/' => {
                            while let Some(c) = self.consume() {
                                len += c.len_utf8();
                                if c == '\n' {
                                    break;
                                }
                            }
                            SyntaxKind::COMMENT
                        }
                        '*' => {
                            len += 1;
                            self.consume();

                            let mut prev = '_';
                            while let Some(c) = self.consume() {
                                len += c.len_utf8();
                                if c == '/' && prev == '*' {
                                    break;
                                }
                                prev = c;
                            }
                            SyntaxKind::COMMENT
                        }
                        _ => SyntaxKind::SLASH,
                    }
                } else {
                    SyntaxKind::SLASH
                }
            }
            '+' => {
                if let Some('+') = self.peek() {
                    self.consume();
                    len += '+'.len_utf8();

                    SyntaxKind::PLUSPLUS
                } else {
                    SyntaxKind::PLUS
                }
            }
            '-' => {
                if let Some('-') = self.peek() {
                    self.consume();
                    len += '-'.len_utf8();

                    SyntaxKind::MINUSMINUS
                } else {
                    SyntaxKind::MINUS
                }
            }
            '*' => {
                if let Some('*') = self.peek() {
                    self.consume();
                    len += '*'.len_utf8();

                    SyntaxKind::STARSTAR
                } else {
                    SyntaxKind::STAR
                }
            }
            '%' => SyntaxKind::PERCENT,
            '!' => SyntaxKind::BANG,
            '&' => {
                if let Some('&') = self.peek() {
                    self.consume();
                    len += '&'.len_utf8();

                    SyntaxKind::AMPAMP
                } else {
                    SyntaxKind::AMP
                }
            }
            '|' => {
                if let Some('|') = self.peek() {
                    self.consume();
                    len += '|'.len_utf8();

                    SyntaxKind::PIPEPIPE
                } else {
                    SyntaxKind::PIPE
                }
            }
            '=' => {
                if let Some('>') = self.peek() {
                    self.consume();
                    len += '>'.len_utf8();

                    SyntaxKind::FAT_ARROW
                } else {
                    SyntaxKind::EQ
                }
            }
            '.' => SyntaxKind::DOT,
            '?' => SyntaxKind::QUESTION,
            ':' => SyntaxKind::COLON,
            '^' => SyntaxKind::CARET,
            '~' => SyntaxKind::TILDE,
            c @ ('"' | '\'') => {
                let mut escape = false;
                while let Some(c1) = self.consume() {
                    len += c1.len_utf8();
                    if !escape && c1 == c {
                        break;
                    }
                    escape = c1 == '\\' && !escape;
                }
                SyntaxKind::STRING
            }
            '0' => {
                let mut hex = false;
                if let Some('x') = self.peek() {
                    self.consume();
                    len += 'x'.len_utf8();
                    hex = true;
                }
                while let Some(c1) = self.peek() {
                    if matches!(c1, '0'..='9' | '_') || (hex && matches!(c1, 'a'..='f' | 'A'..='F'))
                    {
                        self.consume();
                        len += 'x'.len_utf8();
                    } else {
                        break;
                    }
                }
                if hex {
                    SyntaxKind::HEX_NUMBER
                } else {
                    SyntaxKind::DECIMAL_NUMBER
                }
            }
            '1'..='9' => {
                while let Some(c1 @ ('0'..='9' | '_' | '.')) = self.peek() {
                    self.consume();
                    len += c1.len_utf8();
                }
                if let Some('E' | 'e') = self.peek() {
                    self.consume();
                    len += 'e'.len_utf8();
                    if let Some('-') = self.peek() {
                        self.consume();
                        len += '-'.len_utf8();
                    }
                    while let Some(c1 @ ('0'..='9' | '_')) = self.peek() {
                        self.consume();
                        len += c1.len_utf8();
                    }
                }

                SyntaxKind::DECIMAL_NUMBER
            }
            'a'..='z' | 'A'..='Z' | '$' | '_' => {
                while let Some(c1 @ ('a'..='z' | 'A'..='Z' | '0'..='9' | '$' | '_')) = self.peek() {
                    self.consume();
                    len += c1.len_utf8();
                }
                let ident = &self.text[pos..pos + len];
                if ident == "hex" {
                    if let Some(c @ ('"' | '\'')) = self.peek() {
                        len += c.len_utf8();
                        self.consume();
                        let mut escape = false;
                        while let Some(c1) = self.consume() {
                            len += c1.len_utf8();
                            if !escape && c1 == c {
                                break;
                            }
                            escape = false;
                            if c1 == '\\' {
                                escape = true
                            }
                        }
                        SyntaxKind::HEX_STRING
                    } else {
                        SyntaxKind::IDENT
                    }
                } else if let Some(kw) = SyntaxKind::from_keyword(&self.text[pos..pos + len]) {
                    kw
                } else {
                    SyntaxKind::IDENT
                }
            }
            ',' => SyntaxKind::COMMA,
            _ => SyntaxKind::ERROR,
        };

        if let Some('=') = self.peek() {
            if let Some(new_kind) = with_eq_suffix(kind) {
                kind = new_kind;
                len += '='.len_utf8();
                self.consume();
            }
        }

        Some(Token { kind, len })
    }
}
