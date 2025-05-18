use std::{fmt::Display};

use num_bigint::BigInt;

#[derive(Clone, Eq, Debug, PartialEq, Hash, salsa::Update)]
pub enum Literal {
    String(Vec<Vec<u8>>),
    Number(BigInt),
    Boolean(bool),
    HexString(Vec<Vec<u8>>),
    UnicodeStringLiteral(),
    Error,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(items) => {
                for i in items {
                    write!(f, "\"{}\"", String::from_utf8_lossy(i))?;
                };
            } ,
            Literal::Number(big_int) => {
                write!(f, "{}", big_int);
            },
            Literal::Boolean(b) => {
                f.write_str(if *b {
                    "true"
                } else {
                    "false"
                })?;
            },
            Literal::HexString(items) => {
                for i in items {
                    write!(f, "\"{}\"", String::from_utf8_lossy(i))?;
                };

            },
            Literal::UnicodeStringLiteral() => {
                f.write_str("Unicode string literal(unimplemented)")?;
            },
            Literal::Error => { f.write_str("{invalid literal}")?; },
        }
        Ok(())
    }
}