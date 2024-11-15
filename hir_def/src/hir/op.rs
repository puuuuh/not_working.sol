use std::fmt::{Display, Formatter, Write};
use syntax::{SyntaxToken, T};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    LogicOp(LogicOp),
    ArithOp(ArithOp),
    CmpOp(CmpOp),
    Assignment { op: Option<ArithOp> },
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::LogicOp(op) => {
                Display::fmt(op, f)?;
            }
            BinaryOp::ArithOp(op) => {
                Display::fmt(op, f)?;
            }
            BinaryOp::CmpOp(op) => {
                Display::fmt(op, f)?;
            }
            BinaryOp::Assignment { op } => {
                if let Some(op) = op {
                    Display::fmt(op, f)?;
                }
                f.write_str("=")?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PostfixOp {
    Inc,
    Dec,
}

impl Display for PostfixOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            PostfixOp::Inc => "++",
            PostfixOp::Dec => "--",
        })
    }
}

impl From<SyntaxToken> for PostfixOp {
    fn from(value: SyntaxToken) -> Self {
        match value.kind() {
            T![--] => Self::Dec,
            T![++] => Self::Inc,
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, derive_more::Display)]
pub enum PrefixOp {
    #[display("++")]
    Inc,
    #[display("--")]
    Dec,
    #[display("!")]
    Not,
    #[display("~")]
    BitNot,
    #[display("delete ")]
    Delete,
    #[display("-")]
    Minus,
}

impl From<SyntaxToken> for PrefixOp {
    fn from(value: SyntaxToken) -> Self {
        match value.kind() {
            T![-] => Self::Minus,
            T![++] => Self::Inc,
            T![--] => Self::Dec,
            T![~] => Self::BitNot,
            T![delete] => Self::Delete,
            T![!] => Self::Not,
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, derive_more::Display)]
pub enum LogicOp {
    #[display("&&")]
    And,
    #[display("||")]
    Or,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, derive_more::Display)]
pub enum ArithOp {
    #[display("&")]
    BitAnd,
    #[display("|")]
    BitOr,
    #[display("^")]
    BitXor,
    #[display("<<")]
    LShift,
    #[display(">>")]
    RShift,
    #[display(">>>")]
    RShiftZero,
    #[display("+")]
    Add,
    #[display("-")]
    Sub,
    #[display("*")]
    Mul,
    #[display("/")]
    Div,
    #[display("%")]
    Rem,
    #[display("**")]
    Pow,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    Eq { inverted: bool },
    Ord { ordering: Ordering, eq: bool },
}

impl Display for CmpOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CmpOp::Eq { inverted } => {
                if *inverted {
                    f.write_str("!=")?;
                } else {
                    f.write_str("==")?;
                }
            }
            CmpOp::Ord { ordering, eq } => {
                f.write_char(match ordering {
                    Ordering::Less => '<',
                    Ordering::Greater => '>',
                })?;
                if *eq {
                    f.write_char('=')?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ordering {
    Less,
    Greater,
}
impl From<SyntaxToken> for BinaryOp {
    fn from(value: SyntaxToken) -> Self {
        match value.kind() {
            T![||] => Self::LogicOp(LogicOp::Or),
            T![&&] => Self::LogicOp(LogicOp::And),

            T![==] => Self::CmpOp(CmpOp::Eq { inverted: false }),
            T![!=] => Self::CmpOp(CmpOp::Eq { inverted: true }),
            T![<=] => Self::CmpOp(CmpOp::Ord { ordering: Ordering::Less, eq: true }),
            T![>=] => Self::CmpOp(CmpOp::Ord { ordering: Ordering::Greater, eq: true }),
            T![<] => Self::CmpOp(CmpOp::Ord { ordering: Ordering::Less, eq: false }),
            T![>] => Self::CmpOp(CmpOp::Ord { ordering: Ordering::Greater, eq: false }),

            T![|] => Self::ArithOp(ArithOp::BitOr),
            T![&] => Self::ArithOp(ArithOp::BitAnd),
            T![^] => Self::ArithOp(ArithOp::BitXor),
            T![<<] => Self::ArithOp(ArithOp::LShift),
            T![>>] => Self::ArithOp(ArithOp::RShift),
            T![>>>] => Self::ArithOp(ArithOp::RShiftZero),
            T![+] => Self::ArithOp(ArithOp::Add),
            T![-] => Self::ArithOp(ArithOp::Sub),
            T![*] => Self::ArithOp(ArithOp::Mul),
            T![/] => Self::ArithOp(ArithOp::Div),
            T![%] => Self::ArithOp(ArithOp::Rem),
            T![**] => Self::ArithOp(ArithOp::Pow),

            T![=] => Self::Assignment { op: None },

            T![+=] => Self::Assignment { op: Some(ArithOp::Add) },
            T![-=] => Self::Assignment { op: Some(ArithOp::Sub) },
            T![*=] => Self::Assignment { op: Some(ArithOp::Mul) },
            T![/=] => Self::Assignment { op: Some(ArithOp::Div) },
            T![%=] => Self::Assignment { op: Some(ArithOp::Rem) },

            T![|=] => Self::Assignment { op: Some(ArithOp::BitOr) },
            T![^=] => Self::Assignment { op: Some(ArithOp::BitXor) },
            T![&=] => Self::Assignment { op: Some(ArithOp::BitAnd) },
            T![<<=] => Self::Assignment { op: Some(ArithOp::LShift) },
            T![>>=] => Self::Assignment { op: Some(ArithOp::RShift) },
            T![>>>=] => Self::Assignment { op: Some(ArithOp::RShiftZero) },
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update, derive_more::Display)]
pub enum UserDefineableOp {
    #[display("&")]
    BitAnd,
    #[display("~")]
    BitNot,
    #[display("|")]
    BitOr,
    #[display("^")]
    BitXor,
    #[display("+")]
    Add,
    #[display("/")]
    Div,
    #[display("%")]
    Rem,
    #[display("*")]
    Mul,
    #[display("-")]
    Sub,
    #[display("==")]
    Eq,
    #[display(">")]
    Gt,
    #[display(">=")]
    Ge,
    #[display("<")]
    Lt,
    #[display("<=")]
    Le,
    #[display("!=")]
    NotEq,
}

impl From<SyntaxToken> for UserDefineableOp {
    fn from(value: SyntaxToken) -> Self {
        match value.kind() {
            T![==] => Self::Eq,
            T![!=] => Self::NotEq,
            T![<=] => Self::Le,
            T![>=] => Self::Ge,
            T![<] => Self::Lt,
            T![>] => Self::Gt,

            T![|] => Self::BitOr,
            T![&] => Self::BitAnd,
            T![^] => Self::BitXor,
            T![+] => Self::Add,
            T![-] => Self::Sub,
            T![*] => Self::Mul,
            T![/] => Self::Div,
            T![%] => Self::Rem,

            _ => unreachable!(),
        }
    }
}
