use num_bigint::BigInt;

#[derive(Clone, Eq, PartialEq, Debug, Hash, salsa::Update)]
pub enum Literal {
    String(Vec<Vec<u8>>),
    Number(BigInt),
    Boolean(bool),
    HexString(Vec<Vec<u8>>),
    UnicodeStringLiteral(),
    Error,
}
