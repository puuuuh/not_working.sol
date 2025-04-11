use crate::hir::Literal;
use crate::lower::LowerCtx;
use num_bigint::BigInt;
use syntax::ast::nodes;

pub fn decode_string_literal(data: &str) -> Vec<u8> {
    let t = if data.starts_with("\"") { '"' } else { '\'' };
    let data = data.strip_prefix(t).unwrap_or(data);
    let data = data.strip_suffix(t).unwrap_or(data);
    data.as_bytes().to_vec()
}

impl<'db> LowerCtx<'db> {
    pub(crate) fn lower_literal(&mut self, value: nodes::Literal) -> Literal {
        match value {
            nodes::Literal::StringLiteral(s) => Literal::String(
                s.string_tokens().map(|s| decode_string_literal(s.text())).collect(),
            ),
            nodes::Literal::HexStringLiteral(s) => Literal::HexString(
                s.hex_string_tokens().map(|s| decode_string_literal(s.text())).collect(),
            ),
            nodes::Literal::DecimalNumberLiteral(d) => d
                .decimal_number_token()
                .and_then(|t| BigInt::parse_bytes(t.text().replace('_', "").as_bytes(), 10))
                .map(Literal::Number)
                .unwrap_or(Literal::Error),
            nodes::Literal::HexNumberLiteral(d) => d
                .hex_number_token()
                .and_then(|t| BigInt::parse_bytes(t.text().replace('_', "").as_bytes(), 16))
                .map(Literal::Number)
                .unwrap_or(Literal::Error),
        }
    }
}
