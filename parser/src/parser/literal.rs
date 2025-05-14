use crate::parser::Parser;
use crate::{eat, peek};
use syntax::SyntaxKind::*;

impl<'a> Parser<'a> {
    pub(crate) fn literal(&mut self) {
        'parse: {
            match self.current() {
                STRING => {
                    self.builder.start_node(STRING_LITERAL.into());
                    while self.eat(STRING) {}
                }
                HEX_STRING => {
                    self.builder.start_node(HEX_STRING_LITERAL.into());
                    while self.eat(HEX_STRING) {}
                }
                DECIMAL_NUMBER => {
                    self.builder.start_node(DECIMAL_NUMBER_LITERAL.into());
                    self.bump_any();
                    if let Some(den) = self.eat_oneof(&[
                        WEI_KW, GWEI_KW, ETHER_KW, SECONDS_KW, MINUTES_KW, HOURS_KW, DAYS_KW,
                        WEEKS_KW, YEARS_KW,
                    ]) {
                        self.builder.start_node(SUB_DENOMINATOR.into());
                        self.eat(den);
                        self.builder.finish_node();
                    }
                }
                HEX_NUMBER => {
                    self.builder.start_node(HEX_NUMBER_LITERAL.into());
                    self.bump_any();
                    if let Some(den) = self.eat_oneof(&[
                        WEI_KW, GWEI_KW, ETHER_KW, SECONDS_KW, MINUTES_KW, HOURS_KW, DAYS_KW,
                        WEEKS_KW, YEARS_KW,
                    ]) {
                        self.builder.start_node(SUB_DENOMINATOR.into());
                        self.eat(den);
                        self.builder.finish_node();
                    }
                }
                TRUE_KW | FALSE_KW => {
                    self.builder.start_node(BOOL_LITERAL.into());
                    self.bump_any();
                }
                _ => unreachable!(),
            }
            self.builder.finish_node();
        }
    }
}
