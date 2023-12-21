use crate::{
    lex::{
        token::{TokenErrorKind, TokenKind},
        tokenize::{extract::TokenExtractResult, scan::Scanner},
    },
    literal::Literal,
    number::Number,
};

pub(in crate::lex::tokenize) struct Numeric<'me, 'str, R> {
    decimal_point: bool,
    radix: R,
    scan: &'me mut Scanner<'str>,
    sign: Option<Sign>,
}

impl<'me, 'str, R: Radix> Numeric<'me, 'str, R> {
    fn new(scan: &'me mut Scanner<'str>, radix: R) -> Self {
        Self {
            decimal_point: false,
            scan,
            radix,
            sign: None,
        }
    }

    pub(in crate::lex::tokenize) fn scan(&mut self, first: char) -> TokenExtractResult {
        if let Some(err) = self.classify_char(first) {
            return self.fail(err);
        }
        while let Some(ch) = self.scan.char_if_not_delimiter() {
            if let Some(err) = self.classify_char(ch) {
                return self.fail(err);
            }
        }
        self.parse()
    }

    fn classify_char(&mut self, ch: char) -> Option<TokenErrorKind> {
        match ch {
            '+' | '-' => {
                if self.sign.is_some() {
                    todo!();
                }
                self.sign = Some(if ch == '+' {
                    Sign::Positive
                } else {
                    Sign::Negative
                });
            }
            '.' => {
                if !self.radix.allow_decimal_point() {
                    return Some(TokenErrorKind::NumberInvalidDecimalPoint {
                        at: 0,
                        radix: self.radix.name(),
                    });
                }
                if self.decimal_point {
                    return Some(TokenErrorKind::NumberUnexpectedDecimalPoint { at: 0 });
                }
                self.decimal_point = true;
                todo!();
            }
            '/' => todo!(),
            '@' => todo!(),
            'e' | 'E' => todo!(),
            'i' | 'I' => todo!(),
            'n' | 'N' => todo!(),
            _ if self.radix.is_digit(ch) => todo!(),
            _ => todo!(),
        }
        None
    }

    fn fail(&mut self, kind: TokenErrorKind) -> TokenExtractResult {
        self.scan.rest_of_token();
        Err(kind)
    }

    fn parse(&self) -> TokenExtractResult {
        todo!();
    }
}

impl<'me, 'str> Numeric<'me, 'str, Decimal> {
    pub(in crate::lex::tokenize) fn decimal(scan: &'me mut Scanner<'str>) -> Self {
        Self::new(scan, Decimal)
    }
}

pub(in crate::lex::tokenize) trait Radix {
    fn is_digit(&self, ch: char) -> bool;
    fn name(&self) -> &'static str;

    fn allow_decimal_point(&self) -> bool {
        false
    }
}

pub(in crate::lex::tokenize) struct Decimal;

impl Radix for Decimal {
    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }

    fn name(&self) -> &'static str {
        "decimal"
    }

    fn allow_decimal_point(&self) -> bool {
        true
    }
}

pub(super) enum Sign {
    Negative,
    Positive,
}

pub(super) fn imaginary(sign: Sign) -> TokenKind {
    TokenKind::Literal(Literal::Number(Number::complex(
        0,
        match sign {
            Sign::Negative => -1,
            Sign::Positive => 1,
        },
    )))
}

pub(super) fn infinity(sign: Sign) -> TokenKind {
    TokenKind::Literal(Literal::Number(Number::real(match sign {
        Sign::Negative => f64::NEG_INFINITY,
        Sign::Positive => f64::INFINITY,
    })))
}

pub(super) fn nan() -> TokenKind {
    TokenKind::Literal(Literal::Number(Number::real(f64::NAN)))
}
