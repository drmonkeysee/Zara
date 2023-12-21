use crate::{
    lex::{
        token::{TokenErrorKind, TokenKind},
        tokenize::{
            extract::TokenExtractResult,
            scan::{ScanItem, Scanner},
        },
    },
    literal::Literal,
    number::Number,
};

pub(in crate::lex::tokenize) struct Numeric<'me, 'str, R> {
    decimal_point: bool,
    radix: R,
    scan: &'me mut Scanner<'str>,
    sign: Option<Sign>,
    start: &'me ScanItem<'str>,
}

impl<'me, 'str, R: Radix> Numeric<'me, 'str, R> {
    fn new(scan: &'me mut Scanner<'str>, start: &'me ScanItem<'str>, radix: R) -> Self {
        Self {
            decimal_point: false,
            scan,
            radix,
            sign: None,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn scan(&mut self) -> TokenExtractResult {
        if let Some(err) = self.classify(*self.start) {
            return self.fail(err);
        }
        while let Some(item) = self.scan.next_if_not_delimiter() {
            if let Some(err) = self.classify(item) {
                return self.fail(err);
            }
        }
        self.parse()
    }

    fn classify(&mut self, item: ScanItem) -> Option<TokenErrorKind> {
        let (idx, ch) = item;
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
                        at: idx,
                        radix: self.radix.name(),
                    });
                }
                if self.decimal_point {
                    return Some(TokenErrorKind::NumberUnexpectedDecimalPoint { at: idx });
                }
                self.decimal_point = true;
            }
            '/' => todo!(),
            '@' => todo!(),
            'e' | 'E' => todo!(),
            'i' | 'I' => todo!(),
            'n' | 'N' => todo!(),
            _ if self.radix.is_digit(ch) => (),
            _ => todo!(),
        }
        None
    }

    fn fail(&mut self, kind: TokenErrorKind) -> TokenExtractResult {
        self.scan.rest_of_token();
        Err(kind)
    }

    fn parse(&mut self) -> TokenExtractResult {
        if self.decimal_point {
            let flt: f64 = self.extract_str(self.start.0).parse()?;
            return Ok(TokenKind::Literal(Literal::Number(Number::real(flt))));
        }
        todo!();
    }

    fn extract_str(&mut self, start: usize) -> &str {
        let end = self.scan.pos();
        self.scan.lexeme(start..end)
    }
}

impl<'me, 'str> Numeric<'me, 'str, Decimal> {
    pub(in crate::lex::tokenize) fn decimal(
        scan: &'me mut Scanner<'str>,
        start: &'me ScanItem<'str>,
    ) -> Self {
        Self::new(scan, start, Decimal)
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
