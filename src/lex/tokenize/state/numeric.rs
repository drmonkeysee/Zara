use crate::{
    lex::{
        token::{TokenErrorKind, TokenKind},
        tokenize::{
            extract::TokenExtractResult,
            scan::{ScanItem, Scanner},
        },
    },
    literal::Literal,
    number::{Number, Real},
};
use std::{fmt::Debug, ops::Range};

pub(in crate::lex::tokenize) struct Numeric<'me, 'str, R> {
    classifier: Classifier<R>,
    scan: &'me mut Scanner<'str>,
    start: &'me ScanItem<'str>,
}

impl<'me, 'str, R: Radix + Default + Debug> Numeric<'me, 'str, R> {
    fn new(
        scan: &'me mut Scanner<'str>,
        start: &'me ScanItem<'str>,
        classifier: Classifier<R>,
    ) -> Self {
        Self {
            classifier,
            scan,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn scan(&mut self) -> TokenExtractResult {
        while let Some(item) = self.scan.next_if_not_delimiter() {
            if let Some(err) = self.classifier.classify(item) {
                return self.fail(err);
            }
        }
        self.parse()
    }

    fn fail(&mut self, kind: TokenErrorKind) -> TokenExtractResult {
        self.scan.rest_of_token();
        Err(kind)
    }

    fn parse(&mut self) -> TokenExtractResult {
        let end = self.scan.pos();
        self.classifier.parse(self.scan.lexeme(self.start.0..end))
    }
}

impl<'me, 'str> Numeric<'me, 'str, Decimal> {
    pub(in crate::lex::tokenize) fn decimal(
        scan: &'me mut Scanner<'str>,
        start: &'me ScanItem<'str>,
    ) -> Self {
        let idx = start.0;
        Self::new(
            scan,
            start,
            Classifier {
                magnitude: Some(idx..idx + 1),
                radix: Decimal,
                sign: Some(Sign::Positive),
                ..Default::default()
            },
        )
    }
}

pub(in crate::lex::tokenize) trait Radix {
    const BASE: u32;
    const NAME: &'static str;

    fn is_digit(&self, ch: char) -> bool;

    fn allow_floating_point(&self) -> bool {
        false
    }
}

#[derive(Debug, Default)]
pub(in crate::lex::tokenize) struct Decimal;

impl Radix for Decimal {
    const BASE: u32 = 10;
    const NAME: &'static str = "decimal";

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }

    fn allow_floating_point(&self) -> bool {
        true
    }
}

#[derive(Debug)]
pub(super) enum Sign {
    Negative,
    Positive,
}

// TODO: handle inexact
pub(super) fn imaginary(sign: Sign) -> TokenKind {
    real_to_token(
        match sign {
            Sign::Negative => -1,
            Sign::Positive => 1,
        },
        true,
    )
}

pub(super) fn infinity(sign: Sign, imaginary: bool) -> TokenKind {
    real_to_token(
        match sign {
            Sign::Negative => f64::NEG_INFINITY,
            Sign::Positive => f64::INFINITY,
        },
        imaginary,
    )
}

pub(super) fn nan(imaginary: bool) -> TokenKind {
    real_to_token(f64::NAN, imaginary)
}

#[derive(Debug, Default)]
enum Classification {
    Exponent,
    Float,
    #[default]
    Integer,
}

#[derive(Debug)]
enum Exactness {
    Exact,
    Inexact,
}

#[derive(Debug, Default)]
struct Classifier<R> {
    exactness: Option<Exactness>,
    exponent: Option<Range<usize>>,
    exponent_sign: Option<Sign>,
    fraction: Option<Range<usize>>,
    magnitude: Option<Range<usize>>,
    radix: R,
    sign: Option<Sign>,
    state: Classification,
}

// TODO: imaginary numbers
impl<R: Radix + Default + Debug> Classifier<R> {
    fn classify(&mut self, item: ScanItem) -> Option<TokenErrorKind> {
        match self.state {
            Classification::Exponent => self.scientific(item),
            Classification::Float => self.floating_point(item),
            Classification::Integer => self.integral(item),
        }
    }

    fn parse(&self, input: &str) -> TokenExtractResult {
        if let Some(err) = self.validate() {
            return Err(err);
        }
        match self.exactness {
            Some(Exactness::Inexact) => self.parse_inexact(input),
            _ => self.parse_exact(input),
        }
    }

    fn integral(&mut self, item: ScanItem) -> Option<TokenErrorKind> {
        let (idx, ch) = item;
        match ch {
            '.' => {
                if self.radix.allow_floating_point() {
                    self.state = Classification::Float;
                    self.fraction = Some(idx..idx + 1);
                    if self.exactness.is_none() {
                        self.exactness = Some(Exactness::Inexact);
                    }
                    None
                } else {
                    Some(TokenErrorKind::NumberInvalidDecimalPoint {
                        at: idx,
                        radix: R::NAME,
                    })
                }
            }
            'e' | 'E' => {
                if self.radix.allow_floating_point() {
                    self.state = Classification::Exponent;
                    self.exponent = Some(idx..idx + 1);
                    if self.exactness.is_none() {
                        self.exactness = Some(Exactness::Inexact);
                    }
                    None
                } else {
                    Some(TokenErrorKind::NumberInvalidExponent {
                        at: idx,
                        radix: R::NAME,
                    })
                }
            }
            _ if self.radix.is_digit(ch) => {
                debug_assert!(self.magnitude.is_some());
                self.magnitude.as_mut().unwrap().end += 1;
                None
            }
            _ => self.invalid(),
        }
    }

    fn floating_point(&mut self, item: ScanItem) -> Option<TokenErrorKind> {
        let (idx, ch) = item;
        match ch {
            '.' => Some(TokenErrorKind::NumberUnexpectedDecimalPoint { at: idx }),
            'e' | 'E' => {
                self.state = Classification::Exponent;
                self.exponent = Some(idx..idx + 1);
                if self.exactness.is_none() {
                    self.exactness = Some(Exactness::Inexact);
                }
                None
            }
            _ if self.radix.is_digit(ch) => {
                debug_assert!(self.fraction.is_some());
                self.fraction.as_mut().unwrap().end += 1;
                None
            }
            _ => self.invalid(),
        }
    }

    fn scientific(&mut self, item: ScanItem) -> Option<TokenErrorKind> {
        let (idx, ch) = item;
        match ch {
            '+' | '-' => {
                debug_assert!(self.exponent.is_some());
                self.exponent.as_mut().unwrap().end += 1;
                if self.exponent_sign.is_some() {
                    Some(TokenErrorKind::NumberMalformedExponent { at: idx })
                } else {
                    self.exponent_sign = Some(if ch == '+' {
                        Sign::Positive
                    } else {
                        Sign::Negative
                    });
                    None
                }
            }
            _ if self.radix.is_digit(ch) => {
                debug_assert!(self.exponent.is_some());
                self.exponent.as_mut().unwrap().end += 1;
                None
            }
            _ => Some(TokenErrorKind::NumberMalformedExponent { at: idx }),
        }
    }

    fn invalid(&self) -> Option<TokenErrorKind> {
        Some(TokenErrorKind::NumberInvalid)
    }

    fn validate(&self) -> Option<TokenErrorKind> {
        match self.state {
            Classification::Exponent => {
                debug_assert!(self.exponent.is_some());
                let exp = self.exponent.as_ref().unwrap();
                debug_assert!(!exp.is_empty());
                if exp.len() == 1 || (exp.len() == 2 && self.exponent_sign.is_some()) {
                    return Some(TokenErrorKind::NumberMalformedExponent { at: exp.start });
                }
            }
            Classification::Float => {
                debug_assert!(
                    self.magnitude.as_ref().is_some_and(|r| !r.is_empty())
                        || self.fraction.as_ref().is_some_and(|r| !r.is_empty())
                );
            }
            Classification::Integer => {
                debug_assert!(self.magnitude.as_ref().is_some_and(|r| !r.is_empty()));
            }
        }
        None
    }

    fn parse_exact(&self, input: &str) -> TokenExtractResult {
        // TODO: handle u64+sign, exact vs non-specified
        let int = i64::from_str_radix(input, R::BASE)?;
        Ok(TokenKind::Literal(Literal::Number(Number::real(int))))
    }

    fn parse_inexact(&self, input: &str) -> TokenExtractResult {
        let flt: f64 = input.parse()?;
        Ok(TokenKind::Literal(Literal::Number(Number::real(flt))))
    }
}

fn real_to_token(r: impl Into<Real>, imaginary: bool) -> TokenKind {
    if imaginary {
        TokenKind::Imaginary(r.into())
    } else {
        TokenKind::Literal(Literal::Number(Number::real(r)))
    }
}
