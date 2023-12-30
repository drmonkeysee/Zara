use crate::{
    lex::{
        token::{TokenErrorKind, TokenKind},
        tokenize::{
            extract::TokenExtractResult,
            scan::{ScanItem, Scanner},
        },
    },
    literal::Literal,
    number::{Integer, Number, Real, Sign},
};
use std::{fmt::Debug, ops::Range};

pub(in crate::lex::tokenize) struct Numeric<'me, 'str, R> {
    classifier: Classifier<R>,
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

impl<'me, 'str, R: Radix + Copy + Debug + Default> Numeric<'me, 'str, R> {
    fn new(scan: &'me mut Scanner<'str>, start: ScanItem<'str>, classifier: Classifier<R>) -> Self {
        Self {
            classifier,
            scan,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn scan(&mut self) -> TokenExtractResult {
        if let Some(err) = self.classify() {
            self.fail(err)
        } else {
            match self.parse()? {
                Continuation::Complete(r) => Ok(real_to_token(r, self.classifier.imaginary)),
                Continuation::Denominator(numerator) => self.scan_denominator(numerator),
                Continuation::Imaginary(r, s) => self.scan_imaginary(r, s),
            }
        }
    }

    fn classify(&mut self) -> Option<TokenErrorKind> {
        while let Some(item) = self.scan.next_if_not_delimiter() {
            if let Some(err) = self.classifier.classify(item) {
                return Some(err);
            }
            if self.classifier.done {
                break;
            }
        }
        None
    }

    fn scan_denominator(&mut self, numerator: Integer) -> TokenExtractResult {
        self.classifier.reset_as_denominator();
        if let Some(err) = self.classify() {
            self.fail(err)
        } else if let Continuation::Complete(Real::Integer(denominator)) = self.parse()? {
            Ok(real_to_token(
                Real::reduce(numerator, denominator)?,
                self.classifier.imaginary,
            ))
        } else {
            // TODO: must be an integer
            todo!();
        }
    }

    fn scan_imaginary(&mut self, real: Real, sign: Sign) -> TokenExtractResult {
        /*
        let tok = SignIdentifier::new(self.scan, sign_item).scan()?
        if let TokenKind::Imaginary(imag) = tok {
            return Ok(TokenKind::Literal(Literal::Number(Number::complex(
                real, imag,
            ))));
        }
        else
            Invalid Imaginary Part
        */
        todo!();
    }

    fn fail(&mut self, kind: TokenErrorKind) -> TokenExtractResult {
        self.scan.rest_of_token();
        Err(kind)
    }

    fn parse(&mut self) -> ClassifierResult {
        let end = self.scan.pos();
        self.classifier.parse(self.scan.lexeme(self.start.0..end))
    }
}

// NOTE: these ctors are always called after one confirmed
// decimal digit has been scanned.
impl<'me, 'str> Numeric<'me, 'str, Decimal> {
    pub(in crate::lex::tokenize) fn decimal(
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        let idx = start.0;
        Self::new(
            scan,
            start,
            Classifier {
                magnitude: Some(idx..idx + 1),
                radix: Decimal,
                ..Default::default()
            },
        )
    }

    pub(in crate::lex::tokenize) fn try_float(
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        let idx = start.0;
        Self::new(
            scan,
            start,
            Classifier {
                fraction: Some(idx..idx + 2),
                radix: Decimal,
                state: Classification::Float,
                ..Default::default()
            },
        )
    }

    pub(in crate::lex::tokenize) fn try_signed_float(
        sign: Sign,
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        let idx = start.0 + 1;
        Self::new(
            scan,
            start,
            Classifier {
                fraction: Some(idx..idx + 1),
                radix: Decimal,
                sign: Some(sign),
                state: Classification::Float,
                ..Default::default()
            },
        )
    }

    pub(in crate::lex::tokenize) fn try_signed_number(
        sign: Sign,
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        let idx = start.0 + 1;
        Self::new(
            scan,
            start,
            Classifier {
                magnitude: Some(idx..idx + 1),
                radix: Decimal,
                sign: Some(sign),
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

#[derive(Clone, Copy, Debug, Default)]
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

// TODO: handle inexact
pub(super) fn imaginary(sign: Sign) -> TokenKind {
    real_to_token(
        match sign {
            Sign::Negative => -1,
            Sign::Positive => 1,
            Sign::Zero => 0,
        },
        true,
    )
}

pub(super) fn infinity(sign: Sign, imaginary: bool) -> TokenKind {
    real_to_token(
        match sign {
            Sign::Negative => f64::NEG_INFINITY,
            Sign::Positive => f64::INFINITY,
            Sign::Zero => 0.0,
        },
        imaginary,
    )
}

pub(super) fn nan(imaginary: bool) -> TokenKind {
    real_to_token(f64::NAN, imaginary)
}

type ClassifierResult = Result<Continuation, TokenErrorKind>;

#[derive(Debug, Default)]
enum Classification {
    Exponent,
    Float,
    #[default]
    Integer,
}

enum Continuation {
    Complete(Real),
    Denominator(Integer),
    Imaginary(Real, Sign),
}

#[derive(Clone, Copy, Debug)]
enum Exactness {
    Exact,
    Inexact,
}

#[derive(Debug, Default)]
struct Classifier<R> {
    done: bool,
    exactness: Option<Exactness>,
    exponent: Option<Range<usize>>,
    exponent_sign: Option<Sign>,
    fraction: Option<Range<usize>>,
    imaginary: bool,
    magnitude: Option<Range<usize>>,
    radix: R,
    sign: Option<Sign>,
    state: Classification,
}

// TODO: imaginary numbers
impl<R: Radix + Copy + Debug + Default> Classifier<R> {
    fn reset_as_denominator(&mut self) {
        *self = Self {
            exactness: self.exactness,
            radix: self.radix,
            sign: Some(Sign::Positive),
            ..Default::default()
        }
    }

    fn classify(&mut self, item: ScanItem) -> Option<TokenErrorKind> {
        if self.imaginary {
            Some(TokenErrorKind::NumberInvalid)
        } else {
            match self.state {
                Classification::Exponent => self.scientific(item),
                Classification::Float => self.floating_point(item),
                Classification::Integer => self.integral(item),
            }
        }
    }

    fn parse(&self, input: &str) -> ClassifierResult {
        if let Some(err) = self.validate() {
            return Err(err);
        }
        match self.exactness {
            Some(Exactness::Exact) => self.parse_exact(input),
            Some(Exactness::Inexact) => self.parse_inexact(input),
            None => match self.state {
                Classification::Exponent | Classification::Float => self.parse_inexact(input),
                Classification::Integer => self.parse_int(input),
            },
        }
    }

    fn integral(&mut self, item: ScanItem) -> Option<TokenErrorKind> {
        let (idx, ch) = item;
        match ch {
            '.' => {
                if self.radix.allow_floating_point() {
                    self.state = Classification::Float;
                    self.fraction = Some(idx..idx + 1);
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
                    None
                } else {
                    Some(TokenErrorKind::NumberInvalidExponent {
                        at: idx,
                        radix: R::NAME,
                    })
                }
            }
            'i' | 'I' => {
                self.imaginary = true;
                None
            }
            _ if self.radix.is_digit(ch) => {
                debug_assert!(self.magnitude.is_some());
                self.magnitude.as_mut().unwrap().end += 1;
                None
            }
            _ => Some(TokenErrorKind::NumberInvalid),
        }
    }

    fn floating_point(&mut self, item: ScanItem) -> Option<TokenErrorKind> {
        let (idx, ch) = item;
        match ch {
            '.' => Some(TokenErrorKind::NumberUnexpectedDecimalPoint { at: idx }),
            'e' | 'E' => {
                self.state = Classification::Exponent;
                self.exponent = Some(idx..idx + 1);
                None
            }
            'i' | 'I' => {
                self.imaginary = true;
                None
            }
            _ if self.radix.is_digit(ch) => {
                debug_assert!(self.fraction.is_some());
                self.fraction.as_mut().unwrap().end += 1;
                None
            }
            _ => Some(TokenErrorKind::NumberInvalid),
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
                    self.exponent_sign = Some(super::char_to_sign(ch));
                    None
                }
            }
            'i' | 'I' => {
                if self.exponent.is_some() {
                    self.imaginary = true;
                    None
                } else {
                    Some(TokenErrorKind::NumberMalformedExponent { at: idx })
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

    fn validate(&self) -> Option<TokenErrorKind> {
        if self.imaginary && self.sign.is_none() {
            return Some(TokenErrorKind::ImaginaryMissingSign);
        }
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

    fn parse_inexact(&self, input: &str) -> ClassifierResult {
        let num = input.get(..input.len() - usize::from(self.imaginary));
        debug_assert!(num.is_some_and(|sc| !sc.is_empty()));
        let flt: f64 = num.unwrap().parse()?;
        Ok(Continuation::Complete(flt.into()))
    }

    fn parse_exact(&self, input: &str) -> ClassifierResult {
        // TODO: int vs float
        self.parse_int(input)
    }

    fn parse_int(&self, input: &str) -> ClassifierResult {
        i64::from_str_radix(input, R::BASE).map_or_else(
            |_| self.parse_sign_magnitude(input),
            |val| Ok(Continuation::Complete(val.into())),
        )
    }

    fn parse_sign_magnitude(&self, input: &str) -> ClassifierResult {
        if let Some(mag) = &self.magnitude {
            if let Some(mag_slice) = input.get(mag.start..mag.end) {
                return u64::from_str_radix(mag_slice, R::BASE).map_or_else(
                    |_| self.parse_multi_precision(input),
                    |val| {
                        let sign_mag = (self.sign.unwrap_or(Sign::Positive), val);
                        Ok(Continuation::Complete(sign_mag.into()))
                    },
                );
            }
        }
        Err(TokenErrorKind::NumberInvalid)
    }

    fn parse_multi_precision(&self, input: &str) -> ClassifierResult {
        todo!();
    }
}

fn real_to_token(r: impl Into<Real>, imaginary: bool) -> TokenKind {
    if imaginary {
        TokenKind::Imaginary(r.into())
    } else {
        TokenKind::Literal(Literal::Number(Number::real(r)))
    }
}
