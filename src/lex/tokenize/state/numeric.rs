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
use std::{
    fmt::Debug,
    ops::{ControlFlow, Range},
};

pub(in crate::lex::tokenize) struct DecimalNumber<'me, 'str> {
    classifier: Classifier,
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

// NOTE: these ctors are always called after one confirmed decimal digit has been scanned
impl<'me, 'str> DecimalNumber<'me, 'str> {
    pub(in crate::lex::tokenize) fn new(
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        let idx = start.0;
        Self {
            classifier: Classifier::Int(Magnitude {
                digits: idx..idx + 1,
                ..Default::default()
            }),
            scan,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn try_signed_number(
        sign: Sign,
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        let idx = start.0 + 1;
        Self {
            classifier: Classifier::Int(Magnitude {
                digits: idx..idx + 1,
                sign: Some(sign),
                ..Default::default()
            }),
            scan,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn try_float(
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        let idx = start.0;
        Self {
            classifier: Classifier::Flt(Float {
                fraction: idx..idx + 2,
                ..Default::default()
            }),
            scan,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn try_signed_float(
        sign: Sign,
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        let idx = start.0 + 1;
        Self {
            classifier: Classifier::Flt(Float {
                fraction: idx..idx + 2,
                integral: Magnitude {
                    sign: Some(sign),
                    ..Default::default()
                },
                ..Default::default()
            }),
            scan,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn scan(&mut self) -> TokenExtractResult {
        let mut brk = Ok(BreakCondition::Complete);
        while let Some(item) = self.scan.next_if_not_delimiter() {
            match self.classifier.classify(item) {
                ControlFlow::Continue(None) => (),
                ControlFlow::Continue(Some(c)) => {
                    self.classifier = c;
                }
                ControlFlow::Break(b) => {
                    brk = b;
                    break;
                }
            }
        }
        match brk {
            Ok(cond) => {
                match cond {
                    BreakCondition::Complete => Ok(real_to_token(self.parse()?, false)),
                    BreakCondition::Complex { kind, sign, start } => {
                        let real = self.parse()?;
                        self.scan_imaginary(real, sign, kind, start)
                    }
                    BreakCondition::Fraction => {
                        // TODO: handle inexact e.g #i4/3 => 1.3333...
                        // this means numerator's parse should not apply inexact modifier too early
                        let real = self.parse()?;
                        debug_assert!(matches!(real, Real::Integer(_)));
                        if let Real::Integer(numerator) = self.parse()? {
                            self.scan_denominator(numerator)
                        } else {
                            unreachable!();
                        }
                    }
                    BreakCondition::Imaginary => {
                        if let Some(item) = self.scan.next_if_not_delimiter() {
                            // NOTE: maybe malformed "in"finity? otherwise assume malformed imaginary
                            self.fail(if item.1.is_ascii_alphabetic() {
                                TokenErrorKind::NumberInvalid
                            } else {
                                TokenErrorKind::ImaginaryMalformed
                            })
                        } else if !self.classifier.has_sign() {
                            self.fail(TokenErrorKind::ImaginaryMissingSign)
                        } else {
                            Ok(real_to_token(self.parse()?, true))
                        }
                    }
                }
            }
            Err(err) => self.fail(err),
        }
    }

    fn parse(&mut self) -> ParseResult {
        let end = self.scan.pos();
        self.classifier.parse(self.scan.lexeme(self.start.0..end))
    }

    fn scan_denominator(&mut self, numerator: Integer) -> TokenExtractResult {
        todo!();
        /*
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
        */
    }

    fn scan_imaginary(
        &mut self,
        real: Real,
        sign: Sign,
        kind: ComplexKind,
        start: ScanItem,
    ) -> TokenExtractResult {
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

    fn fail(&mut self, err: TokenErrorKind) -> TokenExtractResult {
        self.scan.end_of_token();
        Err(err)
    }
}

struct RadixNumber<'me, 'str, R> {
    classifier: Integral<R>,
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

pub(in crate::lex::tokenize) enum RadixDigit {
    Digit,
    InvalidDigit,
    NonDigit,
}

pub(in crate::lex::tokenize) trait RadixPolicy {
    const BASE: u32;
    const NAME: &'static str;

    fn is_digit(&self, ch: char) -> RadixDigit;
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

#[derive(Debug)]
enum ComplexKind {
    Cartesian,
    Polar,
}

#[derive(Clone, Copy, Debug)]
enum Exactness {
    Exact,
    Inexact,
}

type DecimalControl<'str> =
    ControlFlow<Result<BreakCondition<'str>, TokenErrorKind>, Option<Classifier>>;
type RadixControl<'str> = ControlFlow<Result<BreakCondition<'str>, TokenErrorKind>>;
type ParseResult = Result<Real, TokenErrorKind>;

#[derive(Debug)]
enum BreakCondition<'str> {
    Complete,
    Complex {
        kind: ComplexKind,
        sign: Sign,
        start: ScanItem<'str>,
    },
    Fraction,
    Imaginary,
}

#[derive(Debug)]
enum Classifier {
    Flt(Float),
    Int(Magnitude),
    Sci(Scientific),
}

impl Classifier {
    fn has_sign(&self) -> bool {
        match self {
            Self::Flt(f) => f.integral.sign,
            Self::Int(i) => i.sign,
            Self::Sci(s) => s.significand.integral.sign,
        }
        .is_some()
    }

    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        match self {
            Self::Flt(f) => f.classify(item),
            Self::Int(i) => i.classify(item),
            Self::Sci(s) => s.classify(item),
        }
    }

    fn parse(&self, input: &str) -> ParseResult {
        match self {
            Self::Flt(f) => f.parse(input),
            Self::Int(i) => i.parse(input),
            Self::Sci(s) => s.parse(input),
        }
    }
}

#[derive(Debug)]
struct Integral<R> {
    mag: Magnitude,
    radix: R,
}

impl<R: RadixPolicy> Integral<R> {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> RadixControl<'str> {
        let (idx, ch) = item;
        match ch {
            '+' | '-' => {
                if self.mag.digits.is_empty() {
                    if self.mag.sign.is_none() {
                        self.mag.sign = Some(super::char_to_sign(ch));
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(Err(TokenErrorKind::NumberInvalid))
                    }
                } else {
                    todo!(); // begin imaginary part
                }
            }
            '.' => ControlFlow::Break(Err(TokenErrorKind::NumberInvalidDecimalPoint {
                at: idx,
                radix: R::NAME,
            })),
            'e' | 'E' => ControlFlow::Break(Err(TokenErrorKind::NumberInvalidExponent {
                at: idx,
                radix: R::NAME,
            })),
            'i' | 'I' => ControlFlow::Break(Ok(BreakCondition::Imaginary)),
            _ => match self.radix.is_digit(ch) {
                RadixDigit::Digit => {
                    if self.mag.digits.is_empty() {
                        self.mag.digits = idx..idx;
                    }
                    self.mag.digits.end += 1;
                    ControlFlow::Continue(())
                }
                RadixDigit::InvalidDigit => todo!(),
                RadixDigit::NonDigit => ControlFlow::Break(Err(TokenErrorKind::NumberInvalid)),
            },
            // TODO: /, @
        }
    }
}

#[derive(Clone, Debug, Default)]
struct Magnitude {
    digits: Range<usize>,
    exactness: Option<Exactness>,
    sign: Option<Sign>,
}

impl Magnitude {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        let (idx, ch) = item;
        match ch {
            '.' => ControlFlow::Continue(Some(Classifier::Flt(Float {
                fraction: idx..idx + 1,
                integral: self.clone(),
            }))),
            '/' => ControlFlow::Break(Ok(BreakCondition::Fraction)),
            'e' | 'E' => ControlFlow::Continue(Some(Classifier::Sci(Scientific {
                exponent: idx..idx + 1,
                significand: Float {
                    integral: self.clone(),
                    fraction: idx..idx,
                    ..Default::default()
                },
                ..Default::default()
            }))),
            'i' | 'I' => ControlFlow::Break(Ok(BreakCondition::Imaginary)),
            _ if ch.is_ascii_digit() => {
                self.digits.end += 1;
                ControlFlow::Continue(None)
            }
            _ => ControlFlow::Break(Err(TokenErrorKind::NumberInvalid)),
        }
        // TODO: +-, @
    }

    fn parse(&self, input: &str) -> ParseResult {
        if let Some(sign_mag) = input.get(..self.digits.end) {
            match self.exactness {
                None | Some(Exactness::Exact) => i64::from_str_radix(sign_mag, 10).map_or_else(
                    |_| self.parse_sign_magnitude(sign_mag),
                    |val| Ok(val.into()),
                ),
                Some(Exactness::Inexact) => todo!(),
            }
        } else {
            Err(TokenErrorKind::NumberInvalid)
        }
    }

    fn parse_sign_magnitude(&self, input: &str) -> ParseResult {
        if let Some(mag) = input.get(self.digits.start..) {
            return u64::from_str_radix(mag, 10).map_or_else(
                |_| self.parse_multi_precision(input),
                |val| {
                    let sign_mag = (self.sign.unwrap_or(Sign::Positive), val);
                    Ok(sign_mag.into())
                },
            );
        }
        Err(TokenErrorKind::NumberInvalid)
    }

    fn parse_multi_precision(&self, input: &str) -> ParseResult {
        todo!();
    }
}

#[derive(Clone, Debug, Default)]
struct Float {
    fraction: Range<usize>,
    integral: Magnitude,
}

impl Float {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        let (idx, ch) = item;
        match ch {
            '.' => ControlFlow::Break(Err(TokenErrorKind::NumberUnexpectedDecimalPoint {
                at: idx,
            })),
            '/' => ControlFlow::Break(Err(TokenErrorKind::RationalInvalid)),
            'e' | 'E' => ControlFlow::Continue(Some(Classifier::Sci(Scientific {
                exponent: idx..idx + 1,
                significand: self.clone(),
                ..Default::default()
            }))),
            'i' | 'I' => ControlFlow::Break(Ok(BreakCondition::Imaginary)),
            _ if ch.is_ascii_digit() => {
                self.fraction.end += 1;
                ControlFlow::Continue(None)
            }
            _ => ControlFlow::Break(Err(TokenErrorKind::NumberInvalid)),
        }
        // TODO: +-, @
    }

    fn parse(&self, input: &str) -> ParseResult {
        self.parse_to(input, self.fraction.end)
    }

    fn parse_to(&self, input: &str, end: usize) -> ParseResult {
        if let Some(numstr) = input.get(..end) {
            match self.integral.exactness {
                None | Some(Exactness::Inexact) => {
                    let flt: f64 = numstr.parse()?;
                    Ok(flt.into())
                }
                Some(Exactness::Exact) => todo!(),
            }
        } else {
            Err(TokenErrorKind::NumberInvalid)
        }
    }
}

#[derive(Debug, Default)]
struct Scientific {
    exponent: Range<usize>,
    exponent_sign: Option<Sign>,
    significand: Float,
}

impl Scientific {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        let ch = item.1;
        match ch {
            '+' | '-' => {
                if self.exponent_sign.is_some() {
                    if self.exponent.len() == 2 {
                        ControlFlow::Break(Err(self.malformed_exponent()))
                    } else {
                        // TODO: begin imaginary part
                        todo!();
                    }
                } else {
                    self.exponent_sign = Some(super::char_to_sign(ch));
                    self.exponent.end += 1;
                    ControlFlow::Continue(None)
                }
            }
            '/' => ControlFlow::Break(Err(TokenErrorKind::RationalInvalid)),
            'i' | 'I' => ControlFlow::Break(if self.no_e_value() {
                Err(self.malformed_exponent())
            } else {
                Ok(BreakCondition::Imaginary)
            }),
            _ if ch.is_ascii_digit() => {
                self.exponent.end += 1;
                ControlFlow::Continue(None)
            }
            _ => ControlFlow::Break(Err(self.malformed_exponent())),
        }
        // TODO: +-, @
    }

    fn parse(&self, input: &str) -> ParseResult {
        if self.no_e_value() {
            return Err(self.malformed_exponent());
        }
        self.significand.parse_to(input, self.exponent.end)
    }

    fn no_e_value(&self) -> bool {
        // NOTE: exponent range contains no digits but may include 'e' and sign
        self.exponent.len() <= 1 || (self.exponent.len() == 2 && self.exponent_sign.is_some())
    }

    fn malformed_exponent(&self) -> TokenErrorKind {
        TokenErrorKind::NumberMalformedExponent {
            at: self.exponent.start,
        }
    }
}

/*
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
*/

fn real_to_token(r: impl Into<Real>, imaginary: bool) -> TokenKind {
    if imaginary {
        TokenKind::Imaginary(r.into())
    } else {
        TokenKind::Literal(Literal::Number(Number::real(r)))
    }
}
