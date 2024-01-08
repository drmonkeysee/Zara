use super::Identifier;
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
    exactness: Option<Exactness>,
    scan: &'me mut Scanner<'str>,
    spec: ClassifierSpec,
    start: ScanItem<'str>,
}

// NOTE: these ctors are always called after one confirmed decimal digit has been scanned
impl<'me, 'str> DecimalNumber<'me, 'str> {
    pub(in crate::lex::tokenize) fn new(
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        Self {
            exactness: None,
            scan,
            spec: ClassifierSpec::New,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn try_signed_number(
        sign: Sign,
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        Self {
            exactness: None,
            scan,
            spec: ClassifierSpec::SignedNumber(sign),
            start,
        }
    }

    pub(in crate::lex::tokenize) fn try_float(
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        Self {
            exactness: None,
            scan,
            spec: ClassifierSpec::Float,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn try_signed_float(
        sign: Sign,
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        Self {
            exactness: None,
            scan,
            spec: ClassifierSpec::SignedFloat(sign),
            start,
        }
    }

    pub(in crate::lex::tokenize) fn scan(&mut self) -> TokenExtractResult {
        let mut brk = Ok(BreakCondition::Complete);
        let mut classifier = self.spec.new_classifier();
        while let Some(item) = self.scan.next_if_not_delimiter() {
            match classifier.classify(item) {
                ControlFlow::Continue(None) => (),
                ControlFlow::Continue(Some(c)) => {
                    classifier = c;
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
                    BreakCondition::Complete => self.complete(&classifier, false),
                    BreakCondition::Complex { kind, start } => match self.parse(&classifier) {
                        Ok(real) => self.scan_imaginary(real, kind, start),
                        Err(err) => self.fail(err),
                    },
                    BreakCondition::Fraction(m) => {
                        // TODO: handle inexact e.g #i4/3 => 1.3333...
                        match m.exact_parse(self.get_lexeme()) {
                            Ok(numerator) => {
                                self.scan_denominator(numerator, classifier.has_sign())
                            }
                            Err(err) => self.fail(err),
                        }
                    }
                    BreakCondition::Imaginary => {
                        if let Some(item) = self.scan.next_if_not_delimiter() {
                            // NOTE: maybe malformed "in"finity? otherwise assume malformed imaginary
                            self.fail(if item.1.is_ascii_alphabetic() {
                                TokenErrorKind::NumberInvalid
                            } else {
                                TokenErrorKind::ImaginaryInvalid
                            })
                        } else if !classifier.has_sign() {
                            self.fail(TokenErrorKind::ImaginaryMissingSign)
                        } else {
                            self.complete(&classifier, true)
                        }
                    }
                }
            }
            Err(err) => self.fail(err),
        }
    }

    fn scan_denominator(&mut self, numerator: Integer, explicit_sign: bool) -> TokenExtractResult {
        let mut d = DenominatorNumber::new(self.scan);
        let (denominator, cond) = d.scan::<Decimal>()?;
        let real = Real::reduce(numerator, denominator)?;
        match cond {
            BreakCondition::Complete => Ok(real_to_token(real, false)),
            BreakCondition::Complex { kind, start } => todo!(),
            BreakCondition::Imaginary => {
                if explicit_sign {
                    Ok(real_to_token(real, true))
                } else {
                    Err(TokenErrorKind::ImaginaryMissingSign)
                }
            }
            BreakCondition::Fraction(_) => unreachable!(),
        }
    }

    fn scan_imaginary(
        &mut self,
        real: Real,
        kind: ComplexKind,
        start: ScanItem,
    ) -> TokenExtractResult {
        debug_assert!(matches!(start.1, '+' | '-'));
        match Identifier::new(self.scan, start).scan() {
            Ok(TokenKind::Imaginary(imag)) => Ok(TokenKind::Literal(Literal::Number(match kind {
                ComplexKind::Cartesian => Number::complex(real, imag),
                ComplexKind::Polar => todo!(),
            }))),
            _ => self.fail(TokenErrorKind::ComplexInvalid),
        }
    }

    fn complete(&mut self, classifier: &Classifier, imaginary: bool) -> TokenExtractResult {
        self.parse(classifier)
            .map_or_else(|err| self.fail(err), |r| Ok(real_to_token(r, imaginary)))
    }

    fn fail(&mut self, err: TokenErrorKind) -> TokenExtractResult {
        self.scan.end_of_token();
        Err(err)
    }

    fn parse(&mut self, classifier: &Classifier) -> ParseResult {
        let exactness = self.exactness;
        classifier.parse(self.get_lexeme(), exactness)
    }

    fn get_lexeme(&mut self) -> &str {
        self.scan.current_lexeme_at(self.start.0)
    }
}

enum ClassifierSpec {
    Float,
    New,
    SignedFloat(Sign),
    SignedNumber(Sign),
}

impl ClassifierSpec {
    fn new_classifier(&self) -> Classifier {
        match self {
            Self::Float => Classifier::Flt(Float {
                fraction: 0..2,
                ..Default::default()
            }),
            Self::New => Classifier::Int(DecimalInt(Magnitude {
                digits: 0..1,
                ..Default::default()
            })),
            Self::SignedFloat(s) => Classifier::Flt(Float {
                fraction: 1..3,
                integral: Magnitude {
                    sign: Some(*s),
                    ..Default::default()
                },
            }),
            Self::SignedNumber(s) => Classifier::Int(DecimalInt(Magnitude {
                digits: 1..2,
                sign: Some(*s),
                ..Default::default()
            })),
        }
    }
}

struct DenominatorNumber<'me, 'str> {
    scan: &'me mut Scanner<'str>,
    start: usize,
}

impl<'me, 'str> DenominatorNumber<'me, 'str> {
    fn new(scan: &'me mut Scanner<'str>) -> Self {
        let start = scan.pos();
        Self { scan, start }
    }

    fn scan<R: Radix + Debug + Default>(
        &mut self,
    ) -> Result<(Integer, BreakCondition<R>), TokenErrorKind> {
        let mut brk = Ok(BreakCondition::<R>::Complete);
        let mut classifier = Integral(Magnitude {
            sign: Some(Sign::Positive),
            ..Default::default()
        });
        while let Some(item) = self.scan.next_if_not_delimiter() {
            match classifier.classify(item) {
                ControlFlow::Continue(()) => (),
                ControlFlow::Break(b) => {
                    brk = b;
                    break;
                }
            }
        }
        match brk {
            Ok(cond) => Ok((
                match cond {
                    BreakCondition::Complete => classifier
                        .0
                        .exact_parse(self.get_lexeme())
                        .map_err(|_| self.fail()),
                    BreakCondition::Complex { kind, start } => todo!(),
                    BreakCondition::Fraction(_) => Err(self.fail()),
                    BreakCondition::Imaginary => {
                        if self.scan.next_if_not_delimiter().is_some() {
                            Err(self.fail())
                        } else {
                            classifier
                                .0
                                .exact_parse(self.get_lexeme())
                                .map_err(|_| self.fail())
                        }
                    }
                }?,
                cond,
            )),
            _ => Err(self.fail()),
        }
    }

    fn fail(&mut self) -> TokenErrorKind {
        self.scan.end_of_token();
        TokenErrorKind::RationalInvalid
    }

    fn get_lexeme(&mut self) -> &str {
        self.scan.current_lexeme_at(self.start)
    }
}

struct RadixNumber<'me, 'str, R> {
    classifier: Integral<R>,
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

pub(in crate::lex::tokenize) trait Radix {
    const BASE: u32;
    const NAME: &'static str;

    fn is_digit(&self, ch: char) -> bool;
}

#[derive(Clone, Copy, Debug, Default)]
struct Decimal;

impl Radix for Decimal {
    const BASE: u32 = 10;
    const NAME: &'static str = "decimal";

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
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

#[derive(Clone, Copy, Debug)]
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
    ControlFlow<Result<BreakCondition<'str, Decimal>, TokenErrorKind>, Option<Classifier>>;
type RadixControl<'str, R> = ControlFlow<Result<BreakCondition<'str, R>, TokenErrorKind>>;
type ParseResult = Result<Real, TokenErrorKind>;

#[derive(Debug)]
enum BreakCondition<'str, R> {
    Complete,
    Complex {
        kind: ComplexKind,
        start: ScanItem<'str>,
    },
    Fraction(Magnitude<R>),
    Imaginary,
}

#[derive(Debug)]
enum Classifier {
    Flt(Float),
    Int(DecimalInt),
    Sci(Scientific),
}

impl Classifier {
    fn has_sign(&self) -> bool {
        match self {
            Self::Flt(f) => f.integral.sign,
            Self::Int(i) => i.0.sign,
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

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        match self {
            Self::Flt(f) => f.parse(input, exactness),
            Self::Int(i) => i.parse(input, exactness),
            Self::Sci(s) => s.parse(input, exactness),
        }
    }
}

#[derive(Debug)]
struct Integral<R>(Magnitude<R>);

impl<R: Radix + Debug> Integral<R> {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> RadixControl<'str, R> {
        let (idx, ch) = item;
        match ch {
            '+' | '-' => {
                if self.0.digits.is_empty() {
                    if self.0.sign.is_none() {
                        self.0.sign = Some(super::char_to_sign(ch));
                        self.0.digits = 1..1;
                        ControlFlow::Continue(())
                    } else {
                        ControlFlow::Break(Err(TokenErrorKind::NumberInvalid))
                    }
                } else {
                    ControlFlow::Break(Ok(BreakCondition::Complex {
                        kind: ComplexKind::Cartesian,
                        start: item,
                    }))
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
            _ if self.0.radix.is_digit(ch) => {
                self.0.digits.end += 1;
                ControlFlow::Continue(())
            }
            _ => ControlFlow::Break(Err(TokenErrorKind::NumberInvalid)),
            // TODO: /, @
        }
    }
}

#[derive(Debug)]
struct DecimalInt(Magnitude<Decimal>);

impl DecimalInt {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        let ch = item.1;
        let offset = self.0.digits.end;
        match ch {
            '+' | '-' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Cartesian,
                start: item,
            })),
            '.' => ControlFlow::Continue(Some(Classifier::Flt(Float {
                fraction: offset..offset + 1,
                integral: self.0.clone(),
            }))),
            '/' => ControlFlow::Break(Ok(BreakCondition::Fraction(self.0.clone()))),
            'e' | 'E' => ControlFlow::Continue(Some(Classifier::Sci(Scientific {
                exponent: offset..offset + 1,
                significand: Float {
                    integral: self.0.clone(),
                    fraction: offset..offset,
                },
                ..Default::default()
            }))),
            'i' | 'I' => ControlFlow::Break(Ok(BreakCondition::Imaginary)),
            _ if self.0.radix.is_digit(ch) => {
                self.0.digits.end += 1;
                ControlFlow::Continue(None)
            }
            _ => ControlFlow::Break(Err(TokenErrorKind::NumberInvalid)),
        }
        // TODO: @
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        match exactness {
            None | Some(Exactness::Exact) => Ok(self.0.exact_parse(input)?.into()),
            Some(Exactness::Inexact) => todo!(),
        }
    }
}

type ExactParseResult = Result<Integer, TokenErrorKind>;

// NOTE: all ranges are relative to parse string, not the token scanner;
// e.g. all magnitude digits start at 0 or 1 (if sign is explicit).
#[derive(Clone, Debug, Default)]
struct Magnitude<R> {
    digits: Range<usize>,
    radix: R,
    sign: Option<Sign>,
}

impl<R: Radix + Debug> Magnitude<R> {
    fn exact_parse(&self, input: &str) -> ExactParseResult {
        if !self.digits.is_empty() {
            // TODO: use magnitude start instead of assuming input starts at sign
            if let Some(sign_mag) = input.get(..self.digits.end) {
                return i64::from_str_radix(sign_mag, R::BASE).map_or_else(
                    |_| self.parse_sign_magnitude(sign_mag),
                    |val| Ok(val.into()),
                );
            }
        }
        Err(TokenErrorKind::NumberInvalid)
    }

    fn parse_sign_magnitude(&self, input: &str) -> ExactParseResult {
        if let Some(mag) = input.get(self.digits.start..) {
            u64::from_str_radix(mag, R::BASE).map_or_else(
                |_| self.parse_multi_precision(input),
                |val| {
                    let sign_mag = (self.sign.unwrap_or(Sign::Positive), val);
                    Ok(sign_mag.into())
                },
            )
        } else {
            Err(TokenErrorKind::NumberInvalid)
        }
    }

    fn parse_multi_precision(&self, input: &str) -> ExactParseResult {
        Err(TokenErrorKind::Unimplemented(input.to_owned()))
    }
}

#[derive(Clone, Debug, Default)]
struct Float {
    fraction: Range<usize>,
    integral: Magnitude<Decimal>,
}

impl Float {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        let (idx, ch) = item;
        let offset = self.fraction.end;
        match ch {
            '+' | '-' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Cartesian,
                start: item,
            })),
            '.' => ControlFlow::Break(Err(TokenErrorKind::NumberUnexpectedDecimalPoint {
                at: idx,
            })),
            '/' => ControlFlow::Break(Err(TokenErrorKind::RationalInvalid)),
            'e' | 'E' => ControlFlow::Continue(Some(Classifier::Sci(Scientific {
                exponent: offset..offset + 1,
                significand: self.clone(),
                ..Default::default()
            }))),
            'i' | 'I' => ControlFlow::Break(Ok(BreakCondition::Imaginary)),
            _ if self.integral.radix.is_digit(ch) => {
                self.fraction.end += 1;
                ControlFlow::Continue(None)
            }
            _ => ControlFlow::Break(Err(TokenErrorKind::NumberInvalid)),
        }
        // TODO: @
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        self.parse_to(input, self.fraction.end, exactness)
    }

    fn parse_to(&self, input: &str, end: usize, exactness: Option<Exactness>) -> ParseResult {
        if let Some(numstr) = input.get(..end) {
            match exactness {
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
                if self.exponent_sign.is_some() && self.exponent.len() == 2 {
                    ControlFlow::Break(Err(self.malformed_exponent()))
                } else if self.exponent.len() == 1 {
                    self.exponent_sign = Some(super::char_to_sign(ch));
                    self.exponent.end += 1;
                    ControlFlow::Continue(None)
                } else {
                    ControlFlow::Break(Ok(BreakCondition::Complex {
                        kind: ComplexKind::Cartesian,
                        start: item,
                    }))
                }
            }
            '/' => ControlFlow::Break(Err(TokenErrorKind::RationalInvalid)),
            'i' | 'I' => ControlFlow::Break(if self.no_e_value() {
                Err(self.malformed_exponent())
            } else {
                Ok(BreakCondition::Imaginary)
            }),
            _ if self.significand.integral.radix.is_digit(ch) => {
                self.exponent.end += 1;
                ControlFlow::Continue(None)
            }
            _ => ControlFlow::Break(Err(self.malformed_exponent())),
        }
        // TODO: @
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        if self.no_e_value() {
            return Err(self.malformed_exponent());
        }
        self.significand
            .parse_to(input, self.exponent.end, exactness)
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

fn real_to_token(r: impl Into<Real>, imaginary: bool) -> TokenKind {
    if imaginary {
        TokenKind::Imaginary(r.into())
    } else {
        TokenKind::Literal(Literal::Number(Number::real(r)))
    }
}
