use super::{ComplexKind, Exactness, Identifier};
use crate::{
    lex::{
        token::{TokenErrorKind, TokenKind},
        tokenize::{
            extract::TokenExtractResult,
            scan::{ScanItem, Scanner},
        },
    },
    literal::Literal,
    number::{Decimal, Integer, Number, Radix, Real, Sign},
};
use std::{
    fmt::Debug,
    num::{IntErrorKind, ParseIntError},
    ops::{ControlFlow, Range},
};

pub(super) struct DecimalNumber<'me, 'str> {
    classifier: DecimalClassifier,
    exactness: Option<Exactness>,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

// NOTE: these ctors are always called after one confirmed decimal digit has been scanned
impl<'me, 'str> DecimalNumber<'me, 'str> {
    pub(super) fn new(
        scan: &'me mut Scanner<'str>,
        start: usize,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: DecimalClassifier::Int(DecimalInt(Magnitude {
                digits: 0..1,
                ..Default::default()
            })),
            exactness,
            scan,
            start,
        }
    }

    pub(super) fn try_signed_number(
        sign: Sign,
        scan: &'me mut Scanner<'str>,
        start: usize,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: DecimalClassifier::Int(DecimalInt(Magnitude {
                digits: 1..2,
                sign: Some(sign),
                ..Default::default()
            })),
            exactness,
            scan,
            start,
        }
    }

    pub(super) fn try_float(
        scan: &'me mut Scanner<'str>,
        start: usize,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: DecimalClassifier::Flt(Float {
                fraction: 1..2,
                ..Default::default()
            }),
            exactness,
            scan,
            start,
        }
    }

    pub(super) fn try_signed_float(
        sign: Sign,
        scan: &'me mut Scanner<'str>,
        start: usize,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: DecimalClassifier::Flt(Float {
                fraction: 2..3,
                integral: Magnitude {
                    sign: Some(sign),
                    ..Default::default()
                },
            }),
            exactness,
            scan,
            start,
        }
    }

    pub(super) fn scan(&mut self) -> TokenExtractResult {
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
        ConditionHandler {
            classifier: &self.classifier,
            exactness: self.exactness,
            scan: self.scan,
            start: self.start,
        }
        .resolve(brk)
    }
}

pub(super) struct RadixNumber<'me, 'str, R> {
    classifier: Integral<R>,
    exactness: Option<Exactness>,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

impl<'me, 'str, R: Radix + Clone + Debug + Default> RadixNumber<'me, 'str, R> {
    pub(super) fn new(scan: &'me mut Scanner<'str>, exactness: Option<Exactness>) -> Self {
        let start = scan.pos();
        Self {
            classifier: Integral(Magnitude::default()),
            exactness,
            scan,
            start,
        }
    }

    fn with_sign(
        scan: &'me mut Scanner<'str>,
        start: ScanItem,
        exactness: Option<Exactness>,
    ) -> Self {
        let (start, sign) = start;
        Self {
            classifier: Integral(Magnitude {
                digits: 1..1,
                sign: Some(super::char_to_sign(sign)),
                ..Default::default()
            }),
            exactness,
            scan,
            start,
        }
    }

    pub(super) fn scan(&mut self) -> TokenExtractResult {
        let mut brk = Ok(BreakCondition::<R>::Complete);
        while let Some(item) = self.scan.next_if_not_delimiter() {
            match self.classifier.classify(item) {
                ControlFlow::Continue(()) => (),
                ControlFlow::Break(b) => {
                    brk = b;
                    break;
                }
            }
        }
        ConditionHandler {
            classifier: &self.classifier,
            exactness: self.exactness,
            scan: self.scan,
            start: self.start,
        }
        .resolve(brk)
    }
}

pub(super) fn imaginary(sign: Sign, exactness: Option<Exactness>) -> TokenKind {
    let sign_val = sign as i64;
    let r: Real = match exactness {
        None | Some(Exactness::Exact) => sign_val.into(),
        Some(Exactness::Inexact) => (sign_val as f64).into(),
    };
    real_to_token(r, true)
}

pub(super) fn infinity(sign: Sign, is_imaginary: bool) -> TokenKind {
    real_to_token(
        match sign {
            Sign::Negative => f64::NEG_INFINITY,
            Sign::Positive => f64::INFINITY,
            Sign::Zero => 0.0,
        },
        is_imaginary,
    )
}

pub(super) fn nan(is_imaginary: bool) -> TokenKind {
    real_to_token(f64::NAN, is_imaginary)
}

struct ConditionHandler<'me, 'str, C> {
    classifier: &'me C,
    exactness: Option<Exactness>,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

impl<'me, 'str, C: Classifier> ConditionHandler<'me, 'str, C> {
    fn resolve<R: Radix + Debug>(
        mut self,
        result: Result<BreakCondition<'str, R>, TokenErrorKind>,
    ) -> TokenExtractResult {
        match result {
            Ok(cond) => {
                match cond {
                    BreakCondition::Complete => self.complete(false),
                    BreakCondition::Complex { kind, start } => {
                        // NOTE: delay application of exactness until final
                        // composition of complex number; specifically Polar
                        // will round-trip inputs through float representation,
                        // undoing any exactness applied to real part.
                        match self.classifier.parse(self.get_lexeme(), None) {
                            Ok(real) => self.scan_imaginary(real, kind, start),
                            Err(err) => self.fail(err),
                        }
                    }
                    BreakCondition::Fraction(m) => match m.parse(self.get_lexeme()) {
                        Ok(numerator) => self.scan_denominator(numerator),
                        Err(err) => self.fail(err),
                    },
                    BreakCondition::Imaginary => {
                        if let Some(item) = self.scan.next_if_not_delimiter() {
                            // NOTE: maybe malformed "in"finity? otherwise assume malformed imaginary
                            self.fail(if item.1.is_ascii_alphabetic() {
                                TokenErrorKind::NumberInvalid
                            } else {
                                TokenErrorKind::ImaginaryInvalid
                            })
                        } else if !self.classifier.has_sign() {
                            self.fail(TokenErrorKind::ImaginaryMissingSign)
                        } else {
                            self.complete(true)
                        }
                    }
                }
            }
            Err(err) => self.fail(err),
        }
    }

    fn scan_denominator(&mut self, numerator: Integer) -> TokenExtractResult {
        let mut d = self.classifier.denominator_scanner(self.scan);
        let (denominator, cond) = d.scan()?;
        let mut real = Real::reduce(numerator, denominator)?;
        match cond {
            FractionBreak::Complete => {
                if matches!(self.exactness, Some(Exactness::Inexact)) {
                    real = real.into_inexact();
                }
                Ok(real_to_token(real, false))
            }
            FractionBreak::Complex { kind, start } => self.scan_imaginary(real, kind, start),
            FractionBreak::Imaginary => {
                if self.classifier.has_sign() {
                    if matches!(self.exactness, Some(Exactness::Inexact)) {
                        real = real.into_inexact();
                    }
                    Ok(real_to_token(real, true))
                } else {
                    Err(TokenErrorKind::ImaginaryMissingSign)
                }
            }
        }
    }

    fn scan_imaginary(
        &mut self,
        real: Real,
        kind: ComplexKind,
        start: ScanItem,
    ) -> TokenExtractResult {
        match kind {
            ComplexKind::Cartesian => {
                debug_assert!(matches!(start.1, '+' | '-'));
                if let Ok(TokenKind::Imaginary(imag)) =
                    self.classifier
                        .cartesian_scan(self.scan, start, self.exactness)
                {
                    let real = match self.exactness {
                        Some(Exactness::Exact) => real.into_exact(),
                        Some(Exactness::Inexact) => real.into_inexact(),
                        None => real,
                    };
                    Ok(TokenKind::Literal(Literal::Number(Number::complex(
                        real, imag,
                    ))))
                } else {
                    self.fail(TokenErrorKind::ComplexInvalid)
                }
            }
            ComplexKind::Polar => {
                debug_assert_eq!(start.1, '@');
                if let Ok(TokenKind::Literal(Literal::Number(Number::Real(rads)))) =
                    self.classifier.polar_scan(self.scan)
                {
                    let pol = Number::polar(real, rads);
                    Ok(TokenKind::Literal(Literal::Number(match self.exactness {
                        Some(Exactness::Exact) => pol.into_exact(),
                        Some(Exactness::Inexact) => pol.into_inexact(),
                        None => pol,
                    })))
                } else {
                    self.fail(TokenErrorKind::PolarInvalid)
                }
            }
        }
    }

    fn complete(&mut self, is_imaginary: bool) -> TokenExtractResult {
        if is_imaginary && self.classifier.is_empty() {
            if let Some(sign) = self.classifier.get_sign() {
                return Ok(imaginary(sign, self.exactness));
            }
        }
        self.classifier
            .parse(self.get_lexeme(), self.exactness)
            .map_or_else(|err| self.fail(err), |r| Ok(real_to_token(r, is_imaginary)))
    }

    fn fail(&mut self, err: TokenErrorKind) -> TokenExtractResult {
        self.scan.end_of_token();
        Err(err)
    }

    fn get_lexeme(&mut self) -> &'str str {
        self.scan.current_lexeme_at(self.start)
    }
}

struct Denominator<'me, 'str, R> {
    classifier: Integral<R>,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

impl<'me, 'str, R: Radix + Clone + Debug + Default> Denominator<'me, 'str, R> {
    fn new(scan: &'me mut Scanner<'str>) -> Self {
        let start = scan.pos();
        Self {
            classifier: Integral(Magnitude {
                sign: Some(Sign::Positive),
                ..Default::default()
            }),
            scan,
            start,
        }
    }

    fn scan(&mut self) -> Result<(Integer, FractionBreak), TokenErrorKind> {
        let mut brk = Ok(BreakCondition::<R>::Complete);
        while let Some(item) = self.scan.next_if_not_delimiter() {
            match self.classifier.classify(item) {
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
                    BreakCondition::Complete | BreakCondition::Complex { .. } => {
                        let input = self.get_lexeme();
                        self.classifier.0.parse(input).map_err(|_| self.fail())
                    }
                    BreakCondition::Fraction(_) => Err(self.fail()),
                    BreakCondition::Imaginary => {
                        if self.scan.next_if_not_delimiter().is_some() {
                            Err(self.fail())
                        } else {
                            let input = self.get_lexeme();
                            self.classifier.0.parse(input).map_err(|_| self.fail())
                        }
                    }
                }?,
                cond.into(),
            )),
            _ => Err(self.fail()),
        }
    }

    fn fail(&mut self) -> TokenErrorKind {
        self.scan.end_of_token();
        TokenErrorKind::RationalInvalid
    }

    fn get_lexeme(&mut self) -> &'str str {
        self.scan.current_lexeme_at(self.start)
    }
}

type DecimalControl<'str> =
    ControlFlow<Result<BreakCondition<'str, Decimal>, TokenErrorKind>, Option<DecimalClassifier>>;
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
enum FractionBreak<'str> {
    Complete,
    Complex {
        kind: ComplexKind,
        start: ScanItem<'str>,
    },
    Imaginary,
}

impl<'str, R> From<BreakCondition<'str, R>> for FractionBreak<'str> {
    fn from(value: BreakCondition<'str, R>) -> Self {
        match value {
            BreakCondition::Complete => Self::Complete,
            BreakCondition::Complex { kind, start } => Self::Complex { kind, start },
            BreakCondition::Imaginary => Self::Imaginary,
            // NOTE: Denominator ensures this case never happens
            BreakCondition::Fraction(_) => unreachable!(),
        }
    }
}

trait Classifier {
    type Radix: Radix + Clone + Debug + Default;

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult;
    fn get_sign(&self) -> Option<Sign>;
    fn is_empty(&self) -> bool;
    fn cartesian_scan(
        &self,
        scan: &mut Scanner,
        start: ScanItem,
        exactness: Option<Exactness>,
    ) -> TokenExtractResult;
    fn polar_scan(&self, scan: &mut Scanner) -> TokenExtractResult;

    fn has_sign(&self) -> bool {
        self.get_sign().is_some()
    }

    fn denominator_scanner<'me, 'str>(
        &self,
        scan: &'me mut Scanner<'str>,
    ) -> Denominator<'me, 'str, Self::Radix> {
        Denominator::<Self::Radix>::new(scan)
    }
}

#[derive(Debug)]
enum DecimalClassifier {
    Flt(Float),
    Int(DecimalInt),
    Sci(Scientific),
}

impl DecimalClassifier {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        match self {
            Self::Flt(f) => f.classify(item),
            Self::Int(i) => i.classify(item),
            Self::Sci(s) => s.classify(item),
        }
    }
}

impl Classifier for DecimalClassifier {
    type Radix = Decimal;

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        match self {
            Self::Flt(f) => f.parse(input, exactness),
            Self::Int(i) => i.parse(input, exactness),
            Self::Sci(s) => s.parse(input, exactness),
        }
    }

    fn get_sign(&self) -> Option<Sign> {
        match self {
            Self::Flt(f) => f.integral.sign,
            Self::Int(i) => i.0.sign,
            Self::Sci(s) => s.significand.integral.sign,
        }
    }

    // NOTE: decimal classifier always classifies at least one digit
    fn is_empty(&self) -> bool {
        // NOTE: this should never happen due to the above invariant
        debug_assert!(match self {
            Self::Flt(f) => !(f.integral.digits.is_empty() && f.fraction.is_empty()),
            Self::Int(i) => !i.0.digits.is_empty(),
            Self::Sci(s) =>
                !(s.significand.integral.digits.is_empty()
                    && s.significand.fraction.is_empty()
                    && s.exponent.is_empty()),
        });
        false
    }

    fn cartesian_scan(
        &self,
        scan: &mut Scanner,
        start: ScanItem,
        exactness: Option<Exactness>,
    ) -> TokenExtractResult {
        Identifier::with_exactness(scan, start, exactness).scan()
    }

    fn polar_scan(&self, scan: &mut Scanner) -> TokenExtractResult {
        scan.next_if_not_delimiter()
            .map_or(Err(TokenErrorKind::PolarInvalid), |start| {
                Identifier::new(scan, start).scan()
            })
    }
}

#[derive(Debug)]
struct Integral<R>(Magnitude<R>);

impl<R: Radix + Clone + Debug + Default> Integral<R> {
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
            '/' => ControlFlow::Break(Ok(BreakCondition::Fraction(self.0.clone()))),
            '@' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Polar,
                start: item,
            })),
            'i' | 'I' => ControlFlow::Break(Ok(BreakCondition::Imaginary)),
            _ if self.0.radix.is_digit(ch) => {
                self.0.digits.end += 1;
                ControlFlow::Continue(())
            }
            // NOTE: e|E hexadecimal is_digit is true, so check exponent after digit
            'e' | 'E' => ControlFlow::Break(Err(TokenErrorKind::NumberInvalidExponent {
                at: idx,
                radix: R::NAME,
            })),
            _ => ControlFlow::Break(Err(if !self.has_sign() && self.is_empty() {
                TokenErrorKind::NumberExpected
            } else {
                TokenErrorKind::NumberInvalid
            })),
        }
    }
}

impl<R: Radix + Clone + Debug + Default> Classifier for Integral<R> {
    type Radix = R;

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        // NOTE: always parse exact magnitude first to account for radix
        let n = self.0.parse(input)?;
        match exactness {
            None | Some(Exactness::Exact) => Ok(n.into()),
            Some(Exactness::Inexact) => Ok(n.into_inexact()),
        }
    }

    fn get_sign(&self) -> Option<Sign> {
        self.0.sign
    }

    fn is_empty(&self) -> bool {
        self.0.digits.is_empty()
    }

    fn cartesian_scan(
        &self,
        scan: &mut Scanner,
        start: ScanItem,
        exactness: Option<Exactness>,
    ) -> TokenExtractResult {
        RadixNumber::<R>::with_sign(scan, start, exactness).scan()
    }

    fn polar_scan(&self, scan: &mut Scanner) -> TokenExtractResult {
        RadixNumber::<R>::new(scan, Some(Exactness::Exact)).scan()
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
    fn parse(&self, input: &str) -> ExactParseResult {
        if self.digits.is_empty() {
            Err(TokenErrorKind::NumberExpected)
        } else {
            input
                .get(..self.digits.end)
                .map_or(Err(TokenErrorKind::NumberInvalid), |signed_num| {
                    i64::from_str_radix(signed_num, R::BASE).map_or_else(
                        |_| self.parse_sign_magnitude(signed_num),
                        |val| Ok(val.into()),
                    )
                })
        }
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

#[derive(Debug)]
struct DecimalInt(Magnitude<Decimal>);

impl DecimalInt {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        let (idx, ch) = item;
        let offset = self.0.digits.end;
        match ch {
            '+' | '-' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Cartesian,
                start: item,
            })),
            '.' => ControlFlow::Continue(Some(DecimalClassifier::Flt(Float {
                fraction: offset + 1..offset + 1,
                integral: self.0.clone(),
            }))),
            '/' => ControlFlow::Break(Ok(BreakCondition::Fraction(self.0.clone()))),
            '@' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Polar,
                start: item,
            })),
            'e' | 'E' => ControlFlow::Continue(Some(DecimalClassifier::Sci(Scientific {
                e_at: idx,
                exponent: offset + 1..offset + 1,
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
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        match exactness {
            None | Some(Exactness::Exact) => Ok(self.0.parse(input)?.into()),
            Some(Exactness::Inexact) => parse_inexact_to(self.0.digits.end, input),
        }
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
            '@' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Polar,
                start: item,
            })),
            'e' | 'E' => ControlFlow::Continue(Some(DecimalClassifier::Sci(Scientific {
                e_at: idx,
                exponent: offset + 1..offset + 1,
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
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        match exactness {
            None | Some(Exactness::Inexact) => parse_inexact_to(self.fraction.end, input),
            Some(Exactness::Exact) => self.parse_exact(input, 0),
        }
    }

    fn parse_exact(&self, input: &str, exponent: i32) -> ParseResult {
        let mut buf = String::new();
        let mut num = Magnitude::<Decimal>::default();
        if self.integral.sign.is_some() {
            buf += input.get(0..1).unwrap_or_default();
            num.sign = self.integral.sign;
            num.digits = 1..1;
        }
        buf += input.get(self.integral.digits.clone()).unwrap_or_default();
        let frac = input.get(self.fraction.clone()).unwrap_or_default();
        buf += frac;
        let scale = exponent - i32::try_from(frac.len()).unwrap_or_default();
        let adjustment = std::iter::repeat('0')
            .take(scale.abs().try_into().unwrap_or_default())
            .collect::<String>();
        Ok(if scale < 0 {
            num.digits.end = buf.len();
            let adjustment = format!("1{adjustment}",);
            let denom = Magnitude::<Decimal> {
                digits: 0..adjustment.len(),
                ..Default::default()
            };
            Real::reduce(num.parse(&buf)?, denom.parse(&adjustment)?)?
        } else {
            buf += &adjustment;
            num.digits.end = buf.len();
            num.parse(&buf)?.into()
        })
    }
}

#[derive(Debug, Default)]
struct Scientific {
    e_at: usize,
    exponent: Range<usize>,
    exponent_sign: Option<Sign>,
    significand: Float,
}

impl Scientific {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        let ch = item.1;
        match ch {
            '+' | '-' => {
                if self.exponent_sign.is_some() && self.exponent.len() == 1 {
                    ControlFlow::Break(Err(self.malformed_exponent()))
                } else if self.exponent.is_empty() {
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
            '@' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Polar,
                start: item,
            })),
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
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        if self.no_e_value() {
            Err(self.malformed_exponent())
        } else {
            match exactness {
                None | Some(Exactness::Inexact) => parse_inexact_to(self.exponent.end, input),
                Some(Exactness::Exact) => self.parse_exact(input),
            }
        }
    }

    fn parse_exact(&self, input: &str) -> ParseResult {
        let exponent: i32 = input
            .get(self.exponent.clone())
            .unwrap_or_default()
            .parse()
            .map_err(|err: ParseIntError| match err.kind() {
                IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                    TokenErrorKind::ExponentOutOfRange { at: self.e_at }
                }
                _ => self.malformed_exponent(),
            })?;
        self.significand.parse_exact(input, exponent)
    }

    fn no_e_value(&self) -> bool {
        self.exponent.is_empty() || (self.exponent.len() == 1 && self.exponent_sign.is_some())
    }

    fn malformed_exponent(&self) -> TokenErrorKind {
        TokenErrorKind::NumberMalformedExponent { at: self.e_at }
    }
}

fn parse_inexact_to(end: usize, input: &str) -> ParseResult {
    input
        .get(..end)
        .map_or(Err(TokenErrorKind::NumberInvalid), |fstr| {
            Ok(fstr.parse::<f64>()?.into())
        })
}

fn real_to_token(r: impl Into<Real>, is_imaginary: bool) -> TokenKind {
    if is_imaginary {
        TokenKind::Imaginary(r.into())
    } else {
        TokenKind::Literal(Literal::Number(Number::real(r)))
    }
}
