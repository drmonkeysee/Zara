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
    number::{Integer, Number, Real, Sign},
};
use std::{
    fmt::Debug,
    ops::{ControlFlow, Range},
};

pub(super) struct DecimalNumber<'me, 'str> {
    classifier: Classifier,
    exactness: Option<Exactness>,
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

// NOTE: these ctors are always called after one confirmed decimal digit has been scanned
impl<'me, 'str> DecimalNumber<'me, 'str> {
    pub(super) fn new(
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: Classifier::Int(DecimalInt(Magnitude {
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
        start: ScanItem<'str>,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: Classifier::Int(DecimalInt(Magnitude {
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
        start: ScanItem<'str>,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: Classifier::Flt(Float {
                fraction: 0..2,
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
        start: ScanItem<'str>,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: Classifier::Flt(Float {
                fraction: 1..3,
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
        match brk {
            Ok(cond) => {
                match cond {
                    BreakCondition::Complete => self.complete(false),
                    BreakCondition::Complex { kind, start } => match self.parse() {
                        Ok(real) => self.scan_imaginary(real, kind, start),
                        Err(err) => self.fail(err),
                    },
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
        let mut d = Denominator::<Decimal>::new(self.scan);
        let (denominator, cond) = d.scan()?;
        let mut real = Real::reduce(numerator, denominator)?;
        if let Some(Exactness::Inexact) = self.exactness {
            real = real.into_inexact();
        }
        match cond {
            FractionBreak::Complete => Ok(real_to_token(real, false)),
            FractionBreak::Complex { kind, start } => self.scan_imaginary(real, kind, start),
            FractionBreak::Imaginary => {
                if self.classifier.has_sign() {
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
                    Identifier::with_exactness(self.scan, start, self.exactness).scan()
                {
                    Ok(TokenKind::Literal(Literal::Number(Number::complex(
                        real, imag,
                    ))))
                } else {
                    self.fail(TokenErrorKind::ComplexInvalid)
                }
            }
            ComplexKind::Polar => {
                debug_assert_eq!(start.1, '@');
                if let Some(first) = self.scan.next_if_not_delimiter() {
                    // NOTE: polar literals must roundtrip through float representation
                    // by definition so exactness does not apply during parsing.
                    // TODO: handle exact final representation
                    if let Ok(TokenKind::Literal(Literal::Number(Number::Real(rads)))) =
                        Identifier::new(self.scan, first).scan()
                    {
                        return Ok(TokenKind::Literal(Literal::Number(Number::polar(
                            real, rads,
                        ))));
                    }
                }
                self.fail(TokenErrorKind::PolarInvalid)
            }
        }
    }

    fn complete(&mut self, imaginary: bool) -> TokenExtractResult {
        self.parse()
            .map_or_else(|err| self.fail(err), |r| Ok(real_to_token(r, imaginary)))
    }

    fn fail(&mut self, err: TokenErrorKind) -> TokenExtractResult {
        self.scan.end_of_token();
        Err(err)
    }

    fn parse(&mut self) -> ParseResult {
        let exactness = self.exactness;
        let input = self.get_lexeme();
        self.classifier.parse(input, exactness)
    }

    fn get_lexeme(&mut self) -> &'str str {
        self.scan.current_lexeme_at(self.start.0)
    }
}

pub(super) struct RadixNumber<'me, 'str, R> {
    classifier: Integral<R>,
    exactness: Exactness,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

impl<'me, 'str, R: Radix + Clone + Debug + Default> RadixNumber<'me, 'str, R> {
    pub(super) fn new(scan: &'me mut Scanner<'str>, exactness: Exactness) -> Self {
        let start = scan.pos();
        Self {
            classifier: Integral(Magnitude::default()),
            exactness,
            scan,
            start,
        }
    }

    fn with_sign(scan: &'me mut Scanner<'str>, start: ScanItem, exactness: Exactness) -> Self {
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
        match brk {
            Ok(cond) => {
                match cond {
                    BreakCondition::Complete => self.complete(false),
                    BreakCondition::Complex { kind, start } => match self.parse() {
                        Ok(real) => self.scan_imaginary(real, kind, start),
                        Err(err) => self.fail(err),
                    },
                    BreakCondition::Fraction(m) => {
                        // TODO: handle inexact e.g #i4/3 => 1.3333...
                        match m.parse(self.get_lexeme()) {
                            Ok(numerator) => self.scan_denominator(numerator),
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
        let mut d = Denominator::<R>::new(self.scan);
        let (denominator, cond) = d.scan()?;
        let mut real = Real::reduce(numerator, denominator)?;
        if self.exactness == Exactness::Inexact {
            real = real.into_inexact();
        }
        match cond {
            FractionBreak::Complete => Ok(real_to_token(real, false)),
            FractionBreak::Complex { kind, start } => self.scan_imaginary(real, kind, start),
            FractionBreak::Imaginary => {
                if self.classifier.has_sign() {
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
                    RadixNumber::<R>::with_sign(self.scan, start, self.exactness).scan()
                {
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
                    RadixNumber::<R>::new(self.scan, self.exactness).scan()
                {
                    return Ok(TokenKind::Literal(Literal::Number(Number::polar(
                        real, rads,
                    ))));
                }
                self.fail(TokenErrorKind::PolarInvalid)
            }
        }
    }

    fn complete(&mut self, is_imaginary: bool) -> TokenExtractResult {
        if is_imaginary && self.classifier.is_empty() {
            if let Some(sign) = self.classifier.get_sign() {
                return Ok(imaginary(sign, Some(self.exactness)));
            }
        }
        self.parse()
            .map_or_else(|err| self.fail(err), |r| Ok(real_to_token(r, is_imaginary)))
    }

    fn fail(&mut self, err: TokenErrorKind) -> TokenExtractResult {
        self.scan.end_of_token();
        Err(err)
    }

    fn parse(&mut self) -> ParseResult {
        let exactness = self.exactness;
        let input = self.get_lexeme();
        self.classifier.parse(input, exactness)
    }

    fn get_lexeme(&mut self) -> &'str str {
        self.scan.current_lexeme_at(self.start)
    }
}

pub(super) trait Radix {
    const BASE: u32;
    const NAME: &'static str;

    fn is_digit(&self, ch: char) -> bool;
}

#[derive(Clone, Copy, Debug, Default)]
pub(super) struct Binary;

impl Radix for Binary {
    const BASE: u32 = 2;
    const NAME: &'static str = "binary";

    fn is_digit(&self, ch: char) -> bool {
        matches!(ch, '0'..='1')
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub(super) struct Octal;

impl Radix for Octal {
    const BASE: u32 = 8;
    const NAME: &'static str = "octal";

    fn is_digit(&self, ch: char) -> bool {
        // TODO: nightly-only experimental API.
        // (is_ascii_octdigit https://github.com/rust-lang/rust/issues/101288)
        matches!(ch, '0'..='7')
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub(super) struct Decimal;

impl Radix for Decimal {
    const BASE: u32 = 10;
    const NAME: &'static str = "decimal";

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub(super) struct Hexadecimal;

impl Radix for Hexadecimal {
    const BASE: u32 = 16;
    const NAME: &'static str = "hexadecimal";

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_hexdigit()
    }
}

// TODO: handle inexact
pub(super) fn imaginary(sign: Sign, exactness: Option<Exactness>) -> TokenKind {
    let sign_val = sign as i64;
    let r: Real = match exactness {
        None | Some(Exactness::Exact) => sign_val.into(),
        Some(Exactness::Inexact) => (sign_val as f64).into(),
    };
    real_to_token(r, true)
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
struct Integral<R>(Magnitude<R>);

impl<R: Radix + Clone + Debug> Integral<R> {
    fn has_sign(&self) -> bool {
        self.0.sign.is_some()
    }

    fn get_sign(&self) -> Option<Sign> {
        self.0.sign
    }

    fn is_empty(&self) -> bool {
        self.0.digits.is_empty()
    }

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

    fn parse(&self, input: &str, exactness: Exactness) -> ParseResult {
        // NOTE: always parse exact magnitude first to account for radix
        let n = self.0.parse(input)?;
        match exactness {
            Exactness::Exact => Ok(n.into()),
            Exactness::Inexact => Ok(n.into_inexact()),
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
            '@' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Polar,
                start: item,
            })),
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
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        match exactness {
            None | Some(Exactness::Inexact) => parse_inexact_to(self.fraction.end, input),
            Some(Exactness::Exact) => Err(TokenErrorKind::Unimplemented(input.to_owned())),
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
                Some(Exactness::Exact) => Err(TokenErrorKind::Unimplemented(input.to_owned())),
            }
        }
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

fn parse_inexact_to(end: usize, input: &str) -> ParseResult {
    input
        .get(..end)
        .map_or(Err(TokenErrorKind::NumberInvalid), |fstr| {
            Ok(fstr.parse::<f64>()?.into())
        })
}

fn real_to_token(r: impl Into<Real>, imaginary: bool) -> TokenKind {
    if imaginary {
        TokenKind::Imaginary(r.into())
    } else {
        TokenKind::Literal(Literal::Number(Number::real(r)))
    }
}
