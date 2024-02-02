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
    number::{Decimal, FloatSpec, IntSpec, Integer, Number, NumericError, Radix, Real, Sign},
};
use std::{fmt::Debug, marker::PhantomData, ops::ControlFlow};

pub(super) struct RealNumber<'me, 'str> {
    classifier: RealClassifier,
    exactness: Option<Exactness>,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

// NOTE: this lexer is only invoked after one confirmed digit has been scanned
impl<'me, 'str> RealNumber<'me, 'str> {
    pub(super) fn new(
        scan: &'me mut Scanner<'str>,
        start: usize,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: RealClassifier::Int(DecimalInt(IntSpec {
                magnitude: 0..1,
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
            classifier: RealClassifier::Int(DecimalInt(IntSpec {
                magnitude: 1..2,
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
            classifier: RealClassifier::Flt(Float(FloatSpec {
                fraction: 1..2,
                ..Default::default()
            })),
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
            classifier: RealClassifier::Flt(Float(FloatSpec {
                fraction: 2..3,
                integral: IntSpec {
                    sign: Some(sign),
                    ..Default::default()
                },
                ..Default::default()
            })),
            exactness,
            scan,
            start,
        }
    }

    pub(super) fn scan(mut self) -> TokenExtractResult {
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
        let (props, parser) = self
            .classifier
            .commit(self.scan.current_lexeme_at(self.start), self.exactness);
        ConditionProcessor {
            props,
            scan: self.scan,
        }
        .resolve(parser, brk)
    }
}

pub(super) struct RadixNumber<'me, 'str, R> {
    classifier: Integral<R>,
    exactness: Option<Exactness>,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

impl<'me, 'str, R: Radix + Default> RadixNumber<'me, 'str, R> {
    pub(super) fn new(scan: &'me mut Scanner<'str>, exactness: Option<Exactness>) -> Self {
        let start = scan.pos();
        Self {
            classifier: Integral(IntSpec::default()),
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
            classifier: Integral(IntSpec {
                magnitude: 1..1,
                sign: Some(super::char_to_sign(sign)),
                ..Default::default()
            }),
            exactness,
            scan,
            start,
        }
    }

    pub(super) fn scan(mut self) -> TokenExtractResult {
        let mut brk = Ok(BreakCondition::Complete);
        while let Some(item) = self.scan.next_if_not_delimiter() {
            match self.classifier.classify(item) {
                ControlFlow::Continue(()) => (),
                ControlFlow::Break(b) => {
                    brk = b;
                    break;
                }
            }
        }
        let (props, parser) = self
            .classifier
            .commit(self.scan.current_lexeme_at(self.start), self.exactness);
        ConditionProcessor {
            props,
            scan: self.scan,
        }
        .resolve(parser, brk)
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

struct ConditionProcessor<'me, 'str, P> {
    props: P,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str, P: ClassifierProps> ConditionProcessor<'me, 'str, P> {
    fn resolve<N: ClassifierParser>(
        mut self,
        parser: N,
        result: BreakResult<'str>,
    ) -> TokenExtractResult {
        match result {
            Ok(cond) => {
                match cond {
                    BreakCondition::Complete => self.complete(parser, false),
                    BreakCondition::Complex { kind, start } => {
                        // NOTE: delay application of exactness until final
                        // composition of complex number; specifically Polar
                        // will round-trip inputs through float representation,
                        // undoing any exactness applied to real part.
                        match parser.parse(None) {
                            Ok(real) => self.scan_imaginary(real, kind, start),
                            Err(err) => self.fail(err),
                        }
                    }
                    BreakCondition::Fraction => match parser.parse_int() {
                        Ok(numerator) => self.scan_denominator(numerator),
                        Err(err) => self.fail(err.into()),
                    },
                    BreakCondition::Imaginary => {
                        if let Some(item) = self.scan.next_if_not_delimiter() {
                            // NOTE: maybe malformed "in"finity? otherwise assume malformed imaginary
                            self.fail(if item.1.is_ascii_alphabetic() {
                                TokenErrorKind::NumberInvalid
                            } else {
                                TokenErrorKind::ImaginaryInvalid
                            })
                        } else if !self.props.has_sign() {
                            self.fail(TokenErrorKind::ImaginaryMissingSign)
                        } else {
                            self.complete(parser, true)
                        }
                    }
                }
            }
            Err(err) => self.fail(err),
        }
    }

    fn scan_denominator(&mut self, numerator: Integer) -> TokenExtractResult {
        let (denominator, cond) = Denominator::new(self.scan).scan::<P::Radix>()?;
        let mut real = Real::reduce(numerator, denominator)?;
        match cond {
            FractionBreak::Complete => {
                if matches!(self.props.get_exactness(), Some(Exactness::Inexact)) {
                    real = real.into_inexact();
                }
                Ok(real_to_token(real, false))
            }
            FractionBreak::Complex { kind, start } => self.scan_imaginary(real, kind, start),
            FractionBreak::Imaginary => {
                if self.props.has_sign() {
                    if matches!(self.props.get_exactness(), Some(Exactness::Inexact)) {
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
                if let Ok(TokenKind::Imaginary(imag)) = self.props.cartesian_scan(self.scan, start)
                {
                    let real = match self.props.get_exactness() {
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
                    self.props.polar_scan(self.scan)
                {
                    let pol = Number::polar(real, rads);
                    Ok(TokenKind::Literal(Literal::Number(
                        match self.props.get_exactness() {
                            Some(Exactness::Exact) => pol.into_exact(),
                            Some(Exactness::Inexact) => pol.into_inexact(),
                            None => pol,
                        },
                    )))
                } else {
                    self.fail(TokenErrorKind::PolarInvalid)
                }
            }
        }
    }

    fn complete<N: ClassifierParser>(
        &mut self,
        parser: N,
        is_imaginary: bool,
    ) -> TokenExtractResult {
        if is_imaginary && self.props.is_empty() {
            if let Some(sign) = self.props.get_sign() {
                return Ok(imaginary(sign, self.props.get_exactness()));
            }
        }
        parser
            .parse(self.props.get_exactness())
            .map_or_else(|err| self.fail(err), |r| Ok(real_to_token(r, is_imaginary)))
    }

    fn fail(&mut self, err: TokenErrorKind) -> TokenExtractResult {
        self.scan.end_of_token();
        Err(err)
    }
}

struct Denominator<'me, 'str> {
    scan: &'me mut Scanner<'str>,
    start: usize,
}

impl<'me, 'str> Denominator<'me, 'str> {
    fn new(scan: &'me mut Scanner<'str>) -> Self {
        let start = scan.pos();
        Self { scan, start }
    }

    fn scan<R: Radix + Default>(
        mut self,
    ) -> Result<(Integer, FractionBreak<'str>), TokenErrorKind> {
        let mut brk = Ok(BreakCondition::Complete);
        let mut classifier = Integral::<R>(IntSpec {
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
        let (_, parser) = classifier.commit(self.get_lexeme(), None);
        match brk {
            Ok(cond) => Ok((
                match cond {
                    BreakCondition::Complete | BreakCondition::Complex { .. } => {
                        parser.parse_int().map_err(|_| self.fail())
                    }
                    BreakCondition::Fraction => Err(self.fail()),
                    BreakCondition::Imaginary => {
                        if self.scan.next_if_not_delimiter().is_some() {
                            Err(self.fail())
                        } else {
                            parser.parse_int().map_err(|_| self.fail())
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

type BreakResult<'str> = Result<BreakCondition<'str>, TokenErrorKind>;
type RealControl<'str> = ControlFlow<BreakResult<'str>, Option<RealClassifier>>;
type RadixControl<'str> = ControlFlow<BreakResult<'str>>;
type ParseResult = Result<Real, TokenErrorKind>;
type IntParseResult = Result<Integer, TokenErrorKind>;

enum BreakCondition<'str> {
    Complete,
    Complex {
        kind: ComplexKind,
        start: ScanItem<'str>,
    },
    Fraction,
    Imaginary,
}

enum FractionBreak<'str> {
    Complete,
    Complex {
        kind: ComplexKind,
        start: ScanItem<'str>,
    },
    Imaginary,
}

impl<'str> From<BreakCondition<'str>> for FractionBreak<'str> {
    fn from(value: BreakCondition<'str>) -> Self {
        match value {
            BreakCondition::Complete => Self::Complete,
            BreakCondition::Complex { kind, start } => Self::Complex { kind, start },
            BreakCondition::Imaginary => Self::Imaginary,
            // NOTE: Denominator ensures this case never happens
            BreakCondition::Fraction => unreachable!(),
        }
    }
}

trait ClassifierProps {
    type Radix: Radix + Default;

    fn get_sign(&self) -> Option<Sign>;
    fn get_exactness(&self) -> Option<Exactness>;
    fn is_empty(&self) -> bool;
    fn cartesian_scan(&self, scan: &mut Scanner, start: ScanItem) -> TokenExtractResult;
    fn polar_scan(&self, scan: &mut Scanner) -> TokenExtractResult;

    fn has_sign(&self) -> bool {
        self.get_sign().is_some()
    }
}

trait ClassifierParser {
    fn parse(self, exactness: Option<Exactness>) -> ParseResult;
    fn parse_int(self) -> IntParseResult;
}

enum RealClassifier {
    Flt(Float),
    Int(DecimalInt),
    Sci(Scientific),
}

impl RealClassifier {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> RealControl<'str> {
        match self {
            Self::Flt(f) => f.classify(item),
            Self::Int(i) => i.classify(item),
            Self::Sci(s) => s.classify(item),
        }
    }

    fn commit(self, input: &str, exactness: Option<Exactness>) -> (RealProps, RealParser) {
        (
            RealProps {
                empty: self.is_empty(),
                exactness,
                sign: self.get_sign(),
            },
            RealParser { input, spec: self },
        )
    }

    fn into_int_spec(self) -> IntSpec<Decimal> {
        match self {
            Self::Flt(Float(f)) => f.integral,
            Self::Int(DecimalInt(i)) => i,
            Self::Sci(Scientific { spec, .. }) => spec.integral,
        }
    }

    fn get_sign(&self) -> Option<Sign> {
        match self {
            Self::Flt(Float(f)) => f.integral.sign,
            Self::Int(DecimalInt(i)) => i.sign,
            Self::Sci(Scientific { spec, .. }) => spec.integral.sign,
        }
    }

    // NOTE: real classifier always classifies at least one digit
    fn is_empty(&self) -> bool {
        debug_assert!(match self {
            Self::Flt(Float(f)) => !f.is_empty(),
            Self::Int(DecimalInt(i)) => !i.is_empty(),
            Self::Sci(Scientific { spec, .. }) => !spec.is_empty(),
        });
        false
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        match self {
            Self::Flt(f) => f.parse(input, exactness),
            Self::Int(i) => i.parse(input, exactness),
            Self::Sci(s) => s.parse(input, exactness),
        }
    }
}

struct RealProps {
    empty: bool,
    exactness: Option<Exactness>,
    sign: Option<Sign>,
}

impl ClassifierProps for RealProps {
    type Radix = Decimal;

    fn get_sign(&self) -> Option<Sign> {
        self.sign
    }

    fn get_exactness(&self) -> Option<Exactness> {
        self.exactness
    }

    fn is_empty(&self) -> bool {
        self.empty
    }

    fn cartesian_scan(&self, scan: &mut Scanner, start: ScanItem) -> TokenExtractResult {
        Identifier::with_exactness(scan, start, self.get_exactness()).scan()
    }

    fn polar_scan(&self, scan: &mut Scanner) -> TokenExtractResult {
        scan.next_if_not_delimiter()
            .map_or(Err(TokenErrorKind::PolarInvalid), |start| {
                Identifier::new(scan, start).scan()
            })
    }
}

struct RealParser<'str> {
    input: &'str str,
    spec: RealClassifier,
}

impl<'str> ClassifierParser for RealParser<'str> {
    fn parse(self, exactness: Option<Exactness>) -> ParseResult {
        self.spec.parse(self.input, exactness)
    }

    fn parse_int(self) -> IntParseResult {
        Ok(self.spec.into_int_spec().into_exact(self.input)?)
    }
}

struct Integral<R>(IntSpec<R>);

impl<R: Radix> Integral<R> {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> RadixControl<'str> {
        let (idx, ch) = item;
        match ch {
            '+' | '-' => {
                if self.0.is_empty() {
                    if self.0.sign.is_none() {
                        self.0.sign = Some(super::char_to_sign(ch));
                        self.0.magnitude = 1..1;
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
            '/' => ControlFlow::Break(Ok(BreakCondition::Fraction)),
            '@' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Polar,
                start: item,
            })),
            'i' | 'I' => ControlFlow::Break(Ok(BreakCondition::Imaginary)),
            _ if self.0.radix.is_digit(ch) => {
                self.0.magnitude.end += 1;
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

    fn commit(self, input: &str, exactness: Option<Exactness>) -> (RadixProps<R>, RadixParser<R>) {
        (
            RadixProps {
                empty: self.0.is_empty(),
                exactness,
                sign: self.0.sign,
                radix: PhantomData::<R>,
            },
            RadixParser::<R> {
                input,
                spec: self.0,
            },
        )
    }

    fn has_sign(&self) -> bool {
        self.0.sign.is_some()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

struct RadixProps<R> {
    empty: bool,
    exactness: Option<Exactness>,
    sign: Option<Sign>,
    radix: PhantomData<R>,
}

impl<R: Radix + Default> ClassifierProps for RadixProps<R> {
    type Radix = R;

    fn get_sign(&self) -> Option<Sign> {
        self.sign
    }

    fn get_exactness(&self) -> Option<Exactness> {
        self.exactness
    }

    fn is_empty(&self) -> bool {
        self.empty
    }

    fn cartesian_scan(&self, scan: &mut Scanner, start: ScanItem) -> TokenExtractResult {
        RadixNumber::<R>::with_sign(scan, start, self.get_exactness()).scan()
    }

    fn polar_scan(&self, scan: &mut Scanner) -> TokenExtractResult {
        RadixNumber::<R>::new(scan, Some(Exactness::Exact)).scan()
    }
}

struct RadixParser<'str, R> {
    input: &'str str,
    spec: IntSpec<R>,
}

impl<'str, R: Radix> ClassifierParser for RadixParser<'str, R> {
    fn parse(self, exactness: Option<Exactness>) -> ParseResult {
        if self.spec.is_empty() {
            Err(TokenErrorKind::NumberExpected)
        } else {
            Ok(match exactness {
                None | Some(Exactness::Exact) => self.parse_int()?.into(),
                Some(Exactness::Inexact) => self.spec.into_inexact(self.input)?,
            })
        }
    }

    fn parse_int(self) -> IntParseResult {
        Ok(self.spec.into_exact(self.input)?)
    }
}

struct DecimalInt(IntSpec<Decimal>);

impl DecimalInt {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> RealControl<'str> {
        let (idx, ch) = item;
        let offset = self.0.magnitude.end;
        match ch {
            '+' | '-' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Cartesian,
                start: item,
            })),
            '.' => ControlFlow::Continue(Some(RealClassifier::Flt(Float(FloatSpec {
                fraction: offset + 1..offset + 1,
                integral: self.0.clone(),
                ..Default::default()
            })))),
            '/' => ControlFlow::Break(Ok(BreakCondition::Fraction)),
            '@' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Polar,
                start: item,
            })),
            'e' | 'E' => ControlFlow::Continue(Some(RealClassifier::Sci(Scientific {
                e_at: idx,
                spec: FloatSpec {
                    exponent: offset + 1..offset + 1,
                    fraction: offset..offset,
                    integral: self.0.clone(),
                },
                ..Default::default()
            }))),
            'i' | 'I' => ControlFlow::Break(Ok(BreakCondition::Imaginary)),
            _ if self.0.radix.is_digit(ch) => {
                self.0.magnitude.end += 1;
                ControlFlow::Continue(None)
            }
            _ => ControlFlow::Break(Err(TokenErrorKind::NumberInvalid)),
        }
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        if self.0.is_empty() {
            Err(TokenErrorKind::NumberExpected)
        } else {
            Ok(match exactness {
                None | Some(Exactness::Exact) => self.0.into_exact(input)?.into(),
                Some(Exactness::Inexact) => self.0.into_inexact(input)?,
            })
        }
    }
}

#[derive(Debug, Default)]
struct Float(FloatSpec);

impl Float {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> RealControl<'str> {
        let (idx, ch) = item;
        let offset = self.0.fraction.end;
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
            'e' | 'E' => {
                let mut spec = self.0.clone();
                spec.exponent = offset + 1..offset + 1;
                ControlFlow::Continue(Some(RealClassifier::Sci(Scientific {
                    e_at: idx,
                    spec,
                    ..Default::default()
                })))
            }
            'i' | 'I' => ControlFlow::Break(Ok(BreakCondition::Imaginary)),
            _ if self.0.integral.radix.is_digit(ch) => {
                self.0.fraction.end += 1;
                ControlFlow::Continue(None)
            }
            _ => ControlFlow::Break(Err(TokenErrorKind::NumberInvalid)),
        }
    }

    fn parse(&self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        Ok(match exactness {
            None | Some(Exactness::Inexact) => self.0.into_inexact(input),
            Some(Exactness::Exact) => self.0.into_exact(input),
        }?)
    }
}

#[derive(Debug, Default)]
struct Scientific {
    e_at: usize,
    exponent_sign: Option<Sign>,
    spec: FloatSpec,
}

impl Scientific {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> RealControl<'str> {
        let ch = item.1;
        match ch {
            '+' | '-' => {
                if self.exponent_sign.is_some() && self.spec.exponent.len() == 1 {
                    ControlFlow::Break(Err(self.malformed_exponent()))
                } else if self.spec.exponent.is_empty() {
                    self.exponent_sign = Some(super::char_to_sign(ch));
                    self.spec.exponent.end += 1;
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
            _ if self.spec.integral.radix.is_digit(ch) => {
                self.spec.exponent.end += 1;
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
                None | Some(Exactness::Inexact) => Ok(self.spec.into_inexact(input)?),
                Some(Exactness::Exact) => match self.spec.into_exact(input) {
                    Err(
                        err @ (NumericError::ParseExponentOutOfRange
                        | NumericError::ParseExponentFailure),
                    ) => Err(TokenErrorKind::NumericErrorAt { at: self.e_at, err }),
                    r @ _ => Ok(r?),
                },
            }
        }
    }

    fn no_e_value(&self) -> bool {
        self.spec.exponent.is_empty()
            || (self.spec.exponent.len() == 1 && self.exponent_sign.is_some())
    }

    fn malformed_exponent(&self) -> TokenErrorKind {
        TokenErrorKind::NumericErrorAt {
            at: self.e_at,
            err: NumericError::ParseExponentFailure,
        }
    }
}

fn real_to_token(r: impl Into<Real>, is_imaginary: bool) -> TokenKind {
    if is_imaginary {
        TokenKind::Imaginary(r.into())
    } else {
        TokenKind::Literal(Literal::Number(Number::real(r)))
    }
}
