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
use std::{fmt::Debug, ops::ControlFlow};

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
            classifier: DecimalClassifier::Int(DecimalInt(IntSpec {
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
            classifier: DecimalClassifier::Int(DecimalInt(IntSpec {
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
            classifier: DecimalClassifier::Flt(Float(FloatSpec {
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
            classifier: DecimalClassifier::Flt(Float(FloatSpec {
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
        /*
        let (props, parser) = self.classifier.commit(self.exactness, self.scan.lexeme())
        ConditionHandler {
            props,
            parser,
            scan: self.scan,
            start: self.start,
        }
        .resolve(brk)
        */
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
    fn resolve<R: Radix + Debug>(mut self, result: RadixBreak<'str, R>) -> TokenExtractResult {
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
                    BreakCondition::Fraction(m) => match m.into_exact(self.get_lexeme()) {
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
            classifier: Integral(IntSpec {
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
                        self.classifier.0.into_exact(input).map_err(|_| self.fail())
                    }
                    BreakCondition::Fraction(_) => Err(self.fail()),
                    BreakCondition::Imaginary => {
                        if self.scan.next_if_not_delimiter().is_some() {
                            Err(self.fail())
                        } else {
                            let input = self.get_lexeme();
                            self.classifier.0.into_exact(input).map_err(|_| self.fail())
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
type RadixBreak<'str, R> = Result<BreakCondition<'str, R>, TokenErrorKind>;
type RadixControl<'str, R> = ControlFlow<RadixBreak<'str, R>>;
type ParseResult = Result<Real, TokenErrorKind>;

#[derive(Debug)]
enum BreakCondition<'str, R> {
    Complete,
    Complex {
        kind: ComplexKind,
        start: ScanItem<'str>,
    },
    Fraction(IntSpec<R>),
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
    fn commit(self, input: &str, exactness: Option<Exactness>)
    /*-> (impl ClassifierProps, impl NumericParser)*/;

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

trait ClassifierProps {
    type Radix: Radix + Clone + Debug + Default;

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

trait NumericParser {
    type ExactResult;

    fn parse(self) -> ParseResult;
    fn exact_parse(self) -> Self::ExactResult;
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
            Self::Flt(Float(f)) => f.integral.sign,
            Self::Int(DecimalInt(i)) => i.sign,
            Self::Sci(Scientific { spec, .. }) => spec.integral.sign,
        }
    }

    // NOTE: decimal classifier always classifies at least one digit
    fn is_empty(&self) -> bool {
        debug_assert!(match self {
            Self::Flt(Float(f)) => !f.is_empty(),
            Self::Int(DecimalInt(i)) => !i.is_empty(),
            Self::Sci(Scientific { spec, .. }) => !spec.is_empty(),
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

    fn commit(self, input: &str, exactness: Option<Exactness>)
    /*-> (impl ClassifierProps, impl NumericParser)*/
    {
        todo!()
    }
}

#[derive(Debug)]
struct Integral<R>(IntSpec<R>);

impl<R: Radix + Clone + Debug + Default> Integral<R> {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> RadixControl<'str, R> {
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
            '/' => ControlFlow::Break(Ok(BreakCondition::Fraction(self.0.clone()))),
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
}

impl<R: Radix + Clone + Debug + Default> Classifier for Integral<R> {
    type Radix = R;

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

    fn get_sign(&self) -> Option<Sign> {
        self.0.sign
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
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

    fn commit(self, input: &str, exactness: Option<Exactness>)
    /*-> (impl ClassifierProps, impl NumericParser)*/
    {
        todo!()
    }
}

#[derive(Debug)]
struct DecimalInt(IntSpec<Decimal>);

impl DecimalInt {
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
        let (idx, ch) = item;
        let offset = self.0.magnitude.end;
        match ch {
            '+' | '-' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Cartesian,
                start: item,
            })),
            '.' => ControlFlow::Continue(Some(DecimalClassifier::Flt(Float(FloatSpec {
                fraction: offset + 1..offset + 1,
                integral: self.0.clone(),
                ..Default::default()
            })))),
            '/' => ControlFlow::Break(Ok(BreakCondition::Fraction(self.0.clone()))),
            '@' => ControlFlow::Break(Ok(BreakCondition::Complex {
                kind: ComplexKind::Polar,
                start: item,
            })),
            'e' | 'E' => ControlFlow::Continue(Some(DecimalClassifier::Sci(Scientific {
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
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
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
                ControlFlow::Continue(Some(DecimalClassifier::Sci(Scientific {
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
    fn classify<'str>(&mut self, item: ScanItem<'str>) -> DecimalControl<'str> {
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
