use super::{
    ComplexKind, Exactness, Identifier, ScanItem, Scanner, TokenErrorKind, TokenExtractResult,
    TokenKind,
};
use crate::number::{
    Decimal, FloatSpec, IntSpec, Integer, Number, NumericError, Radix, Real, Sign,
};
use std::{marker::PhantomData, ops::ControlFlow};

pub(super) struct RealNumber<'me, 'txt> {
    classifier: RealClassifier,
    exactness: Option<Exactness>,
    scanner: &'me mut Scanner<'txt>,
    start: usize,
}

// NOTE: this lexer is only invoked after one confirmed digit has been scanned
impl<'me, 'txt> RealNumber<'me, 'txt> {
    pub(super) fn new(
        scanner: &'me mut Scanner<'txt>,
        start: usize,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: RealClassifier::Int(RealInt(IntSpec {
                magnitude: 0..1,
                ..Default::default()
            })),
            exactness,
            scanner,
            start,
        }
    }

    pub(super) fn try_signed_number(
        sign: Sign,
        scanner: &'me mut Scanner<'txt>,
        start: usize,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: RealClassifier::Int(RealInt(IntSpec {
                magnitude: 1..2,
                sign: Some(sign),
                ..Default::default()
            })),
            exactness,
            scanner,
            start,
        }
    }

    pub(super) fn try_float(
        scanner: &'me mut Scanner<'txt>,
        start: usize,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: RealClassifier::Flt(Float(FloatSpec {
                fraction: 1..2,
                ..Default::default()
            })),
            exactness,
            scanner,
            start,
        }
    }

    pub(super) fn try_signed_float(
        sign: Sign,
        scanner: &'me mut Scanner<'txt>,
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
            scanner,
            start,
        }
    }

    pub(super) fn scan(mut self) -> TokenExtractResult {
        let mut brk = Ok(BreakCondition::default());
        while let Some(item) = self.scanner.next_if_not_delimiter() {
            match self.classifier.classify(item) {
                RealControl::Break(b) => {
                    brk = b;
                    break;
                }
                RealControl::Continue(None) => (),
                RealControl::Continue(Some(c)) => {
                    self.classifier = c;
                }
            }
        }
        let cond = brk?;
        let (props, parser) = self
            .classifier
            .commit(self.scanner.current_lexeme_at(self.start), self.exactness);
        ConditionProcessor {
            props,
            scanner: self.scanner,
        }
        .resolve(parser, &cond)
    }
}

pub(super) struct RadixNumber<'me, 'txt, R> {
    classifier: Integral<R>,
    exactness: Option<Exactness>,
    scanner: &'me mut Scanner<'txt>,
    start: usize,
}

impl<'me, 'txt, R: Radix + Default> RadixNumber<'me, 'txt, R> {
    pub(super) fn new(scanner: &'me mut Scanner<'txt>, exactness: Option<Exactness>) -> Self {
        let start = scanner.pos();
        Self {
            classifier: Integral {
                spec: IntSpec::default(),
                ..Default::default()
            },
            exactness,
            scanner,
            start,
        }
    }

    fn with_sign(
        scanner: &'me mut Scanner<'txt>,
        (start, sign): ScanItem,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            classifier: Integral {
                spec: IntSpec {
                    magnitude: 1..1,
                    sign: Some(super::char_to_sign(sign)),
                    ..Default::default()
                },
                ..Default::default()
            },
            exactness,
            scanner,
            start,
        }
    }

    pub(super) fn scan(mut self) -> TokenExtractResult {
        let mut brk = Ok(BreakCondition::default());
        while let Some(item) = self.scanner.next_if_not_delimiter() {
            match self.classifier.classify(item) {
                RadixControl::Break(b) => {
                    brk = b;
                    break;
                }
                RadixControl::Continue(u) => u,
            }
        }
        let cond = self.classifier.finalize_condition(brk?);
        let (props, parser) = self
            .classifier
            .commit(self.scanner.current_lexeme_at(self.start), self.exactness);
        ConditionProcessor {
            props,
            scanner: self.scanner,
        }
        .resolve(parser, &cond)
    }
}

pub(super) fn imaginary(sign: Sign, exactness: Option<Exactness>) -> TokenKind {
    let sign_val = sign as i64;
    let r = match exactness {
        None | Some(Exactness::Exact) => Real::from(sign_val),
        #[allow(clippy::cast_precision_loss)]
        Some(Exactness::Inexact) => Real::from(sign_val as f64),
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

struct ConditionProcessor<'me, 'txt, P> {
    props: P,
    scanner: &'me mut Scanner<'txt>,
}

impl<P: ClassifierProps> ConditionProcessor<'_, '_, P> {
    fn resolve<N: ClassifierParser>(
        mut self,
        parser: N,
        cond: &BreakCondition,
    ) -> TokenExtractResult {
        match cond {
            BreakCondition::Fraction => {
                if self.props.radix_infnan() {
                    Err(TokenErrorKind::RationalInvalid)
                } else {
                    parser
                        .parse_int()
                        .and_then(|numerator| self.scan_denominator(numerator))
                }
            }
            BreakCondition::Sub(SubCondition::Complete) => self.complete(parser, false),
            BreakCondition::Sub(SubCondition::Complex { kind, start }) => {
                if self.props.radix_infnan() {
                    if let Ok(TokenKind::Number(Number::Real(r))) = parser.extract_radix_infnan() {
                        Ok(r)
                    } else {
                        Err(match kind {
                            ComplexKind::Cartesian => TokenErrorKind::ComplexInvalid,
                            ComplexKind::Polar => TokenErrorKind::PolarInvalid,
                        })
                    }
                } else {
                    // NOTE: delay application of exactness until final
                    // composition of complex number; specifically Polar
                    // will round-trip inputs through float representation,
                    // undoing any exactness applied to real part.
                    parser.parse(None)
                }
                .and_then(|real| self.scan_imaginary(real, *kind, *start))
            }
            BreakCondition::Sub(SubCondition::Imaginary) => {
                if let Some(item) = self.scanner.next_if_not_delimiter() {
                    // NOTE: maybe malformed "in"finity? otherwise assume malformed imaginary
                    Err(if item.1.is_ascii_alphabetic() {
                        TokenErrorKind::NumberInvalid
                    } else {
                        TokenErrorKind::ImaginaryInvalid
                    })
                } else if !self.props.has_sign() {
                    Err(TokenErrorKind::ImaginaryMissingSign)
                } else {
                    self.complete(parser, true)
                }
            }
        }
    }

    fn scan_denominator(&mut self, numerator: Integer) -> TokenExtractResult {
        let (denominator, cond) = Denominator::new(self.scanner).scan::<P::Radix>()?;
        let mut real = Real::reduce(numerator, denominator)?;
        match cond {
            SubCondition::Complete => {
                if let Some(Exactness::Inexact) = self.props.get_exactness() {
                    real = real.into_inexact();
                }
                Ok(real_to_token(real, false))
            }
            SubCondition::Complex { kind, start } => self.scan_imaginary(real, kind, start),
            SubCondition::Imaginary => {
                if self.props.has_sign() {
                    if let Some(Exactness::Inexact) = self.props.get_exactness() {
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
                match self.props.cartesian_scan(self.scanner, start) {
                    Ok(TokenKind::Imaginary(imag)) => {
                        let real = match self.props.get_exactness() {
                            // NOTE: this conversion shouldn't ever fail because real has already
                            // been parsed as a valid float, but if it does, give up and return NaN.
                            Some(Exactness::Exact) => real.try_into_exact().unwrap_or(Real::nan()),
                            Some(Exactness::Inexact) => real.into_inexact(),
                            None => real,
                        };
                        Ok(TokenKind::Number(Number::complex(real, imag)))
                    }
                    _ => Err(TokenErrorKind::ComplexInvalid),
                }
            }
            ComplexKind::Polar => {
                debug_assert_eq!(start.1, '@');
                match self.props.polar_scan(self.scanner) {
                    Ok(TokenKind::Number(Number::Real(rads))) => {
                        let pol = Number::polar(real, rads);
                        Ok(TokenKind::Number(match self.props.get_exactness() {
                            // NOTE: this conversion shouldn't ever fail because pol has already
                            // been parsed as a valid float-complex, but if it does, give up and return NaN.
                            Some(Exactness::Exact) => pol.try_into_exact().unwrap_or(Number::nan()),
                            Some(Exactness::Inexact) => pol.into_inexact(),
                            None => pol,
                        }))
                    }
                    _ => Err(TokenErrorKind::PolarInvalid),
                }
            }
        }
    }

    fn complete<N: ClassifierParser>(
        &mut self,
        parser: N,
        is_imaginary: bool,
    ) -> TokenExtractResult {
        if self.props.radix_infnan() {
            return parser.extract_radix_infnan();
        }
        if is_imaginary && self.props.is_empty() {
            if let Some(sign) = self.props.get_sign() {
                return Ok(imaginary(sign, self.props.get_exactness()));
            }
        }
        parser
            .parse(self.props.get_exactness())
            .map(|r| real_to_token(r, is_imaginary))
    }
}

struct Denominator<'me, 'txt> {
    scanner: &'me mut Scanner<'txt>,
    start: usize,
}

impl<'me, 'txt> Denominator<'me, 'txt> {
    fn new(scanner: &'me mut Scanner<'txt>) -> Self {
        let start = scanner.pos();
        Self { scanner, start }
    }

    fn scan<R: Radix + Default>(mut self) -> Result<(Integer, SubCondition<'txt>), TokenErrorKind> {
        let mut brk = Ok(BreakCondition::default());
        let mut classifier = Integral::<R> {
            spec: IntSpec {
                sign: Some(Sign::Positive),
                ..Default::default()
            },
            ..Default::default()
        };
        while let Some(item) = self.scanner.next_if_not_delimiter() {
            match classifier.classify(item) {
                RadixControl::Break(b) => {
                    brk = b;
                    break;
                }
                RadixControl::Continue(u) => u,
            }
        }
        let cond = brk.map_err(|_| TokenErrorKind::RationalInvalid)?;
        let (_, parser) = classifier.commit(self.scanner.current_lexeme_at(self.start), None);
        Ok((
            if self.should_parse(&cond) {
                parser
                    .parse_int()
                    .map_err(|_| TokenErrorKind::RationalInvalid)
            } else {
                Err(TokenErrorKind::RationalInvalid)
            }?,
            cond.into(),
        ))
    }

    fn should_parse(&mut self, cond: &BreakCondition) -> bool {
        match cond {
            BreakCondition::Sub(SubCondition::Complete | SubCondition::Complex { .. }) => true,
            BreakCondition::Sub(SubCondition::Imaginary) => {
                self.scanner.next_if_not_delimiter().is_none()
            }
            _ => false,
        }
    }
}

type BreakResult<'txt> = Result<BreakCondition<'txt>, TokenErrorKind>;
type RealControl<'txt> = ControlFlow<BreakResult<'txt>, Option<RealClassifier>>;
type RadixControl<'txt> = ControlFlow<BreakResult<'txt>>;
type ParseResult = Result<Real, TokenErrorKind>;
type IntParseResult = Result<Integer, TokenErrorKind>;

enum BreakCondition<'txt> {
    Fraction,
    Sub(SubCondition<'txt>),
}

impl BreakCondition<'_> {
    fn is_default(&self) -> bool {
        matches!(self, Self::Sub(SubCondition::Complete))
    }

    fn imaginary() -> Self {
        Self::Sub(SubCondition::Imaginary)
    }

    fn cartesian(start: ScanItem) -> Self {
        Self::Sub(SubCondition::Complex {
            kind: ComplexKind::Cartesian,
            start,
        })
    }

    fn polar(start: ScanItem) -> Self {
        Self::Sub(SubCondition::Complex {
            kind: ComplexKind::Polar,
            start,
        })
    }
}

impl Default for BreakCondition<'_> {
    fn default() -> Self {
        Self::Sub(SubCondition::Complete)
    }
}

enum SubCondition<'txt> {
    Complete,
    Complex {
        kind: ComplexKind,
        start: ScanItem<'txt>,
    },
    Imaginary,
}

impl<'txt> From<BreakCondition<'txt>> for SubCondition<'txt> {
    fn from(value: BreakCondition<'txt>) -> Self {
        match value {
            // NOTE: Denominator ensures this case never happens
            BreakCondition::Fraction => unreachable!("unexpected break-to-sub conversion case"),
            BreakCondition::Sub(s) => s,
        }
    }
}

trait ClassifierProps {
    type Radix: Radix + Default;

    fn get_sign(&self) -> Option<Sign>;
    fn get_exactness(&self) -> Option<Exactness>;
    fn is_empty(&self) -> bool;
    fn cartesian_scan(&self, scanner: &mut Scanner, start: ScanItem) -> TokenExtractResult;
    fn polar_scan(&self, scanner: &mut Scanner) -> TokenExtractResult;

    fn radix_infnan(&self) -> bool {
        false
    }

    fn has_sign(&self) -> bool {
        self.get_sign().is_some()
    }
}

trait ClassifierParser {
    fn parse(self, exactness: Option<Exactness>) -> ParseResult;
    fn parse_int(self) -> IntParseResult;

    fn extract_radix_infnan(self) -> TokenExtractResult
    where
        Self: Sized,
    {
        Err(TokenErrorKind::NumberInvalid)
    }
}

enum RealClassifier {
    Flt(Float),
    Int(RealInt),
    Sci(Scientific),
}

impl RealClassifier {
    fn classify<'txt>(&mut self, item: ScanItem<'txt>) -> RealControl<'txt> {
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
            RealParser {
                classifier: self,
                input,
            },
        )
    }

    fn into_int_spec(self) -> IntSpec<Decimal> {
        match self {
            Self::Flt(Float(f)) => f.integral,
            Self::Int(RealInt(i)) => i,
            Self::Sci(Scientific { spec, .. }) => spec.integral,
        }
    }

    fn get_sign(&self) -> Option<Sign> {
        match self {
            Self::Flt(Float(f)) => f.integral.sign,
            Self::Int(RealInt(i)) => i.sign,
            Self::Sci(Scientific { spec, .. }) => spec.integral.sign,
        }
    }

    // NOTE: real classifier always classifies at least one digit
    fn is_empty(&self) -> bool {
        debug_assert!(match self {
            Self::Flt(Float(f)) => !f.is_empty(),
            Self::Int(RealInt(i)) => !i.is_empty(),
            Self::Sci(Scientific { spec, .. }) => !spec.is_empty(),
        });
        false
    }

    fn parse(self, input: &str, exactness: Option<Exactness>) -> ParseResult {
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

    fn cartesian_scan(&self, scanner: &mut Scanner, start: ScanItem) -> TokenExtractResult {
        Identifier::with_exactness(scanner, start, self.get_exactness()).scan()
    }

    fn polar_scan(&self, scanner: &mut Scanner) -> TokenExtractResult {
        scanner
            .next_if_not_delimiter()
            .map_or(Err(TokenErrorKind::PolarInvalid), |start| {
                Identifier::new(scanner, start).scan()
            })
    }
}

struct RealParser<'txt> {
    classifier: RealClassifier,
    input: &'txt str,
}

impl ClassifierParser for RealParser<'_> {
    fn parse(self, exactness: Option<Exactness>) -> ParseResult {
        self.classifier.parse(self.input, exactness)
    }

    fn parse_int(self) -> IntParseResult {
        Ok(self.classifier.into_int_spec().try_into_exact(self.input)?)
    }
}

#[derive(Default)]
enum IntegralMode {
    Inf(usize),
    #[default]
    Int,
    Nan(usize),
}

impl IntegralMode {
    fn len(self) -> Option<usize> {
        match self {
            Self::Inf(len) | Self::Nan(len) => Some(len),
            Self::Int => None,
        }
    }
}

#[derive(Default)]
struct Integral<R> {
    mode: IntegralMode,
    spec: IntSpec<R>,
}

impl<R: Radix> Integral<R> {
    fn classify<'txt>(&mut self, item: ScanItem<'txt>) -> RadixControl<'txt> {
        match &mut self.mode {
            IntegralMode::Inf(end) | IntegralMode::Nan(end) => classify_radix_infnan(item, end),
            IntegralMode::Int => self.classify_int(item),
        }
    }

    fn finalize_condition<'txt>(&mut self, brk: BreakCondition<'txt>) -> BreakCondition<'txt> {
        // NOTE: if inf scan only found 'i' then turns out this was imaginary, not infinity
        if brk.is_default()
            && let IntegralMode::Inf(len) = self.mode
            && len == 2
        {
            self.mode = IntegralMode::Int;
            BreakCondition::imaginary()
        } else {
            brk
        }
    }

    fn commit(self, input: &str, exactness: Option<Exactness>) -> (RadixProps<R>, RadixParser<R>) {
        let infnan_len = self.mode.len();
        (
            RadixProps {
                infnan: infnan_len.is_some(),
                props: RealProps {
                    empty: self.spec.is_empty(),
                    exactness,
                    sign: self.spec.sign,
                },
                radix: PhantomData,
            },
            RadixParser {
                infnan_len,
                input,
                spec: self.spec,
            },
        )
    }

    fn has_sign(&self) -> bool {
        self.spec.has_sign()
    }

    fn is_empty(&self) -> bool {
        self.spec.is_empty()
    }

    fn classify_int<'txt>(&mut self, item: ScanItem<'txt>) -> RadixControl<'txt> {
        let (idx, ch) = item;
        match ch {
            '+' | '-' => {
                if self.spec.is_empty() {
                    if self.spec.sign.is_none() {
                        self.spec.sign = Some(super::char_to_sign(ch));
                        self.spec.magnitude = 1..1;
                        RadixControl::Continue(())
                    } else {
                        RadixControl::Break(Err(TokenErrorKind::NumberInvalid))
                    }
                } else {
                    RadixControl::Break(Ok(BreakCondition::cartesian(item)))
                }
            }
            '.' => RadixControl::Break(Err(TokenErrorKind::NumberInvalidDecimalPoint {
                at: idx,
                radix: R::NAME,
            })),
            '/' => RadixControl::Break(Ok(BreakCondition::Fraction)),
            '@' => RadixControl::Break(Ok(BreakCondition::polar(item))),
            'i' | 'I' => {
                if self.is_empty() && self.has_sign() {
                    // NOTE: length includes sign
                    self.mode = IntegralMode::Inf(2);
                    RadixControl::Continue(())
                } else {
                    RadixControl::Break(Ok(BreakCondition::imaginary()))
                }
            }
            'n' | 'N' => {
                if self.is_empty() && self.has_sign() {
                    // NOTE: length includes sign
                    self.mode = IntegralMode::Nan(2);
                    RadixControl::Continue(())
                } else {
                    RadixControl::Break(Err(TokenErrorKind::NumberInvalid))
                }
            }
            _ if self.spec.radix.is_digit(ch) => {
                self.spec.magnitude.end += 1;
                RadixControl::Continue(())
            }
            // NOTE: e|E hexadecimal is_digit is true, so check exponent after digit
            'e' | 'E' => RadixControl::Break(Err(TokenErrorKind::NumberInvalidExponent {
                at: idx,
                radix: R::NAME,
            })),
            _ => RadixControl::Break(Err(if !self.has_sign() && self.is_empty() {
                TokenErrorKind::NumberExpected
            } else {
                TokenErrorKind::NumberInvalid
            })),
        }
    }
}

struct RadixProps<R> {
    infnan: bool,
    props: RealProps,
    radix: PhantomData<R>,
}

impl<R: Radix + Default> ClassifierProps for RadixProps<R> {
    type Radix = R;

    fn get_sign(&self) -> Option<Sign> {
        self.props.sign
    }

    fn get_exactness(&self) -> Option<Exactness> {
        self.props.exactness
    }

    fn is_empty(&self) -> bool {
        self.props.empty
    }

    fn cartesian_scan(&self, scanner: &mut Scanner, start: ScanItem) -> TokenExtractResult {
        RadixNumber::<R>::with_sign(scanner, start, self.get_exactness()).scan()
    }

    fn polar_scan(&self, scanner: &mut Scanner) -> TokenExtractResult {
        RadixNumber::<R>::new(scanner, Some(Exactness::Exact)).scan()
    }

    fn radix_infnan(&self) -> bool {
        self.infnan
    }
}

struct RadixParser<'txt, R> {
    infnan_len: Option<usize>,
    input: &'txt str,
    spec: IntSpec<R>,
}

impl<R: Radix> ClassifierParser for RadixParser<'_, R> {
    fn parse(self, exactness: Option<Exactness>) -> ParseResult {
        parse_intspec(self.spec, self.input, exactness)
    }

    fn parse_int(self) -> IntParseResult {
        Ok(self.spec.try_into_exact(self.input)?)
    }

    fn extract_radix_infnan(self) -> TokenExtractResult {
        super::numeric_symbol(
            self.infnan_len.map_or(Ok(self.input), |len| {
                self.input.get(0..len).ok_or(TokenErrorKind::NumberInvalid)
            })?,
            None,
        )
        .ok_or(TokenErrorKind::NumberInvalid)
    }
}

struct RealInt(IntSpec<Decimal>);

impl RealInt {
    fn classify<'txt>(&mut self, item: ScanItem<'txt>) -> RealControl<'txt> {
        let (idx, ch) = item;
        let offset = self.0.magnitude.end;
        match ch {
            '+' | '-' => RealControl::Break(Ok(BreakCondition::cartesian(item))),
            '.' => RealControl::Continue(Some(RealClassifier::Flt(Float(FloatSpec {
                #[allow(clippy::range_plus_one)]
                fraction: offset + 1..offset + 1,
                integral: self.0.clone(),
                ..Default::default()
            })))),
            '/' => RealControl::Break(Ok(BreakCondition::Fraction)),
            '@' => RealControl::Break(Ok(BreakCondition::polar(item))),
            'e' | 'E' => RealControl::Continue(Some(RealClassifier::Sci(Scientific {
                e_at: idx,
                spec: FloatSpec {
                    #[allow(clippy::range_plus_one)]
                    exponent: offset + 1..offset + 1,
                    fraction: offset..offset,
                    integral: self.0.clone(),
                },
                ..Default::default()
            }))),
            'i' | 'I' => RealControl::Break(Ok(BreakCondition::imaginary())),
            _ if self.0.radix.is_digit(ch) => {
                self.0.magnitude.end += 1;
                RealControl::Continue(None)
            }
            _ => RealControl::Break(Err(TokenErrorKind::NumberInvalid)),
        }
    }

    fn parse(self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        parse_intspec(self.0, input, exactness)
    }
}

#[derive(Default)]
struct Float(FloatSpec);

impl Float {
    fn classify<'txt>(&mut self, item: ScanItem<'txt>) -> RealControl<'txt> {
        let (idx, ch) = item;
        let offset = self.0.fraction.end;
        match ch {
            '+' | '-' => RealControl::Break(Ok(BreakCondition::cartesian(item))),
            '.' => RealControl::Break(Err(TokenErrorKind::NumberUnexpectedDecimalPoint {
                at: idx,
            })),
            '/' => RealControl::Break(Err(TokenErrorKind::RationalInvalid)),
            '@' => RealControl::Break(Ok(BreakCondition::polar(item))),
            // TODO: https://github.com/rust-lang/rust/issues/15701
            #[allow(clippy::range_plus_one)]
            'e' | 'E' => {
                let mut spec = self.0.clone();
                spec.exponent = offset + 1..offset + 1;
                RealControl::Continue(Some(RealClassifier::Sci(Scientific {
                    e_at: idx,
                    spec,
                    ..Default::default()
                })))
            }
            'i' | 'I' => RealControl::Break(Ok(BreakCondition::imaginary())),
            _ if self.0.integral.radix.is_digit(ch) => {
                self.0.fraction.end += 1;
                RealControl::Continue(None)
            }
            _ => RealControl::Break(Err(TokenErrorKind::NumberInvalid)),
        }
    }

    fn parse(self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        Ok(match exactness {
            None | Some(Exactness::Inexact) => self.0.try_into_inexact(input),
            Some(Exactness::Exact) => self.0.try_into_exact(input),
        }?)
    }
}

#[derive(Default)]
struct Scientific {
    e_at: usize,
    exponent_sign: Option<Sign>,
    spec: FloatSpec,
}

impl Scientific {
    fn classify<'txt>(&mut self, item: ScanItem<'txt>) -> RealControl<'txt> {
        let ch = item.1;
        match ch {
            '+' | '-' => {
                if self.exponent_sign.is_some() && self.spec.exponent.len() == 1 {
                    RealControl::Break(Err(self.malformed_exponent()))
                } else if self.spec.exponent.is_empty() {
                    self.exponent_sign = Some(super::char_to_sign(ch));
                    self.spec.exponent.end += 1;
                    RealControl::Continue(None)
                } else {
                    RealControl::Break(Ok(BreakCondition::cartesian(item)))
                }
            }
            '/' => RealControl::Break(Err(TokenErrorKind::RationalInvalid)),
            '@' => RealControl::Break(Ok(BreakCondition::polar(item))),
            'i' | 'I' => RealControl::Break(if self.no_e_value() {
                Err(self.malformed_exponent())
            } else {
                Ok(BreakCondition::imaginary())
            }),
            _ if self.spec.integral.radix.is_digit(ch) => {
                self.spec.exponent.end += 1;
                RealControl::Continue(None)
            }
            _ => RealControl::Break(Err(self.malformed_exponent())),
        }
    }

    fn parse(self, input: &str, exactness: Option<Exactness>) -> ParseResult {
        if self.no_e_value() {
            Err(self.malformed_exponent())
        } else {
            match exactness {
                None | Some(Exactness::Inexact) => Ok(self.spec.try_into_inexact(input)?),
                Some(Exactness::Exact) => match self.spec.try_into_exact(input) {
                    Err(
                        err @ (NumericError::ParseExponentOutOfRange
                        | NumericError::ParseExponentFailure),
                    ) => Err(TokenErrorKind::NumericErrorAt { at: self.e_at, err }),
                    r => Ok(r?),
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

fn classify_radix_infnan<'txt>(item: ScanItem<'txt>, len: &mut usize) -> RadixControl<'txt> {
    match item.1 {
        '+' | '-' => RadixControl::Break(Ok(BreakCondition::cartesian(item))),
        '/' => RadixControl::Break(Ok(BreakCondition::Fraction)),
        '@' => RadixControl::Break(Ok(BreakCondition::polar(item))),
        'i' | 'I' => {
            *len += 1;
            RadixControl::Break(Ok(BreakCondition::imaginary()))
        }
        _ => {
            *len += 1;
            RadixControl::Continue(())
        }
    }
}

fn real_to_token(r: impl Into<Real>, is_imaginary: bool) -> TokenKind {
    if is_imaginary {
        TokenKind::Imaginary(r.into())
    } else {
        TokenKind::Number(Number::real(r))
    }
}

fn parse_intspec<R: Radix>(
    spec: IntSpec<R>,
    input: &str,
    exactness: Option<Exactness>,
) -> ParseResult {
    if spec.is_empty() {
        Err(TokenErrorKind::NumberExpected)
    } else {
        Ok(match exactness {
            None | Some(Exactness::Exact) => spec.try_into_exact(input)?.into(),
            Some(Exactness::Inexact) => spec.try_into_inexact(input)?,
        })
    }
}
