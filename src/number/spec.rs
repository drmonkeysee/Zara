use super::{IntResult, NumericError, Real, RealResult, Sign};
use crate::txt::TxtSpan;
use std::num::{IntErrorKind, ParseIntError};

#[allow(private_bounds)]
pub(crate) trait Radix: RadixPrivate {
    const BASE: u32;
    const NAME: &'static str;

    fn is_digit(&self, ch: char) -> bool;
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Binary;

impl Radix for Binary {
    const BASE: u32 = 2;
    const NAME: &'static str = "binary";

    fn is_digit(&self, ch: char) -> bool {
        matches!(ch, '0'..='1')
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Octal;

impl Radix for Octal {
    const BASE: u32 = 8;
    const NAME: &'static str = "octal";

    fn is_digit(&self, ch: char) -> bool {
        // TODO: https://doc.rust-lang.org/std/primitive.char.html#method.is_ascii_octdigit
        ch.is_digit(8)
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Decimal;

impl Radix for Decimal {
    const BASE: u32 = 10;
    const NAME: &'static str = "decimal";

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }
}

#[derive(Clone, Copy, Default)]
pub(crate) struct Hexadecimal;

impl Radix for Hexadecimal {
    const BASE: u32 = 16;
    const NAME: &'static str = "hexadecimal";

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_hexdigit()
    }
}

#[derive(Clone, Default)]
pub(crate) struct IntSpec<R> {
    pub(crate) magnitude: TxtSpan,
    pub(crate) radix: R,
    pub(crate) sign: Option<Sign>,
}

impl<R: Radix> IntSpec<R> {
    pub(crate) fn is_empty(&self) -> bool {
        self.magnitude.is_empty()
    }

    pub(crate) fn has_sign(&self) -> bool {
        self.sign.is_some()
    }

    pub(crate) fn try_into_exact(self, input: &str) -> IntResult {
        parse_signed(&self, input)
    }

    pub(crate) fn try_into_inexact(self, input: &str) -> RealResult {
        R::parse_inexact(self, input)
    }
}

#[derive(Clone, Default)]
pub(crate) struct FloatSpec {
    pub(crate) exponent: TxtSpan,
    pub(crate) fraction: TxtSpan,
    pub(crate) integral: IntSpec<Decimal>,
}

impl FloatSpec {
    pub(crate) fn is_empty(&self) -> bool {
        self.exponent.is_empty() && self.fraction.is_empty() && self.integral.is_empty()
    }

    pub(crate) fn try_into_exact(self, input: &str) -> RealResult {
        let mut buf = String::new();
        let mut num = IntSpec::<Decimal>::default();
        if self.integral.has_sign() {
            buf += input.get(..1).unwrap_or_default();
            num.sign = self.integral.sign;
            num.magnitude = 1..1;
        }
        buf += input
            .get(self.integral.magnitude.clone())
            .unwrap_or_default();
        let frac = input.get(self.fraction.clone()).unwrap_or_default();
        buf += frac;
        let exponent = self.parse_exponent(input)?;
        let scale = exponent - i32::try_from(frac.len()).unwrap_or_default();
        let adjustment = "0".repeat(scale.abs().try_into().unwrap_or_default());
        if scale < 0 {
            num.magnitude.end = buf.len();
            let adjustment = format!("1{adjustment}");
            let denom = IntSpec::<Decimal> {
                magnitude: 0..adjustment.len(),
                ..Default::default()
            };
            Real::reduce(
                num.try_into_exact(&buf)?,
                denom.try_into_exact(&adjustment)?,
            )
        } else {
            buf += &adjustment;
            num.magnitude.end = buf.len();
            Ok(num.try_into_exact(&buf)?.into())
        }
    }

    pub(crate) fn try_into_inexact(self, input: &str) -> RealResult {
        let end = if !self.exponent.is_empty() {
            self.exponent.end
        } else if !self.fraction.is_empty() {
            self.fraction.end
        } else {
            self.integral.magnitude.end
        };
        input
            .get(..end)
            .map_or(Err(NumericError::ParseFailure), |fstr| {
                Ok(fstr.parse::<f64>()?.into())
            })
    }

    fn parse_exponent(&self, input: &str) -> Result<i32, NumericError> {
        if self.exponent.is_empty() {
            Ok(0)
        } else {
            input
                .get(self.exponent.clone())
                .unwrap_or_default()
                .parse()
                .map_err(|err: ParseIntError| match err.kind() {
                    IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                        NumericError::ParseExponentOutOfRange
                    }
                    _ => NumericError::ParseExponentFailure,
                })
        }
    }
}

trait RadixPrivate {
    fn parse_inexact<R: Radix>(spec: IntSpec<R>, input: &str) -> RealResult {
        // always parse exact magnitude first to account for radix
        Ok(parse_signed(&spec, input)?.into_inexact())
    }
}

impl RadixPrivate for Binary {}
impl RadixPrivate for Octal {}
impl RadixPrivate for Hexadecimal {}

impl RadixPrivate for Decimal {
    fn parse_inexact<R>(spec: IntSpec<R>, input: &str) -> RealResult {
        input
            .get(..spec.magnitude.end)
            .map_or(Err(NumericError::ParseFailure), |fstr| {
                Ok(fstr.parse::<f64>()?.into())
            })
    }
}

fn parse_signed<R: Radix>(spec: &IntSpec<R>, input: &str) -> IntResult {
    if spec.is_empty() {
        Err(NumericError::ParseFailure)
    } else {
        input
            .get(..spec.magnitude.end)
            .map_or(Err(NumericError::ParseFailure), |signed_num| {
                i64::from_str_radix(signed_num, R::BASE)
                    .map_or_else(|_| parse_sign_magnitude(spec, signed_num), |n| Ok(n.into()))
            })
    }
}

fn parse_sign_magnitude<R: Radix>(spec: &IntSpec<R>, input: &str) -> IntResult {
    input
        .get(spec.magnitude.start..)
        .map_or(Err(NumericError::ParseFailure), |mag| {
            u64::from_str_radix(mag, R::BASE).map_or_else(
                |_| parse_multi_precision(spec, input),
                |n| {
                    let sign_mag = (spec.sign.unwrap_or(Sign::Positive), n);
                    Ok(sign_mag.into())
                },
            )
        })
}

fn parse_multi_precision<R: Radix>(_spec: &IntSpec<R>, input: &str) -> IntResult {
    Err(NumericError::Unimplemented(input.to_owned()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutil::err_or_fail;
    use std::assert_matches;

    #[test]
    fn int_empty() {
        let espec = IntSpec::<Decimal>::default();
        let ispec = espec.clone();

        assert!(espec.is_empty());

        let err = err_or_fail!(espec.try_into_exact("1234"));

        assert_matches!(err, NumericError::ParseFailure);

        let err = err_or_fail!(ispec.try_into_inexact("1234"));

        assert_matches!(err, NumericError::ParseFailure);
    }

    #[test]
    fn int_blank_string() {
        let espec = IntSpec::<Decimal> {
            magnitude: 1..3,
            ..Default::default()
        };
        let ispec = espec.clone();

        assert!(!espec.is_empty());

        let err = err_or_fail!(espec.try_into_exact(""));

        assert_matches!(err, NumericError::ParseFailure);

        let err = err_or_fail!(ispec.try_into_inexact(""));

        assert_matches!(err, NumericError::ParseFailure);
    }

    #[test]
    fn float_empty() {
        let espec = FloatSpec::default();
        let ispec = espec.clone();

        assert!(espec.is_empty());

        let err = err_or_fail!(espec.try_into_exact("1234.456e3"));

        assert_matches!(err, NumericError::ParseFailure);

        let err = err_or_fail!(ispec.try_into_inexact("1234.456e3"));

        assert_matches!(err, NumericError::ParseFailure);
    }

    #[test]
    fn float_blank_string() {
        let espec = FloatSpec {
            exponent: 4..5,
            fraction: 2..3,
            integral: IntSpec {
                magnitude: 0..1,
                ..Default::default()
            },
        };
        let ispec = espec.clone();

        assert!(!espec.is_empty());

        let err = err_or_fail!(espec.try_into_exact(""));

        assert_matches!(err, NumericError::ParseExponentFailure);

        let err = err_or_fail!(ispec.try_into_inexact(""));

        assert_matches!(err, NumericError::ParseFailure);
    }
}
