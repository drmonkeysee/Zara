use crate::number::Number;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub(crate) enum Constant {
    Boolean(bool),
    Character(char),
    Number(Number),
    String(String),
}

impl Constant {
    pub(crate) fn as_token_descriptor(&self) -> TokenDescriptor {
        TokenDescriptor(self)
    }
}

pub(crate) struct TokenDescriptor<'a>(&'a Constant);

impl Display for TokenDescriptor<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.0 {
            Constant::Boolean(_) => f.write_str("BOOL"),
            Constant::Character(_) => f.write_str("CHAR"),
            Constant::Number(n) => write!(f, "NUM<{}>", n.as_token_descriptor()),
            Constant::String(_) => f.write_str("STR"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{number::Real, testutil::ok_or_fail};

    #[test]
    fn bool_token() {
        let b = Constant::Boolean(false);

        assert_eq!(b.as_token_descriptor().to_string(), "BOOL");
    }

    #[test]
    fn char_token() {
        let c = Constant::Character('a');

        assert_eq!(c.as_token_descriptor().to_string(), "CHAR");
    }

    #[test]
    fn string_token() {
        let s = Constant::String("foo".into());

        assert_eq!(s.as_token_descriptor().to_string(), "STR");
    }

    #[test]
    fn integer_token() {
        let n = Constant::Number(Number::real(42));

        assert_eq!(n.as_token_descriptor().to_string(), "NUM<INT>");
    }

    #[test]
    fn float_token() {
        let n = Constant::Number(Number::real(4.2));

        assert_eq!(n.as_token_descriptor().to_string(), "NUM<FLT>");
    }

    #[test]
    fn rational_token() {
        let r = ok_or_fail!(Real::reduce(4, 5));
        let n = Constant::Number(Number::Real(r));

        assert_eq!(n.as_token_descriptor().to_string(), "NUM<RAT>");
    }

    #[test]
    fn complex_token() {
        let n = Constant::Number(Number::complex(3, 5));

        assert_eq!(n.as_token_descriptor().to_string(), "NUM<CPX>");
    }
}
