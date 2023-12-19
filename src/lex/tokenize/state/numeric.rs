use crate::lex::tokenize::{extract::TokenExtractResult, scan::Scanner};

pub(in crate::lex::tokenize) struct Numeric<'me, 'str, R> {
    radix: R,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str, R: Radix> Numeric<'me, 'str, R> {
    fn new(scan: &'me mut Scanner<'str>, radix: R) -> Self {
        Self { scan, radix }
    }

    pub(in crate::lex::tokenize) fn scan(&mut self, first: char) -> TokenExtractResult {
        match first {
            '+' | '-' => todo!(),                       // explicit signed
            '.' => todo!(),                             // float
            _ if self.radix.is_digit(first) => todo!(), // standard
            _ => todo!(),
        }
    }
}

impl<'me, 'str> Numeric<'me, 'str, Decimal> {
    pub(in crate::lex::tokenize) fn decimal(scan: &'me mut Scanner<'str>) -> Self {
        Self::new(scan, Decimal)
    }
}

pub(in crate::lex::tokenize) trait Radix {
    fn is_digit(&self, ch: char) -> bool;
}

pub(in crate::lex::tokenize) struct Decimal;

impl Radix for Decimal {
    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }
}
