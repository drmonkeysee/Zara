use crate::lex::tokenize::{extract::TokenExtractResult, scan::Scanner};

pub(in crate::lex::tokenize) struct Numeric<'me, 'str> {
    pub(in crate::lex::tokenize) scan: &'me mut Scanner<'str>,
}

impl<'me, 'str> Numeric<'me, 'str> {
    pub(in crate::lex::tokenize) fn scan(&mut self, first: char) -> TokenExtractResult {
        todo!();
    }
}
