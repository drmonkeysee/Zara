use super::{FreeText, FreeTextPolicy};
use crate::{
    constant::Constant,
    lex::{TokenKind, token::TokenErrorKind, tokenize::scan::Scanner},
};

pub(in crate::lex::tokenize) type StringLiteral<'me, 'txt, M> =
    FreeText<'me, 'txt, StringPolicy<M>>;

impl<'me, 'txt> StringLiteral<'me, 'txt, StartString> {
    pub(in crate::lex::tokenize) fn new(scanner: &'me mut Scanner<'txt>) -> Self {
        Self::init(scanner, StringPolicy(StartString))
    }
}

impl<'me, 'txt> StringLiteral<'me, 'txt, ContinueString> {
    pub(in crate::lex::tokenize) fn cont(scanner: &'me mut Scanner<'txt>) -> Self {
        Self::init(scanner, StringPolicy(ContinueString))
    }
}

impl<'me, 'txt> StringLiteral<'me, 'txt, LineContinueString> {
    pub(in crate::lex::tokenize) fn line_cont(scanner: &'me mut Scanner<'txt>) -> Self {
        Self::init(scanner, StringPolicy(LineContinueString))
    }
}

impl<'me, 'txt> StringLiteral<'me, 'txt, DiscardString> {
    pub(in crate::lex::tokenize) fn cleanup(scanner: &'me mut Scanner<'txt>) -> Self {
        Self::init(scanner, StringPolicy(DiscardString))
    }
}

pub(in crate::lex::tokenize) struct StringPolicy<M>(M);

impl<M: StringPolicyMode> FreeTextPolicy for StringPolicy<M> {
    const TERMINATOR: char = '"';

    fn prelude(&self, scanner: &mut Scanner) {
        self.0.prelude(scanner);
    }

    fn escape_invalid(&self, start: usize, ch: char) -> TokenErrorKind {
        TokenErrorKind::StringEscapeInvalid { at: start, ch }
    }

    fn hex_expected(&self, start: usize) -> TokenErrorKind {
        TokenErrorKind::StringExpectedHex { at: start }
    }

    fn hex_invalid(&self, start: usize) -> TokenErrorKind {
        TokenErrorKind::StringInvalidHex { at: start }
    }

    fn hex_unterminated(&self, start: usize) -> TokenErrorKind {
        TokenErrorKind::StringUnterminatedHex { at: start }
    }

    fn terminated(&self, buf: String) -> TokenKind {
        self.0.terminated(buf)
    }

    fn unterminated(&self, buf: String, line_cont_idx: Option<usize>) -> TokenKind {
        if let Some(idx) = line_cont_idx {
            let (lead, trail) = buf.split_at(idx);
            if trail.trim().is_empty() {
                return self.0.unterminated(lead.to_owned(), true);
            }
        }
        self.0.unterminated(buf, false)
    }
}

pub(in crate::lex::tokenize) struct StartString;

impl StringPolicyMode for StartString {
    fn terminated(&self, buf: String) -> TokenKind {
        TokenKind::Constant(Constant::String(buf.into()))
    }

    fn unterminated(&self, buf: String, line_cont: bool) -> TokenKind {
        TokenKind::StringBegin { s: buf, line_cont }
    }
}

pub(in crate::lex::tokenize) struct ContinueString;

impl StringPolicyMode for ContinueString {}

pub(in crate::lex::tokenize) struct LineContinueString;

impl StringPolicyMode for LineContinueString {
    fn prelude(&self, scanner: &mut Scanner) {
        scanner.skip_whitespace();
    }
}

pub(in crate::lex::tokenize) struct DiscardString;

impl StringPolicyMode for DiscardString {
    fn terminated(&self, _buf: String) -> TokenKind {
        TokenKind::StringDiscard
    }

    fn unterminated(&self, buf: String, _line_cont: bool) -> TokenKind {
        self.terminated(buf)
    }
}

trait StringPolicyMode {
    fn prelude(&self, _scanner: &mut Scanner) {
        // NOTE: do nothing by default
    }

    fn terminated(&self, buf: String) -> TokenKind {
        TokenKind::StringEnd(buf)
    }

    fn unterminated(&self, buf: String, line_cont: bool) -> TokenKind {
        TokenKind::StringFragment { s: buf, line_cont }
    }
}
