use super::{FreeText, FreeTextPolicy};
use crate::{
    lex::{
        token::{TokenErrorKind, TokenKind},
        tokenize::scan::Scanner,
    },
    literal::Literal,
};

pub(in crate::lex::tokenize) type StringLiteral<'me, 'txt, M> =
    FreeText<'me, 'txt, StringPolicy<M>>;

impl<'me, 'txt> StringLiteral<'me, 'txt, StartString> {
    pub(in crate::lex::tokenize) fn new(scan: &'me mut Scanner<'txt>) -> Self {
        Self::init(scan, StringPolicy(StartString))
    }
}

impl<'me, 'txt> StringLiteral<'me, 'txt, ContinueString> {
    pub(in crate::lex::tokenize) fn cont(scan: &'me mut Scanner<'txt>) -> Self {
        Self::init(scan, StringPolicy(ContinueString))
    }
}

impl<'me, 'txt> StringLiteral<'me, 'txt, LineContinueString> {
    pub(in crate::lex::tokenize) fn line_cont(scan: &'me mut Scanner<'txt>) -> Self {
        Self::init(scan, StringPolicy(LineContinueString))
    }
}

impl<'me, 'txt> StringLiteral<'me, 'txt, DiscardString> {
    pub(in crate::lex::tokenize) fn cleanup(scan: &'me mut Scanner<'txt>) -> Self {
        Self::init(scan, StringPolicy(DiscardString))
    }
}

pub(in crate::lex::tokenize) struct StringPolicy<M>(M);

impl<M: StringPolicyMode> FreeTextPolicy for StringPolicy<M> {
    const TERMINATOR: char = '"';

    fn prelude(&self, scan: &mut Scanner<'_>) {
        self.0.prelude(scan);
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
        TokenKind::Literal(Literal::String(buf))
    }

    fn unterminated(&self, buf: String, line_cont: bool) -> TokenKind {
        TokenKind::StringBegin { s: buf, line_cont }
    }
}

pub(in crate::lex::tokenize) struct ContinueString;

impl StringPolicyMode for ContinueString {}

pub(in crate::lex::tokenize) struct LineContinueString;

impl StringPolicyMode for LineContinueString {
    fn prelude(&self, scan: &mut Scanner<'_>) {
        scan.skip_whitespace();
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
    fn prelude(&self, _scan: &mut Scanner<'_>) {
        // NOTE: do nothing by default
    }

    fn terminated(&self, buf: String) -> TokenKind {
        TokenKind::StringEnd(buf)
    }

    fn unterminated(&self, buf: String, line_cont: bool) -> TokenKind {
        TokenKind::StringFragment { s: buf, line_cont }
    }
}
