mod hashtag;
mod identifier;
mod numeric;
mod string;

// NOTE: re-export for parent module
pub(super) use self::{
    hashtag::{BlockComment, Hashtag},
    identifier::{Identifier, PeriodIdentifier, VerbatimIdentifer},
    numeric::Numeric,
    string::StringLiteral,
};
use super::{extract::TokenExtractResult, scan::Scanner};
use crate::lex::token::{TokenErrorKind, TokenKind};

pub(super) struct FreeText<'me, 'str, P> {
    buf: String,
    policy: P,
    possible_line_cont_idx: Option<usize>,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

impl<'me, 'str, P: FreeTextPolicy> FreeText<'me, 'str, P> {
    fn init(scan: &'me mut Scanner<'str>, policy: P) -> Self {
        Self {
            buf: String::new(),
            policy,
            possible_line_cont_idx: None,
            scan,
            start: 0,
        }
    }

    pub(super) fn scan(mut self) -> TokenExtractResult {
        self.policy.prelude(self.scan);
        while let Some((idx, ch)) = self.scan.next() {
            self.start = idx;
            match ch {
                '\\' => self.escape()?,
                _ if ch == P::TERMINATOR => return Ok(self.terminated()),
                _ => self.buf.push(ch),
            }
        }
        Ok(self.unterminated())
    }

    fn escape(&mut self) -> FreeTextResult {
        match self.scan.char() {
            Some(ch) => match ch {
                'a' => self.buf.push('\x07'),
                'b' => self.buf.push('\x08'),
                'n' => self.buf.push('\n'),
                'r' => self.buf.push('\r'),
                't' => self.buf.push('\t'),
                'x' | 'X' => self.hex()?,
                '"' | '\\' | '|' => self.buf.push(ch),
                _ if ch.is_ascii_whitespace() => {
                    // NOTE: \<whitespace> may be a line-continuation, but we
                    // won't know until we're done lexing this string.
                    self.possible_line_cont_idx = Some(self.buf.len());
                    self.buf.push(ch);
                }
                _ => return Err(self.policy.escape_invalid(self.start, ch)),
            },
            None => {
                // NOTE: \EOL is a line continuation, mark end of buffer
                self.possible_line_cont_idx = Some(self.buf.len());
            }
        };
        Ok(())
    }

    fn hex(&mut self) -> FreeTextResult {
        let start = self.scan.pos();
        self.scan.end_of_word();
        match self.scan.char_if_eq(';') {
            Some(idx) => {
                let rest = self.scan.lexeme(start..idx);
                match parse_char_hex(rest) {
                    HexParse::Invalid => return Err(self.policy.hex_invalid(self.start)),
                    HexParse::Unexpected => return Err(self.policy.hex_expected(self.start)),
                    HexParse::Valid(ch) => self.buf.push(ch),
                }
            }
            None => return Err(self.policy.hex_unterminated(self.start)),
        };
        Ok(())
    }

    fn terminated(self) -> TokenKind {
        self.policy.terminated(self.buf)
    }

    fn unterminated(self) -> TokenKind {
        self.policy
            .unterminated(self.buf, self.possible_line_cont_idx)
    }
}

pub(super) trait FreeTextPolicy {
    const TERMINATOR: char;
    fn prelude(&self, scan: &mut Scanner<'_>);
    fn escape_invalid(&self, start: usize, ch: char) -> TokenErrorKind;
    fn hex_expected(&self, start: usize) -> TokenErrorKind;
    fn hex_invalid(&self, start: usize) -> TokenErrorKind;
    fn hex_unterminated(&self, start: usize) -> TokenErrorKind;
    fn terminated(&self, buf: String) -> TokenKind;
    fn unterminated(&self, buf: String, line_cont_idx: Option<usize>) -> TokenKind;
}

type FreeTextResult = Result<(), TokenErrorKind>;

enum HexParse {
    Invalid,
    Unexpected,
    Valid(char),
}

fn parse_char_hex(txt: &str) -> HexParse {
    // NOTE: don't allow leading sign, which u32::from_str_radix accepts
    if txt.starts_with('+') {
        HexParse::Unexpected
    } else {
        u32::from_str_radix(txt, 16).map_or(HexParse::Unexpected, |hex| {
            char::from_u32(hex).map_or(HexParse::Invalid, HexParse::Valid)
        })
    }
}
