mod hashtag;
mod identifier;
mod numeric;
mod string;

use self::numeric::Sign;
// NOTE: re-export for parent module
pub(super) use self::{
    hashtag::{BlockComment, Hashtag},
    identifier::{Identifier, PeriodIdentifier, VerbatimIdentifer},
    numeric::{Decimal, Numeric, Radix},
    string::StringLiteral,
};
use super::{extract::TokenExtractResult, scan::Scanner};
use crate::lex::token::{TokenErrorKind, TokenKind};

pub(super) struct FreeText<'me, 'str, P> {
    buf: String,
    policy: P,
    possible_line_cont_idx: Option<usize>,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str, P: FreeTextPolicy> FreeText<'me, 'str, P> {
    fn init(scan: &'me mut Scanner<'str>, policy: P) -> Self {
        Self {
            buf: String::new(),
            policy,
            possible_line_cont_idx: None,
            scan,
        }
    }

    pub(super) fn scan(mut self) -> TokenExtractResult {
        self.policy.prelude(self.scan);
        while let Some((idx, ch)) = self.scan.next() {
            match ch {
                '\\' => self.escape(idx)?,
                _ if ch == P::TERMINATOR => return Ok(self.terminated()),
                _ => self.buf.push(ch),
            }
        }
        Ok(self.unterminated())
    }

    fn escape(&mut self, start: usize) -> FreeTextResult {
        match self.scan.char() {
            Some(ch) => match ch {
                'a' => self.buf.push('\x07'),
                'b' => self.buf.push('\x08'),
                'n' => self.buf.push('\n'),
                'r' => self.buf.push('\r'),
                't' => self.buf.push('\t'),
                'x' | 'X' => self.hex(start)?,
                '"' | '\\' | '|' => self.buf.push(ch),
                _ if ch.is_ascii_whitespace() => {
                    // NOTE: \<whitespace> may be a line-continuation, but we
                    // won't know until we're done lexing this string.
                    self.possible_line_cont_idx = Some(self.buf.len());
                    self.buf.push(ch);
                }
                _ => return Err(self.policy.escape_invalid(start, ch)),
            },
            None => {
                // NOTE: \EOL is a line continuation, mark end of buffer
                self.possible_line_cont_idx = Some(self.buf.len());
            }
        };
        Ok(())
    }

    fn hex(&mut self, start: usize) -> FreeTextResult {
        let pos = self.scan.pos();
        self.scan.end_of_word();
        match self.scan.char_if_eq(';') {
            Some(idx) => {
                let rest = self.scan.lexeme(pos..idx);
                match parse_char_hex(rest) {
                    HexParse::Invalid => return Err(self.policy.hex_invalid(start)),
                    HexParse::Unexpected => return Err(self.policy.hex_expected(start)),
                    HexParse::Valid(ch) => self.buf.push(ch),
                }
            }
            None => return Err(self.policy.hex_unterminated(start)),
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

// TODO: handle imaginary inf/nan and inexact
fn numeric_identifier(txt: &str) -> Option<TokenKind> {
    let sign = if txt.starts_with('+') {
        Some(Sign::Positive)
    } else if txt.starts_with('-') {
        Some(Sign::Negative)
    } else {
        None
    };
    if let Some(sign) = sign {
        if let Some(label) = txt.get(1..) {
            return match label.to_ascii_lowercase().as_str() {
                "i" => Some(numeric::imaginary(sign)),
                "inf.0" => Some(numeric::infinity(sign)),
                "nan.0" => Some(numeric::nan()),
                _ => None,
            };
        }
    };
    None
}
