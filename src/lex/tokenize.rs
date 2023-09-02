mod extract;
mod scan;
#[cfg(test)]
mod tests;

use self::{
    extract::{TokenExtract, TokenExtractResult},
    scan::{ScanItem, Scanner},
};
use crate::{
    lex::tokens::{TokenErrorKind, TokenKind, TokenResult},
    literal::Literal,
};

pub(super) struct TokenStream<'a> {
    scan: Scanner<'a>,
}

impl<'a> TokenStream<'a> {
    pub(super) fn new(textline: &'a str) -> Self {
        Self {
            scan: Scanner::new(textline),
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.scan
            .next_token()
            .map(|item| Tokenizer::start(item, &mut self.scan).extract().build())
    }
}

struct Tokenizer<'me, 'str> {
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

impl<'me, 'str> Tokenizer<'me, 'str> {
    fn start(start: ScanItem, scan: &'me mut Scanner<'str>) -> Self {
        Self { start, scan }
    }

    fn extract(mut self) -> TokenExtract {
        let (result, end) = self.scan();
        TokenExtract {
            start: self.start.0,
            end,
            result,
        }
    }

    fn scan(&mut self) -> (TokenExtractResult, usize) {
        (
            match self.start.1 {
                '#' => self.hashtag(),
                '(' => Ok(TokenKind::ParenLeft),
                ')' => Ok(TokenKind::ParenRight),
                _ => self.not_implemented(),
            },
            self.scan.pos(),
        )
    }

    fn hashtag(&mut self) -> TokenExtractResult {
        self.scan
            .trailing_non_delimiter()
            .map_or(Err(TokenErrorKind::HashUnterminated), |ch| match ch {
                'f' | 'F' => self.boolean(false),
                't' | 'T' => self.boolean(true),
                'u' | 'U' => self.bytevector(),
                '\\' => self.character(),
                '(' => Ok(TokenKind::VectorOpen),
                _ => {
                    self.scan.end_of_token();
                    Err(TokenErrorKind::HashInvalid)
                }
            })
    }

    fn boolean(&mut self, val: bool) -> TokenExtractResult {
        let rest = self.scan.rest_of_token();
        if rest.is_empty() || rest.eq_ignore_ascii_case(if val { "rue" } else { "alse" }) {
            Ok(TokenKind::Literal(Literal::Boolean(val)))
        } else {
            Err(TokenErrorKind::BooleanExpected(val))
        }
    }

    fn bytevector(&mut self) -> TokenExtractResult {
        self.scan
            .trailing_non_delimiter()
            .filter(|&ch| ch == '8')
            .and_then(|_| self.scan.trailing_non_delimiter().filter(|&ch| ch == '('))
            .ok_or(TokenErrorKind::ByteVectorExpected)
            .map(|_| TokenKind::ByteVectorOpen)
    }

    fn character(&mut self) -> TokenExtractResult {
        self.scan
            .char()
            .map_or(Ok(TokenKind::Literal(Literal::Character('\n'))), |ch| {
                if ch.is_ascii_whitespace() {
                    Ok(TokenKind::Literal(Literal::Character(ch)))
                } else {
                    let rest = self.scan.rest_of_token();
                    if rest.is_empty() {
                        Ok(TokenKind::Literal(Literal::Character(ch)))
                    } else if let 'x' | 'X' = ch {
                        char_hex(rest)
                    } else {
                        char_name(ch, rest)
                    }
                }
            })
    }

    fn not_implemented(&mut self) -> TokenExtractResult {
        let start = self.start.0;
        let end = self.scan.end_of_token();
        Err(TokenErrorKind::Unimplemented(
            self.scan.lexeme(start..end).to_owned(),
        ))
    }
}

fn char_hex(rest: &str) -> TokenExtractResult {
    // NOTE: don't allow leading sign, which u32::from_str_radix accepts
    if rest.starts_with('+') {
        Err(TokenErrorKind::CharacterExpectedHex)
    } else {
        u32::from_str_radix(rest, 16).map_or(Err(TokenErrorKind::CharacterExpectedHex), |hex| {
            char::from_u32(hex)
                .ok_or(TokenErrorKind::CharacterInvalidHex)
                .map(|ch| TokenKind::Literal(Literal::Character(ch)))
        })
    }
}

fn char_name(ch: char, rest: &str) -> TokenExtractResult {
    match (ch, rest) {
        ('a', "larm") => char_lit('\x07'),
        ('b', "ackspace") => char_lit('\x08'),
        ('d', "elete") => char_lit('\x7f'),
        ('e', "scape") => char_lit('\x1b'),
        ('n', "ewline") => char_lit('\n'),
        ('n', "ull") => char_lit('\0'),
        ('r', "eturn") => char_lit('\r'),
        ('s', "pace") => char_lit(' '),
        ('t', "ab") => char_lit('\t'),
        _ => Err(TokenErrorKind::CharacterExpected),
    }
}

fn char_lit(ch: char) -> TokenExtractResult {
    Ok(TokenKind::Literal(Literal::Character(ch)))
}
