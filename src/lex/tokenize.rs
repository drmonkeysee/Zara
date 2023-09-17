mod extract;
mod scan;
mod state;
#[cfg(test)]
mod tests;

use self::{
    extract::{TokenExtract, TokenExtractResult},
    scan::{ScanItem, Scanner},
    state::BlockComment,
};
use crate::{
    lex::tokens::{TokenContinuation, TokenErrorKind, TokenKind, TokenResult},
    literal::Literal,
};

pub(super) struct TokenStream<'a> {
    cont: Option<TokenContinuation>,
    scan: Scanner<'a>,
}

impl<'a> TokenStream<'a> {
    pub(super) fn new(textline: &'a str, cont: Option<TokenContinuation>) -> Self {
        Self {
            cont,
            scan: Scanner::new(textline),
        }
    }

    fn token(&mut self) -> Option<IterItem<'a>> {
        self.scan.next_token().map(|item| {
            Tokenizer {
                scan: &mut self.scan,
                start: item,
            }
            .extract()
            .build()
        })
    }

    fn continuation(&mut self, cont: TokenContinuation) -> Option<IterItem<'a>> {
        Some(
            Continuation {
                cont,
                scan: &mut self.scan,
            }
            .extract()
            .build(),
        )
    }
}

impl Iterator for TokenStream<'_> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.cont.take() {
            Some(c) => self.continuation(c),
            None => self.token(),
        }
    }
}

type IterItem<'a> = <TokenStream<'a> as Iterator>::Item;

struct Tokenizer<'me, 'str> {
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

impl<'me, 'str> Tokenizer<'me, 'str> {
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
                '(' => Ok(TokenKind::ParenLeft),
                ')' => Ok(TokenKind::ParenRight),
                '\'' => Ok(TokenKind::Quote),
                '`' => Ok(TokenKind::Quasiquote),
                ';' => self.comment(),
                '#' => self.hashtag(),
                '.' => self.period(),
                ',' => self.unquote(),
                _ => self.not_implemented(),
            },
            self.scan.pos(),
        )
    }

    fn comment(&mut self) -> TokenExtractResult {
        self.scan.end_of_line();
        Ok(TokenKind::Comment)
    }

    fn hashtag(&mut self) -> TokenExtractResult {
        match self.scan.char_if_not_token_boundary() {
            Some(ch) => self.hashliteral(ch),
            None => self.hashcomment(),
        }
    }

    fn period(&mut self) -> TokenExtractResult {
        let rest = self.scan.rest_of_token();
        if rest.is_empty() {
            Ok(TokenKind::PairJoiner)
        } else {
            // TODO: jump to number tokenization with period and next char?
            self.not_implemented()
        }
    }

    fn unquote(&mut self) -> TokenExtractResult {
        Ok(self
            .scan
            .char_if_eq('@')
            .map_or(TokenKind::Unquote, |_| TokenKind::UnquoteSplice))
    }

    fn not_implemented(&mut self) -> TokenExtractResult {
        let start = self.start.0;
        let end = self.scan.end_of_token();
        Err(TokenErrorKind::Unimplemented(
            self.scan.lexeme(start..end).to_owned(),
        ))
    }

    fn hashliteral(&mut self, ch: char) -> TokenExtractResult {
        match ch {
            '(' => Ok(TokenKind::Vector),
            'f' | 'F' => self.boolean(false),
            't' | 'T' => self.boolean(true),
            'u' | 'U' => self.bytevector(),
            '\\' => self.character(),
            '!' => self.directive(),
            _ => {
                self.scan.end_of_token();
                Err(TokenErrorKind::HashInvalid)
            }
        }
    }

    fn hashcomment(&mut self) -> TokenExtractResult {
        self.scan
            .char_if_eq(';')
            .map_or_else(|| self.blockcomment(), |_| Ok(TokenKind::CommentDatum))
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
            .char_if_not_delimiter()
            .filter(|&ch| ch == '8')
            .and_then(|_| {
                self.scan
                    .char_if_not_token_boundary()
                    .filter(|&ch| ch == '(')
            })
            .ok_or(TokenErrorKind::ByteVectorExpected)
            .map(|_| TokenKind::ByteVector)
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

    fn directive(&mut self) -> TokenExtractResult {
        match self.scan.rest_of_token().to_ascii_lowercase().as_str() {
            "fold-case" => Ok(TokenKind::DirectiveCase(true)),
            "no-fold-case" => Ok(TokenKind::DirectiveCase(false)),
            "" => Err(TokenErrorKind::DirectiveExpected),
            _ => Err(TokenErrorKind::DirectiveInvalid),
        }
    }

    fn blockcomment(&mut self) -> TokenExtractResult {
        self.scan
            .char_if_eq('|')
            .ok_or(TokenErrorKind::HashUnterminated)
            .map(|_| BlockComment::new(self.scan).consume())
    }
}

struct Continuation<'me, 'str> {
    cont: TokenContinuation,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str> Continuation<'me, 'str> {
    fn extract(mut self) -> TokenExtract {
        let result = Ok(self.consume());
        TokenExtract {
            start: 0,
            end: self.scan.pos(),
            result,
        }
    }

    fn consume(&mut self) -> TokenKind {
        match self.cont {
            TokenContinuation::BlockComment(depth) => {
                BlockComment::cont(depth, &mut self.scan).consume()
            }
        }
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
