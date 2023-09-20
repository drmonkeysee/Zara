use crate::{
    lex::{
        token::{TokenErrorKind, TokenKind},
        tokenize::{extract::TokenExtractResult, scan::Scanner},
    },
    literal::Literal,
};

pub(super) struct Hashtag<'me, 'str> {
    pub(super) scan: &'me mut Scanner<'str>,
}

impl<'me, 'str> Hashtag<'me, 'str> {
    pub(super) fn scan(&mut self) -> TokenExtractResult {
        match self.scan.char_if_not_token_boundary() {
            Some(ch) => self.literal(ch),
            None => self.comment(),
        }
    }

    fn literal(&mut self, ch: char) -> TokenExtractResult {
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

    fn comment(&mut self) -> TokenExtractResult {
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

pub(super) struct StringLit<'me, 'str> {
    pub(super) scan: &'me mut Scanner<'str>,
}

impl<'me, 'str> StringLit<'me, 'str> {
    pub(super) fn scan(&mut self) -> TokenExtractResult {
        let mut buf = String::new();
        while let Some(ch) = self.scan.char() {
            if ch == '"' {
                return Ok(TokenKind::Literal(Literal::String(buf)));
            } else {
                buf.push(ch);
            }
        }
        todo!();
    }
}

pub(super) struct BlockComment<'me, 'str> {
    cont: bool,
    depth: usize,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str> BlockComment<'me, 'str> {
    pub(super) fn new(scan: &'me mut Scanner<'str>) -> Self {
        Self {
            cont: false,
            depth: 0,
            scan,
        }
    }

    pub(super) fn cont(depth: usize, scan: &'me mut Scanner<'str>) -> Self {
        Self {
            cont: true,
            depth,
            scan,
        }
    }

    pub(super) fn consume(&mut self) -> TokenKind {
        while !self.scan.consumed() {
            if let Some((_, ch)) = self.scan.find_any_char(&['|', '#']) {
                if self.end_block(ch) {
                    if self.depth == 0 {
                        return self.end_kind();
                    } else {
                        self.depth -= 1;
                    }
                } else if self.new_block(ch) {
                    self.depth += 1;
                }
            }
        }
        self.continuation_kind()
    }

    fn end_block(&mut self, ch: char) -> bool {
        ch == '|' && self.scan.char_if_eq('#').is_some()
    }

    fn new_block(&mut self, ch: char) -> bool {
        ch == '#' && self.scan.char_if_eq('|').is_some()
    }

    fn continuation_kind(&self) -> TokenKind {
        if self.cont {
            TokenKind::CommentBlockFragment(self.depth)
        } else {
            TokenKind::CommentBlockBegin(self.depth)
        }
    }

    fn end_kind(&self) -> TokenKind {
        if self.cont {
            TokenKind::CommentBlockEnd
        } else {
            TokenKind::CommentBlock
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

// NOTE: state functionality is covered by Tokenizer and Continuation tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blockcomment_new() {
        let mut s = Scanner::new("");

        let target = BlockComment::new(&mut s);

        assert_eq!(target.cont, false);
        assert_eq!(target.depth, 0);
    }

    #[test]
    fn blockcomment_cont() {
        let mut s = Scanner::new("");

        let target = BlockComment::cont(3, &mut s);

        assert_eq!(target.cont, true);
        assert_eq!(target.depth, 3);
    }
}
