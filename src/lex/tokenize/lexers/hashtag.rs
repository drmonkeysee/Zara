use super::{Exactness, HexParse, Identifier, RadixNumber};
use crate::{
    constant::Constant,
    lex::{
        TokenKind,
        token::TokenErrorKind,
        tokenize::{TokenExtractResult, scan::Scanner},
    },
    number::{Binary, Hexadecimal, Octal, Radix},
};

const EXACTL: char = 'e';
const EXACTU: char = EXACTL.to_ascii_uppercase();
const INEXACTL: char = 'i';
const INEXACTU: char = INEXACTL.to_ascii_uppercase();

pub(in crate::lex::tokenize) struct Hashtag<'me, 'txt> {
    pub(in crate::lex::tokenize) scanner: &'me mut Scanner<'txt>,
}

impl Hashtag<'_, '_> {
    pub(in crate::lex::tokenize) fn scan(&mut self) -> TokenExtractResult {
        match self.scanner.char_if_not_token_boundary() {
            Some(ch) => self.constant(ch),
            None => self.comment(),
        }
    }

    fn constant(&mut self, ch: char) -> TokenExtractResult {
        match ch {
            '(' => Ok(TokenKind::Vector),
            EXACTL | EXACTU => self.exactness(Exactness::Exact),
            'f' | 'F' => self.boolean(false),
            INEXACTL | INEXACTU => self.exactness(Exactness::Inexact),
            't' | 'T' => self.boolean(true),
            'u' | 'U' => self.bytevector(),
            '\\' => self.character(),
            '!' => self.directive(),
            _ => self.number(ch),
        }
    }

    fn comment(&mut self) -> TokenExtractResult {
        self.scanner
            .char_if_eq(';')
            .map_or_else(|| self.blockcomment(), |_| Ok(TokenKind::CommentDatum))
    }

    fn boolean(&mut self, val: bool) -> TokenExtractResult {
        let rest = self.scanner.rest_of_token();
        if rest.is_empty() || rest.eq_ignore_ascii_case(if val { "rue" } else { "alse" }) {
            Ok(TokenKind::Constant(Constant::Boolean(val)))
        } else {
            Err(TokenErrorKind::BooleanExpected(val))
        }
    }

    fn bytevector(&mut self) -> TokenExtractResult {
        self.scanner
            .char_if_not_delimiter()
            .filter(|&ch| ch == '8')
            .and_then(|_| {
                self.scanner
                    .char_if_not_token_boundary()
                    .filter(|&ch| ch == '(')
            })
            .ok_or(TokenErrorKind::ByteVectorExpected)
            .map(|_| TokenKind::ByteVector)
    }

    fn character(&mut self) -> TokenExtractResult {
        self.scanner
            .char()
            .map_or(Ok(TokenKind::Constant(Constant::Character('\n'))), |ch| {
                if ch.is_ascii_whitespace() {
                    Ok(TokenKind::Constant(Constant::Character(ch)))
                } else {
                    let rest = self.scanner.rest_of_token();
                    if rest.is_empty() {
                        Ok(TokenKind::Constant(Constant::Character(ch)))
                    } else if let 'x' | 'X' = ch {
                        char_hex(rest)
                    } else {
                        char_name(ch, rest)
                    }
                }
            })
    }

    fn directive(&mut self) -> TokenExtractResult {
        match self.scanner.rest_of_token().to_ascii_lowercase().as_str() {
            "fold-case" => Ok(TokenKind::DirectiveCase(true)),
            "no-fold-case" => Ok(TokenKind::DirectiveCase(false)),
            "" => Err(TokenErrorKind::DirectiveExpected),
            _ => Err(TokenErrorKind::DirectiveInvalid),
        }
    }

    fn blockcomment(&mut self) -> TokenExtractResult {
        self.scanner
            .char_if_eq('|')
            .ok_or(TokenErrorKind::HashUnterminated)
            .map(|_| BlockComment::new(self.scanner).consume())
    }

    fn exactness(&mut self, exactness: Exactness) -> TokenExtractResult {
        let curr = self.scanner.pos();
        if self.scanner.char_if_eq('#').is_some() {
            let num = NumberKind::select_or(
                TokenErrorKind::RadixExpected { at: curr },
                self.scanner,
                None,
            )?;
            num.scan(self.scanner, Some(exactness))
        } else {
            NumberKind::Decimal.scan(self.scanner, Some(exactness))
        }
    }

    fn number(&mut self, ch: char) -> TokenExtractResult {
        let num = NumberKind::select_or(TokenErrorKind::HashInvalid, self.scanner, Some(ch))?;
        let exactness = self.check_exactness()?;
        num.scan(self.scanner, exactness)
    }

    fn check_exactness(&mut self) -> Result<Option<Exactness>, TokenErrorKind> {
        let curr = self.scanner.pos();
        self.scanner.char_if_eq('#').map_or(Ok(None), |_| {
            match self.scanner.char_if_not_delimiter() {
                Some(EXACTL | EXACTU) => Ok(Some(Exactness::Exact)),
                Some(INEXACTL | INEXACTU) => Ok(Some(Exactness::Inexact)),
                _ => Err(TokenErrorKind::ExactnessExpected { at: curr }),
            }
        })
    }
}

pub(in crate::lex::tokenize) struct BlockComment<'me, 'txt, P> {
    depth: usize,
    policy: P,
    scanner: &'me mut Scanner<'txt>,
}

impl<P: BlockCommentPolicy> BlockComment<'_, '_, P> {
    pub(in crate::lex::tokenize) fn consume(mut self) -> TokenKind {
        while !self.scanner.consumed() {
            if let Some((_, ch)) = self.scanner.find_any_char(&['|', '#']) {
                if self.end_block(ch) {
                    if self.depth == 0 {
                        return self.policy.terminated();
                    }
                    self.depth -= 1;
                } else if self.new_block(ch) {
                    self.depth += 1;
                }
            }
        }
        self.policy.unterminated(self.depth)
    }

    fn end_block(&mut self, ch: char) -> bool {
        ch == '|' && self.scanner.char_if_eq('#').is_some()
    }

    fn new_block(&mut self, ch: char) -> bool {
        ch == '#' && self.scanner.char_if_eq('|').is_some()
    }
}

impl<'me, 'txt> BlockComment<'me, 'txt, ContinueComment> {
    pub(in crate::lex::tokenize) fn cont(depth: usize, scanner: &'me mut Scanner<'txt>) -> Self {
        Self {
            depth,
            policy: ContinueComment,
            scanner,
        }
    }
}

impl<'me, 'txt> BlockComment<'me, 'txt, StartComment> {
    fn new(scanner: &'me mut Scanner<'txt>) -> Self {
        Self {
            depth: 0,
            policy: StartComment,
            scanner,
        }
    }
}

pub(in crate::lex::tokenize) trait BlockCommentPolicy {
    fn terminated(&self) -> TokenKind;
    fn unterminated(&self, depth: usize) -> TokenKind;
}

pub(in crate::lex::tokenize) struct ContinueComment;

impl BlockCommentPolicy for ContinueComment {
    fn terminated(&self) -> TokenKind {
        TokenKind::CommentBlockEnd
    }

    fn unterminated(&self, depth: usize) -> TokenKind {
        TokenKind::CommentBlockFragment { depth }
    }
}

struct StartComment;

impl BlockCommentPolicy for StartComment {
    fn terminated(&self) -> TokenKind {
        TokenKind::Comment
    }

    fn unterminated(&self, depth: usize) -> TokenKind {
        TokenKind::CommentBlockBegin { depth }
    }
}

enum NumberKind {
    Binary,
    Decimal,
    Hexadecimal,
    Octal,
}

impl NumberKind {
    fn select_or(
        failure: TokenErrorKind,
        scanner: &mut Scanner,
        ch: Option<char>,
    ) -> Result<NumberKind, TokenErrorKind> {
        match ch.or_else(|| scanner.char_if_not_delimiter()) {
            Some('b' | 'B') => Ok(Self::Binary),
            Some('d' | 'D') => Ok(Self::Decimal),
            Some('o' | 'O') => Ok(Self::Octal),
            Some('x' | 'X') => Ok(Self::Hexadecimal),
            _ => Err(failure),
        }
    }

    fn scan(&self, scanner: &mut Scanner, exactness: Option<Exactness>) -> TokenExtractResult {
        match self {
            Self::Binary => radix::<Binary>(scanner, exactness),
            Self::Decimal => decimal(scanner, exactness),
            Self::Hexadecimal => radix::<Hexadecimal>(scanner, exactness),
            Self::Octal => radix::<Octal>(scanner, exactness),
        }
    }
}

fn radix<R: Radix + Clone + Default>(
    scanner: &mut Scanner,
    exactness: Option<Exactness>,
) -> TokenExtractResult {
    RadixNumber::<R>::new(scanner, exactness).scan()
}

fn decimal(scanner: &mut Scanner, exactness: Option<Exactness>) -> TokenExtractResult {
    if let Some(item) = scanner.next_if_not_delimiter() {
        let result = Identifier::with_exactness(scanner, item, exactness).scan();
        match result {
            Err(TokenErrorKind::IdentifierInvalid(_)) => (),
            Ok(TokenKind::Constant(Constant::Number(_)) | TokenKind::Imaginary(_)) | Err(_) => {
                return result;
            }
            _ => (),
        }
    }
    Err(TokenErrorKind::NumberExpected)
}

fn char_hex(rest: &str) -> TokenExtractResult {
    match super::parse_char_hex(rest) {
        HexParse::Invalid => Err(TokenErrorKind::CharacterInvalidHex),
        HexParse::Unexpected => Err(TokenErrorKind::CharacterExpectedHex),
        HexParse::Valid(ch) => Ok(TokenKind::Constant(Constant::Character(ch))),
    }
}

fn char_name(ch: char, rest: &str) -> TokenExtractResult {
    match (ch, rest) {
        ('a', "larm") => Ok(char_lit('\x07')),
        ('b', "ackspace") => Ok(char_lit('\x08')),
        ('d', "elete") => Ok(char_lit('\x7f')),
        ('e', "scape") => Ok(char_lit('\x1b')),
        ('n', "ewline") => Ok(char_lit('\n')),
        ('n', "ull") => Ok(char_lit('\0')),
        ('r', "eturn") => Ok(char_lit('\r')),
        ('s', "pace") => Ok(char_lit(' ')),
        ('t', "ab") => Ok(char_lit('\t')),
        _ => Err(TokenErrorKind::CharacterExpected),
    }
}

fn char_lit(ch: char) -> TokenKind {
    TokenKind::Constant(Constant::Character(ch))
}

// NOTE: state functionality is covered by Tokenizer and Continuation tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blockcomment_new() {
        let mut s = Scanner::new("");

        let target = BlockComment::new(&mut s);

        assert_eq!(target.depth, 0);
    }

    #[test]
    fn blockcomment_cont() {
        let mut s = Scanner::new("");

        let target = BlockComment::cont(3, &mut s);

        assert_eq!(target.depth, 3);
    }
}
