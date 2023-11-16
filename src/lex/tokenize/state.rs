use super::{extract::TokenExtractResult, scan::Scanner};
use crate::{
    lex::token::{TokenErrorKind, TokenKind},
    literal::Literal,
};
use std::marker::PhantomData;

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

pub(super) struct StringLiteral<'me, 'str, P, T> {
    buf: String,
    policy: PhantomData<P>,
    possible_line_cont_idx: Option<usize>,
    scan: &'me mut Scanner<'str>,
    start: usize,
    token_policy: PhantomData<T>,
}

impl<'me, 'str, P: FreeTextPolicy, T: FreeTextTokenPolicy> StringLiteral<'me, 'str, P, T> {
    fn init(scan: &'me mut Scanner<'str>) -> Self {
        Self {
            buf: String::new(),
            policy: PhantomData,
            possible_line_cont_idx: None,
            scan,
            start: 0,
            token_policy: PhantomData,
        }
    }

    pub(super) fn scan(mut self) -> TokenExtractResult {
        T::prelude(self.scan);
        while let Some((idx, ch)) = self.scan.next() {
            self.start = idx;
            match ch {
                '\\' => self.escape()?,
                _ if ch == P::TERMINATOR => return self.terminated(),
                _ => self.buf.push(ch),
            }
        }
        self.unterminated()
    }

    fn escape(&mut self) -> StringLiteralResult {
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
                _ => return Err(P::escape_invalid(self.start, ch)),
            },
            None => {
                // NOTE: \EOL is a line continuation, mark end of buffer
                self.possible_line_cont_idx = Some(self.buf.len())
            }
        };
        Ok(())
    }

    fn hex(&mut self) -> StringLiteralResult {
        let start = self.scan.pos();
        self.scan.end_of_word();
        match self.scan.char_if_eq(';') {
            Some(idx) => {
                let rest = self.scan.lexeme(start..idx);
                match parse_char_hex(rest) {
                    HexParse::Invalid => return Err(P::hex_invalid(self.start)),
                    HexParse::Unexpected => return Err(P::hex_expected(self.start)),
                    HexParse::Valid(ch) => self.buf.push(ch),
                }
            }
            None => return Err(P::hex_unterminated(self.start)),
        };
        Ok(())
    }

    fn terminated(self) -> TokenExtractResult {
        Ok(T::terminated(self.buf))
    }

    fn unterminated(self) -> TokenExtractResult {
        if let Some(idx) = self.possible_line_cont_idx {
            let (lead, trail) = self.buf.split_at(idx);
            if trail.trim().is_empty() {
                return Ok(T::unterminated(lead.to_owned(), true));
            }
        }
        Ok(T::unterminated(self.buf, false))
    }
}

impl<'me, 'str> StringLiteral<'me, 'str, StringPolicy, StartStringPolicy> {
    pub(super) fn new(scan: &'me mut Scanner<'str>) -> Self {
        Self::init(scan)
    }
}

impl<'me, 'str> StringLiteral<'me, 'str, StringPolicy, DiscardStringPolicy> {
    pub(super) fn cleanup(scan: &'me mut Scanner<'str>) -> Self {
        Self::init(scan)
    }
}

impl<'me, 'str> StringLiteral<'me, 'str, StringPolicy, ContinueStringPolicy> {
    pub(super) fn cont(scan: &'me mut Scanner<'str>) -> Self {
        Self::init(scan)
    }
}

impl<'me, 'str> StringLiteral<'me, 'str, StringPolicy, LineContinueStringPolicy> {
    pub(super) fn line_cont(scan: &'me mut Scanner<'str>) -> Self {
        Self::init(scan)
    }
}

pub(super) trait FreeTextTokenPolicy {
    fn prelude<'me, 'str>(_scan: &'me mut Scanner<'str>) {
        // NOTE: do nothing by default
    }

    fn terminated(buf: String) -> TokenKind {
        TokenKind::StringEnd(buf)
    }

    fn unterminated(buf: String, line_cont: bool) -> TokenKind {
        TokenKind::StringFragment(buf, line_cont)
    }
}

pub(super) struct StartStringPolicy;

impl FreeTextTokenPolicy for StartStringPolicy {
    fn terminated(buf: String) -> TokenKind {
        TokenKind::Literal(Literal::String(buf))
    }

    fn unterminated(buf: String, line_cont: bool) -> TokenKind {
        TokenKind::StringBegin(buf, line_cont)
    }
}

pub(super) struct DiscardStringPolicy;

impl FreeTextTokenPolicy for DiscardStringPolicy {
    fn terminated(_buf: String) -> TokenKind {
        TokenKind::StringDiscard
    }

    fn unterminated(buf: String, _line_cont: bool) -> TokenKind {
        Self::terminated(buf)
    }
}

pub(super) struct ContinueStringPolicy;

impl FreeTextTokenPolicy for ContinueStringPolicy {}

pub(super) struct LineContinueStringPolicy;

impl FreeTextTokenPolicy for LineContinueStringPolicy {
    fn prelude<'me, 'str>(scan: &'me mut Scanner<'str>) {
        scan.skip_whitespace();
    }
}

pub(super) trait FreeTextPolicy {
    const TERMINATOR: char;
    fn escape_invalid(start: usize, ch: char) -> TokenErrorKind;
    fn hex_expected(start: usize) -> TokenErrorKind;
    fn hex_invalid(start: usize) -> TokenErrorKind;
    fn hex_unterminated(start: usize) -> TokenErrorKind;
}

pub(super) struct StringPolicy;

impl FreeTextPolicy for StringPolicy {
    const TERMINATOR: char = '"';

    fn escape_invalid(start: usize, ch: char) -> TokenErrorKind {
        TokenErrorKind::StringEscapeInvalid(start, ch)
    }

    fn hex_expected(start: usize) -> TokenErrorKind {
        TokenErrorKind::StringExpectedHex(start)
    }

    fn hex_invalid(start: usize) -> TokenErrorKind {
        TokenErrorKind::StringInvalidHex(start)
    }

    fn hex_unterminated(start: usize) -> TokenErrorKind {
        TokenErrorKind::StringUnterminatedHex(start)
    }
}

pub(super) struct Identifier<'me, 'str> {
    buf: String,
    peculiar_state: PeculiarState,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str> Identifier<'me, 'str> {
    pub(super) fn new(scan: &'me mut Scanner<'str>) -> Self {
        Self {
            buf: String::new(),
            peculiar_state: PeculiarState::Unspecified,
            scan,
        }
    }

    pub(super) fn scan(mut self, first: char) -> TokenExtractResult {
        if first == '|' {
            self.not_implemented(first) // TODO: verbatim identifier
        } else if is_id_peculiar_initial(first) {
            self.peculiar(first)
        } else if is_id_initial(first) {
            self.standard(first)
        } else {
            self.invalid(first)
        }
    }

    fn standard(mut self, first: char) -> TokenExtractResult {
        self.buf.push(first);
        while let Some(ch) = self.scan.char_if_not_delimiter() {
            if is_id_standard(ch) {
                self.buf.push(ch);
            } else {
                return self.invalid(ch);
            }
        }
        Ok(TokenKind::Identifier(self.buf))
    }

    fn peculiar(mut self, ch: char) -> TokenExtractResult {
        self.push_peculiar(ch);
        let next_ch = self.scan.char_if_not_delimiter();
        self.continue_peculiar(next_ch)
    }

    fn continue_peculiar(mut self, next_ch: Option<char>) -> TokenExtractResult {
        match next_ch {
            Some(ch) => {
                // TODO: this should only be 0..9, change call if is_id_digit is expanded
                if is_id_digit(ch) {
                    match self.peculiar_state {
                        PeculiarState::DefiniteIdentifier => self.standard(ch),
                        _ => self.not_implemented(ch), // TODO: parse as number
                    }
                } else if is_id_peculiar_initial(ch) {
                    self.peculiar(ch)
                } else if is_id_initial(ch) {
                    self.standard(ch)
                } else {
                    self.invalid(ch)
                }
            }
            None => {
                // NOTE: a single '.' is invalid but Tokenizer handles '.'
                // before attempting Identifier so this case never happens.
                Ok(TokenKind::Identifier(self.buf))
            }
        }
    }

    fn invalid(&mut self, ch: char) -> TokenExtractResult {
        self.scan.end_of_token();
        Err(TokenErrorKind::IdentifierInvalid(ch))
    }

    fn push_peculiar(&mut self, ch: char) {
        self.buf.push(ch);
        // NOTE: only 3 cases: + | - | .
        self.peculiar_state = match ch {
            '+' | '-' => match self.peculiar_state {
                PeculiarState::Unspecified => PeculiarState::MaybeSignedNumber,
                _ => PeculiarState::DefiniteIdentifier,
            },
            _ => match self.peculiar_state {
                PeculiarState::Unspecified => PeculiarState::MaybeFloat,
                PeculiarState::MaybeSignedNumber => PeculiarState::MaybeSignedFloat,
                _ => PeculiarState::DefiniteIdentifier,
            },
        }
    }

    fn not_implemented(mut self, ch: char) -> TokenExtractResult {
        self.buf.push(ch);
        self.buf.push_str(self.scan.rest_of_token());
        Err(TokenErrorKind::Unimplemented(self.buf))
    }
}

pub(super) struct PeriodIdentifier<'me, 'str>(Identifier<'me, 'str>);

impl<'me, 'str> PeriodIdentifier<'me, 'str> {
    pub(super) fn new(scan: &'me mut Scanner<'str>) -> Self {
        let mut me = Self(Identifier::new(scan));
        me.0.push_peculiar('.');
        me
    }

    pub(super) fn scan(self, first: char) -> TokenExtractResult {
        self.0.continue_peculiar(Some(first))
    }
}

pub(super) struct BlockComment<'me, 'str, T> {
    depth: usize,
    mode: T,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str, T: BlockCommentMode> BlockComment<'me, 'str, T> {
    pub(super) fn consume(&mut self) -> TokenKind {
        while !self.scan.consumed() {
            if let Some((_, ch)) = self.scan.find_any_char(&['|', '#']) {
                if self.end_block(ch) {
                    if self.depth == 0 {
                        return self.mode.terminated();
                    } else {
                        self.depth -= 1;
                    }
                } else if self.new_block(ch) {
                    self.depth += 1;
                }
            }
        }
        self.mode.unterminated(self.depth)
    }

    fn end_block(&mut self, ch: char) -> bool {
        ch == '|' && self.scan.char_if_eq('#').is_some()
    }

    fn new_block(&mut self, ch: char) -> bool {
        ch == '#' && self.scan.char_if_eq('|').is_some()
    }
}

impl<'me, 'str> BlockComment<'me, 'str, ContinueBlockComment> {
    pub(super) fn cont(depth: usize, scan: &'me mut Scanner<'str>) -> Self {
        Self {
            depth,
            mode: ContinueBlockComment,
            scan,
        }
    }
}

impl<'me, 'str> BlockComment<'me, 'str, StartBlockComment> {
    fn new(scan: &'me mut Scanner<'str>) -> Self {
        Self {
            depth: 0,
            mode: StartBlockComment,
            scan,
        }
    }
}

pub(super) trait BlockCommentMode {
    fn terminated(&self) -> TokenKind;
    fn unterminated(&self, depth: usize) -> TokenKind;
}

pub(super) struct ContinueBlockComment;

impl BlockCommentMode for ContinueBlockComment {
    fn terminated(&self) -> TokenKind {
        TokenKind::CommentBlockEnd
    }

    fn unterminated(&self, depth: usize) -> TokenKind {
        TokenKind::CommentBlockFragment(depth)
    }
}

type StringLiteralResult = Result<(), TokenErrorKind>;

enum PeculiarState {
    DefiniteIdentifier,
    MaybeFloat,
    MaybeSignedFloat,
    MaybeSignedNumber,
    Unspecified,
}

struct StartBlockComment;

impl BlockCommentMode for StartBlockComment {
    fn terminated(&self) -> TokenKind {
        TokenKind::CommentBlock
    }

    fn unterminated(&self, depth: usize) -> TokenKind {
        TokenKind::CommentBlockBegin(depth)
    }
}

enum HexParse {
    Invalid,
    Unexpected,
    Valid(char),
}

fn char_hex(rest: &str) -> TokenExtractResult {
    match parse_char_hex(rest) {
        HexParse::Invalid => Err(TokenErrorKind::CharacterInvalidHex),
        HexParse::Unexpected => Err(TokenErrorKind::CharacterExpectedHex),
        HexParse::Valid(ch) => Ok(TokenKind::Literal(Literal::Character(ch))),
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

fn is_id_initial(ch: char) -> bool {
    // TODO: disallow Mc, Me, Nd
    is_id_letter(ch) || is_id_special_initial(ch)
}

fn is_id_standard(ch: char) -> bool {
    is_id_initial(ch) || is_id_digit(ch) || is_id_peculiar_initial(ch)
}

fn is_id_letter(ch: char) -> bool {
    // TODO: support Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So, Co, U+200C, U+200D
    ch.is_ascii_alphabetic()
}

fn is_id_digit(ch: char) -> bool {
    // TODO: support Nd, Nl, No
    ch.is_ascii_digit()
}

fn is_id_special_initial(ch: char) -> bool {
    matches!(
        ch,
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '_' | '~'
    )
}

fn is_id_peculiar_initial(ch: char) -> bool {
    matches!(ch, '+' | '-' | '.')
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
