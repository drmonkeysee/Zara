use super::{extract::TokenExtractResult, scan::Scanner};
use crate::{
    lex::token::{TokenErrorKind, TokenKind},
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
            .map(|_| new_block_comment(self.scan).consume())
    }
}

pub(super) struct StringLiteralFactory;

impl StringLiteralFactory {
    pub(super) fn new<'me, 'str>(
        scan: &'me mut Scanner<'str>,
    ) -> StringLiteral<'me, 'str, StartStringLiteral> {
        StringLiteral::init(scan, StartStringLiteral)
    }

    pub(super) fn cleanup<'me, 'str>(
        scan: &'me mut Scanner<'str>,
    ) -> StringLiteral<'me, 'str, DiscardStringLiteral> {
        StringLiteral::init(scan, DiscardStringLiteral)
    }

    pub(super) fn cont<'me, 'str>(
        scan: &'me mut Scanner<'str>,
    ) -> StringLiteral<'me, 'str, ContinueStringLiteral> {
        StringLiteral::init(scan, ContinueStringLiteral)
    }

    pub(super) fn line_cont<'me, 'str>(
        scan: &'me mut Scanner<'str>,
    ) -> StringLiteral<'me, 'str, LineContinueStringLiteral> {
        StringLiteral::init(scan, LineContinueStringLiteral)
    }
}

pub(super) struct StringLiteral<'me, 'str, T> {
    buf: String,
    mode: T,
    possible_line_cont_idx: Option<usize>,
    scan: &'me mut Scanner<'str>,
    start: usize,
}

type StringLiteralResult = Result<(), TokenErrorKind>;

impl<'me, 'str, T: StringLiteralMode> StringLiteral<'me, 'str, T> {
    fn init(scan: &'me mut Scanner<'str>, mode: T) -> Self {
        Self {
            buf: String::new(),
            mode,
            possible_line_cont_idx: None,
            scan,
            start: 0,
        }
    }

    pub(super) fn scan(mut self) -> TokenExtractResult {
        self.mode.prelude(self.scan);
        while let Some((idx, ch)) = self.scan.next() {
            self.start = idx;
            match ch {
                '"' => return self.end_string(),
                '\\' => self.escape()?,
                _ => self.buf.push(ch),
            }
        }
        self.unterminated()
    }

    fn escape(&mut self) -> StringLiteralResult {
        if let Some(ch) = self.scan.char() {
            match ch {
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
                _ => Err(TokenErrorKind::StringEscapeInvalid(self.start, ch))?,
            }
        } else {
            // NOTE: \EOL is a line continuation, mark end of buffer
            self.possible_line_cont_idx = Some(self.buf.len());
        }
        Ok(())
    }

    fn hex(&mut self) -> StringLiteralResult {
        let start = self.scan.pos();
        if let Some(idx) = self.scan.find_char(';') {
            let rest = self.scan.lexeme(start..idx);
            match parse_char_hex(rest) {
                HexParse::Invalid => Err(TokenErrorKind::StringInvalidHex(self.start))?,
                HexParse::Unexpected => Err(TokenErrorKind::StringExpectedHex(self.start))?,
                HexParse::Valid(ch) => self.buf.push(ch),
            }
        } else {
            Err(TokenErrorKind::StringUnterminatedHex(self.start))?
        }
        Ok(())
    }

    fn end_string(self) -> TokenExtractResult {
        Ok(self.mode.terminated(self.buf))
    }

    fn unterminated(self) -> TokenExtractResult {
        if self.mode.allows_line_continuation() {
            if let Some(idx) = self.possible_line_cont_idx {
                let (lead, trail) = self.buf.split_at(idx);
                if trail.trim().is_empty() {
                    return Ok(self.mode.unterminated(lead.to_owned(), true));
                }
            }
        }
        Ok(self.mode.unterminated(self.buf, false))
    }
}

pub(super) trait StringLiteralMode {
    fn allows_line_continuation(&self) -> bool {
        true
    }

    fn prelude<'me, 'str>(&self, _scan: &'me mut Scanner<'str>) {
        // NOTE: do nothing by default
    }

    fn terminated(&self, buf: String) -> TokenKind {
        TokenKind::StringEnd(buf)
    }

    fn unterminated(&self, buf: String, line_cont: bool) -> TokenKind {
        TokenKind::StringFragment(buf, line_cont)
    }
}

pub(super) struct StartStringLiteral;

impl StringLiteralMode for StartStringLiteral {
    fn terminated(&self, buf: String) -> TokenKind {
        TokenKind::Literal(Literal::String(buf))
    }

    fn unterminated(&self, buf: String, line_cont: bool) -> TokenKind {
        TokenKind::StringBegin(buf, line_cont)
    }
}

pub(super) struct DiscardStringLiteral;

impl StringLiteralMode for DiscardStringLiteral {
    fn allows_line_continuation(&self) -> bool {
        false
    }

    fn terminated(&self, _buf: String) -> TokenKind {
        TokenKind::StringDiscard
    }

    fn unterminated(&self, buf: String, _line_cont: bool) -> TokenKind {
        self.terminated(buf)
    }
}

pub(super) struct ContinueStringLiteral;

impl StringLiteralMode for ContinueStringLiteral {}

pub(super) struct LineContinueStringLiteral;

impl StringLiteralMode for LineContinueStringLiteral {
    fn prelude<'me, 'str>(&self, scan: &'me mut Scanner<'str>) {
        scan.skip_whitespace();
    }
}

pub(super) struct Identifier<'me, 'str> {
    buf: String,
    first: char,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str> Identifier<'me, 'str> {
    pub(super) fn new(first: char, scan: &'me mut Scanner<'str>) -> Self {
        Self {
            buf: String::new(),
            first,
            scan,
        }
    }

    pub(super) fn scan(&mut self) -> TokenExtractResult {
        // TODO: can first be passed as argument instead of state
        match self.first {
            '|' => todo!(),                            // TODO: literal identifier
            '+' | '-' | '.' => todo!(),                // TODO: peculiar identifier
            _ if is_id_initial(self.first) => todo!(), // TODO: identifier
            _ => todo!(),                              // TODO: invalid identifier
        }
    }
}

pub(super) fn continue_block_comment<'me, 'str>(
    depth: usize,
    scan: &'me mut Scanner<'str>,
) -> BlockComment<'me, 'str, ContinueBlockComment> {
    BlockComment {
        depth,
        mode: ContinueBlockComment,
        scan,
    }
}

fn new_block_comment<'me, 'str>(
    scan: &'me mut Scanner<'str>,
) -> BlockComment<'me, 'str, StartBlockComment> {
    BlockComment {
        depth: 0,
        mode: StartBlockComment,
        scan,
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

struct StartBlockComment;

impl BlockCommentMode for StartBlockComment {
    fn terminated(&self) -> TokenKind {
        TokenKind::CommentBlock
    }

    fn unterminated(&self, depth: usize) -> TokenKind {
        TokenKind::CommentBlockBegin(depth)
    }
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
    is_id_letter(ch) || is_id_special_initial(ch)
}

fn is_id_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic()
}

fn is_id_special_initial(ch: char) -> bool {
    matches!(
        ch,
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '_' | '~'
    )
}

enum HexParse {
    Invalid,
    Unexpected,
    Valid(char),
}

// NOTE: state functionality is covered by Tokenizer and Continuation tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blockcomment_new() {
        let mut s = Scanner::new("");

        let target = new_block_comment(&mut s);

        assert_eq!(target.depth, 0);
    }

    #[test]
    fn blockcomment_cont() {
        let mut s = Scanner::new("");

        let target = continue_block_comment(3, &mut s);

        assert_eq!(target.depth, 3);
    }
}
