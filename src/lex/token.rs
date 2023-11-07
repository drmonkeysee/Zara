use crate::literal::Literal;
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
    ops::Range,
};

#[derive(Debug)]
pub struct TokenType<T> {
    pub(crate) kind: T,
    pub(crate) span: Range<usize>,
}

pub type Token = TokenType<TokenKind>;

impl Token {
    pub(super) fn into_continuation_unsupported(self) -> TokenError {
        TokenError {
            kind: self
                .kind
                .to_continuation()
                .map_or(TokenErrorKind::ContinuationInvalid, TokenErrorKind::from),
            span: self.span,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{:?}]", self.kind, self.span)
    }
}

#[derive(Debug)]
pub enum TokenKind {
    ByteVector,
    Comment,
    CommentBlock,
    CommentBlockBegin(usize),
    CommentBlockEnd,
    CommentBlockFragment(usize),
    CommentDatum,
    DirectiveCase(bool),
    Identifier(String),
    Literal(Literal),
    ParenLeft,
    ParenRight,
    PairJoiner,
    Quasiquote,
    Quote,
    StringBegin(String, bool),
    StringDiscard,
    StringEnd(String),
    StringFragment(String, bool),
    Unquote,
    UnquoteSplice,
    Vector,
}

impl TokenKind {
    pub(super) fn to_continuation(&self) -> Option<TokenContinuation> {
        match self {
            Self::CommentBlockBegin(depth) | Self::CommentBlockFragment(depth) => {
                Some(TokenContinuation::BlockComment(*depth))
            }
            Self::StringBegin(_, line_cont) | Self::StringFragment(_, line_cont) => {
                Some(TokenContinuation::StringLiteral(*line_cont))
            }
            Self::StringDiscard => Some(TokenContinuation::SubstringError),
            _ => None,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ByteVector => f.write_str("BYTEVECTOR"),
            Self::Comment => f.write_str("COMMENT"),
            Self::CommentBlock => f.write_str("BLOCKCOMMENT"),
            Self::CommentBlockBegin(depth) => write!(f, "BLOCKCOMMENTBEGIN<{depth:?}>"),
            Self::CommentBlockEnd => f.write_str("BLOCKCOMMENTEND"),
            Self::CommentBlockFragment(depth) => write!(f, "BLOCKCOMMENTFRAG<{depth:?}>"),
            Self::CommentDatum => f.write_str("DATUMCOMMENT"),
            Self::DirectiveCase(fold) => write!(f, "DIRFOLDCASE<{fold:?}>"),
            Self::Identifier(_) => f.write_str("IDENTIFIER"),
            Self::Literal(lit) => write!(f, "LITERAL<{}>", lit.as_token_descriptor()),
            Self::ParenLeft => f.write_str("LEFTPAREN"),
            Self::ParenRight => f.write_str("RIGHTPAREN"),
            Self::PairJoiner => f.write_str("PAIR"),
            Self::Quasiquote => f.write_str("QUASIQUOTE"),
            Self::Quote => f.write_str("QUOTE"),
            Self::StringBegin(_, line_cont) => {
                write!(f, "BEGINSTR{}", line_cont_token(*line_cont))
            }
            Self::StringDiscard => f.write_str("DISCARDSTR"),
            Self::StringEnd(_) => f.write_str("ENDSTR"),
            Self::StringFragment(_, line_cont) => {
                write!(f, "STRFRAG{}", line_cont_token(*line_cont))
            }
            Self::Unquote => f.write_str("UNQUOTE"),
            Self::UnquoteSplice => f.write_str("UNQUOTESPLICE"),
            Self::Vector => f.write_str("VECTOR"),
        }
    }
}

pub(super) type TokenError = TokenType<TokenErrorKind>;

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for TokenError {}

pub(super) type TokenResult = Result<Token, TokenError>;

#[derive(Debug)]
pub(super) enum TokenErrorKind {
    BlockCommentUnterminated,
    BooleanExpected(bool),
    ByteVectorExpected,
    CharacterExpected,
    CharacterExpectedHex,
    CharacterInvalidHex,
    ContinuationInvalid,
    DirectiveExpected,
    DirectiveInvalid,
    IdentifierInvalid(char),
    StringEscapeInvalid(usize, char),
    StringExpectedHex(usize),
    StringInvalidHex(usize),
    StringUnterminated,
    StringUnterminatedHex(usize),
    HashInvalid,
    HashUnterminated,
    Unimplemented(String),
}

impl TokenErrorKind {
    pub(super) fn to_continuation(&self) -> Option<TokenContinuation> {
        match self {
            TokenErrorKind::StringEscapeInvalid(..)
            | TokenErrorKind::StringExpectedHex(..)
            | TokenErrorKind::StringInvalidHex(..)
            | TokenErrorKind::StringUnterminatedHex(..) => Some(TokenContinuation::SubstringError),
            _ => None,
        }
    }

    pub(super) fn sub_idx(&self) -> Option<usize> {
        match self {
            TokenErrorKind::StringEscapeInvalid(idx, _)
            | TokenErrorKind::StringExpectedHex(idx)
            | TokenErrorKind::StringInvalidHex(idx)
            | TokenErrorKind::StringUnterminatedHex(idx) => Some(*idx),
            _ => None,
        }
    }
}

impl Display for TokenErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::BlockCommentUnterminated => f.write_str("unterminated block comment"),
            Self::BooleanExpected(b) => write!(f, "expected boolean literal: {b}"),
            Self::ByteVectorExpected => f.write_str("expected bytevector literal: #u8(…)"),
            Self::CharacterExpected => f.write_str("expected character literal"),
            Self::CharacterExpectedHex => f.write_str("expected character hex-sequence"),
            Self::CharacterInvalidHex => {
                format_char_range_error("character hex-sequence out of valid range", f)
            }
            Self::ContinuationInvalid => f.write_str("attempted continuation conversion on invalid token; this is likely an interpreter bug!"),
            Self::DirectiveExpected => f.write_str("expected directive: fold-case or no-fold-case"),
            Self::DirectiveInvalid => {
                f.write_str("unsupported directive: expected fold-case or no-fold-case")
            }
            Self::IdentifierInvalid(ch) => write!(f, "invalid identifier character: {ch}"),
            Self::StringEscapeInvalid(_, ch) => write!(f, "invalid escape sequence: \\{ch}"),
            Self::StringExpectedHex(_) => f.write_str("expected hex-escape"),
            Self::StringInvalidHex(_) => {
                format_char_range_error("hex-escape out of valid range", f)
            }
            Self::StringUnterminated => f.write_str("unterminated string-literal"),
            Self::StringUnterminatedHex(_) => f.write_str("unterminated hex-escape"),
            Self::HashInvalid => f.write_str("unexpected #-literal"),
            Self::HashUnterminated => f.write_str("unterminated #-literal"),
            Self::Unimplemented(s) => write!(f, "unimplemented tokenization: '{s}'"),
        }
    }
}

impl From<TokenContinuation> for TokenErrorKind {
    fn from(value: TokenContinuation) -> Self {
        match value {
            TokenContinuation::BlockComment(_) => Self::BlockCommentUnterminated,
            TokenContinuation::StringLiteral(_) | TokenContinuation::SubstringError => {
                Self::StringUnterminated
            }
        }
    }
}

#[derive(Debug)]
pub(super) enum TokenContinuation {
    BlockComment(usize),
    StringLiteral(bool),
    SubstringError,
}

fn format_char_range_error(msg: &str, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "{msg}: [{:#x}, {:#x}]", 0, char::MAX as u32)
}

fn line_cont_token(line_cont: bool) -> &'static str {
    if line_cont {
        "<\\>"
    } else {
        ""
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod token {
        use super::*;

        #[test]
        fn display_bytevector() {
            let token = Token {
                kind: TokenKind::ByteVector,
                span: 0..4,
            };

            assert_eq!(token.to_string(), "BYTEVECTOR[0..4]");
        }

        #[test]
        fn display_comment() {
            let token = Token {
                kind: TokenKind::Comment,
                span: 0..5,
            };

            assert_eq!(token.to_string(), "COMMENT[0..5]");
        }

        #[test]
        fn display_comment_block() {
            let token = Token {
                kind: TokenKind::CommentBlock,
                span: 0..10,
            };

            assert_eq!(token.to_string(), "BLOCKCOMMENT[0..10]");
        }

        #[test]
        fn display_comment_blockbegin() {
            let token = Token {
                kind: TokenKind::CommentBlockBegin(1),
                span: 0..10,
            };

            assert_eq!(token.to_string(), "BLOCKCOMMENTBEGIN<1>[0..10]");
        }

        #[test]
        fn display_comment_blockfragment() {
            let token = Token {
                kind: TokenKind::CommentBlockFragment(1),
                span: 0..10,
            };

            assert_eq!(token.to_string(), "BLOCKCOMMENTFRAG<1>[0..10]");
        }

        #[test]
        fn display_comment_blockend() {
            let token = Token {
                kind: TokenKind::CommentBlockEnd,
                span: 0..10,
            };

            assert_eq!(token.to_string(), "BLOCKCOMMENTEND[0..10]");
        }

        #[test]
        fn display_datum_comment() {
            let token = Token {
                kind: TokenKind::CommentDatum,
                span: 0..2,
            };

            assert_eq!(token.to_string(), "DATUMCOMMENT[0..2]");
        }

        #[test]
        fn display_directive() {
            let token = Token {
                kind: TokenKind::DirectiveCase(false),
                span: 0..10,
            };

            assert_eq!(token.to_string(), "DIRFOLDCASE<false>[0..10]");
        }

        #[test]
        fn display_literal() {
            let token = Token {
                kind: TokenKind::Literal(Literal::Boolean(true)),
                span: 0..2,
            };

            assert_eq!(token.to_string(), "LITERAL<BOOL>[0..2]");
        }

        #[test]
        fn display_paren_left() {
            let token = Token {
                kind: TokenKind::ParenLeft,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "LEFTPAREN[0..1]");
        }

        #[test]
        fn display_paren_right() {
            let token = Token {
                kind: TokenKind::ParenRight,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "RIGHTPAREN[0..1]");
        }

        #[test]
        fn display_pair_joiner() {
            let token = Token {
                kind: TokenKind::PairJoiner,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "PAIR[0..1]");
        }

        #[test]
        fn display_quasiquote() {
            let token = Token {
                kind: TokenKind::Quasiquote,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "QUASIQUOTE[0..1]");
        }

        #[test]
        fn display_quote() {
            let token = Token {
                kind: TokenKind::Quote,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "QUOTE[0..1]");
        }

        #[test]
        fn display_string_begin() {
            let token = Token {
                kind: TokenKind::StringBegin("foo".to_owned(), false),
                span: 0..1,
            };

            assert_eq!(token.to_string(), "BEGINSTR[0..1]");
        }

        #[test]
        fn display_string_begin_with_line_continuation() {
            let token = Token {
                kind: TokenKind::StringBegin("foo\\".to_owned(), true),
                span: 0..1,
            };

            assert_eq!(token.to_string(), "BEGINSTR<\\>[0..1]");
        }

        #[test]
        fn display_string_fragment() {
            let token = Token {
                kind: TokenKind::StringFragment("foo".to_owned(), false),
                span: 0..1,
            };

            assert_eq!(token.to_string(), "STRFRAG[0..1]");
        }

        #[test]
        fn display_string_fragment_with_line_continuation() {
            let token = Token {
                kind: TokenKind::StringFragment("foo \\".to_owned(), true),
                span: 0..1,
            };

            assert_eq!(token.to_string(), "STRFRAG<\\>[0..1]");
        }

        #[test]
        fn display_string_end() {
            let token = Token {
                kind: TokenKind::StringEnd("foo".to_owned()),
                span: 0..1,
            };

            assert_eq!(token.to_string(), "ENDSTR[0..1]");
        }

        #[test]
        fn display_string_discard() {
            let token = Token {
                kind: TokenKind::StringDiscard,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "DISCARDSTR[0..1]");
        }

        #[test]
        fn display_unquote() {
            let token = Token {
                kind: TokenKind::Unquote,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "UNQUOTE[0..1]");
        }

        #[test]
        fn display_unquote_splice() {
            let token = Token {
                kind: TokenKind::UnquoteSplice,
                span: 0..2,
            };

            assert_eq!(token.to_string(), "UNQUOTESPLICE[0..2]");
        }

        #[test]
        fn display_vector() {
            let token = Token {
                kind: TokenKind::Vector,
                span: 0..2,
            };

            assert_eq!(token.to_string(), "VECTOR[0..2]");
        }

        #[test]
        fn display_identifier() {
            let token = Token {
                kind: TokenKind::Identifier("foo".to_owned()),
                span: 0..3,
            };

            assert_eq!(token.to_string(), "IDENTIFIER[0..3]");
        }

        #[test]
        fn no_token_continuation() {
            let kind = TokenKind::Quote;

            assert!(kind.to_continuation().is_none());
        }

        #[test]
        fn block_comment_open_continuation() {
            let kind = TokenKind::CommentBlockBegin(2);

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::BlockComment(2))
            ));
        }

        #[test]
        fn block_comment_fragment_continuation() {
            let kind = TokenKind::CommentBlockFragment(2);

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::BlockComment(2))
            ));
        }

        #[test]
        fn string_open_continuation() {
            let kind = TokenKind::StringBegin("".to_owned(), false);

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::StringLiteral(false))
            ));
        }

        #[test]
        fn string_fragment_continuation() {
            let kind = TokenKind::StringFragment("".to_owned(), true);

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::StringLiteral(true))
            ));
        }

        #[test]
        fn to_valid_continuation_error() {
            let token = Token {
                kind: TokenKind::StringFragment("foo".to_owned(), false),
                span: 1..3,
            };

            let result = token.into_continuation_unsupported();

            assert!(matches!(
                result,
                TokenError {
                    kind: TokenErrorKind::StringUnterminated,
                    span: Range { start: 1, end: 3 },
                }
            ));
        }

        #[test]
        fn to_invalid_continuation_error() {
            let token = Token {
                kind: TokenKind::ParenLeft,
                span: 0..1,
            };

            let result = token.into_continuation_unsupported();

            assert!(matches!(
                result,
                TokenError {
                    kind: TokenErrorKind::ContinuationInvalid,
                    span: Range { start: 0, end: 1 },
                }
            ));
        }
    }

    mod tokenerror {
        use super::*;

        #[test]
        fn display_expected_boolean() {
            let err = TokenError {
                kind: TokenErrorKind::BooleanExpected(true),
                span: 0..1,
            };

            assert_eq!(err.to_string(), "expected boolean literal: true");
        }

        #[test]
        fn display_expected_bytevector() {
            let err = TokenError {
                kind: TokenErrorKind::ByteVectorExpected,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "expected bytevector literal: #u8(…)");
        }

        #[test]
        fn display_expected_character() {
            let err = TokenError {
                kind: TokenErrorKind::CharacterExpected,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "expected character literal");
        }

        #[test]
        fn display_expected_character_hex() {
            let err = TokenError {
                kind: TokenErrorKind::CharacterExpectedHex,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "expected character hex-sequence");
        }

        #[test]
        fn display_invalid_character_hex() {
            let err = TokenError {
                kind: TokenErrorKind::CharacterInvalidHex,
                span: 0..1,
            };

            assert_eq!(
                err.to_string(),
                "character hex-sequence out of valid range: [0x0, 0x10ffff]"
            );
        }

        #[test]
        fn display_expected_directive() {
            let err = TokenError {
                kind: TokenErrorKind::DirectiveExpected,
                span: 0..1,
            };

            assert_eq!(
                err.to_string(),
                "expected directive: fold-case or no-fold-case"
            );
        }

        #[test]
        fn display_invalid_directive() {
            let err = TokenError {
                kind: TokenErrorKind::DirectiveInvalid,
                span: 0..1,
            };

            assert_eq!(
                err.to_string(),
                "unsupported directive: expected fold-case or no-fold-case"
            );
        }

        #[test]
        fn display_invalid_escape() {
            let err = TokenError {
                kind: TokenErrorKind::StringEscapeInvalid(1, 'B'),
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid escape sequence: \\B");
        }

        #[test]
        fn display_expected_string_hex() {
            let err = TokenError {
                kind: TokenErrorKind::StringExpectedHex(1),
                span: 0..1,
            };

            assert_eq!(err.to_string(), "expected hex-escape");
        }

        #[test]
        fn display_invalid_string_hex() {
            let err = TokenError {
                kind: TokenErrorKind::StringInvalidHex(1),
                span: 0..1,
            };

            assert_eq!(
                err.to_string(),
                "hex-escape out of valid range: [0x0, 0x10ffff]"
            );
        }

        #[test]
        fn display_unterminated_string_hex() {
            let err = TokenError {
                kind: TokenErrorKind::StringUnterminatedHex(1),
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unterminated hex-escape");
        }

        #[test]
        fn display_invalid_hash() {
            let err = TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unexpected #-literal");
        }

        #[test]
        fn display_unterminated_hash() {
            let err = TokenError {
                kind: TokenErrorKind::HashUnterminated,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unterminated #-literal");
        }

        #[test]
        fn display_unimplemented() {
            let err = TokenError {
                kind: TokenErrorKind::Unimplemented("foobar".to_owned()),
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unimplemented tokenization: 'foobar'");
        }

        #[test]
        fn no_continuation() {
            let kind = TokenErrorKind::CharacterExpected;

            assert!(kind.to_continuation().is_none());
        }

        #[test]
        fn string_invalid_sequence_continuation() {
            let kind = TokenErrorKind::StringEscapeInvalid(0, 'c');

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::SubstringError)
            ));
        }

        #[test]
        fn string_hex_continuation() {
            let kind = TokenErrorKind::StringExpectedHex(1);

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::SubstringError)
            ));
        }

        #[test]
        fn no_sub_index() {
            let kind = TokenErrorKind::CharacterExpected;

            assert!(kind.sub_idx().is_none());
        }

        #[test]
        fn string_invalid_sequence_sub_index() {
            let kind = TokenErrorKind::StringEscapeInvalid(3, 'c');

            assert!(matches!(kind.sub_idx(), Some(3)));
        }

        #[test]
        fn string_hex_sub_index() {
            let kind = TokenErrorKind::StringExpectedHex(3);

            assert!(matches!(kind.sub_idx(), Some(3)));
        }

        #[test]
        fn display_block_comment_unterminated() {
            let err = TokenError {
                kind: TokenErrorKind::BlockCommentUnterminated,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unterminated block comment");
        }

        #[test]
        fn display_invalid_continuation() {
            let err = TokenError {
                kind: TokenErrorKind::ContinuationInvalid,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "attempted continuation conversion on invalid token; this is likely an interpreter bug!");
        }

        #[test]
        fn display_string_unterminated() {
            let err = TokenError {
                kind: TokenErrorKind::StringUnterminated,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unterminated string-literal");
        }

        #[test]
        fn display_invalid_identifier() {
            let err = TokenError {
                kind: TokenErrorKind::IdentifierInvalid('{'),
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid identifier character: {");
        }
    }

    mod tokencontinuation {
        use super::*;

        #[test]
        fn block_comment() {
            let cont = TokenContinuation::BlockComment(10);

            assert!(matches!(
                cont.into(),
                TokenErrorKind::BlockCommentUnterminated
            ));
        }

        #[test]
        fn string_fragment() {
            let cont = TokenContinuation::StringLiteral(false);

            assert!(matches!(cont.into(), TokenErrorKind::StringUnterminated));
        }

        #[test]
        fn string_error() {
            let cont = TokenContinuation::SubstringError;

            assert!(matches!(cont.into(), TokenErrorKind::StringUnterminated));
        }
    }
}
