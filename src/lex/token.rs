use crate::{
    number::{Number, NumericError, Real},
    txt::TxtSpan,
};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub(crate) struct TokenType<T> {
    pub(crate) kind: T,
    pub(crate) span: TxtSpan,
}

pub(crate) type Token = TokenType<TokenKind>;

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}[{:?}]", self.kind, self.span)
    }
}

#[derive(Debug)]
pub(crate) enum TokenKind {
    Boolean(bool),
    ByteVector,
    Character(char),
    Comment,
    CommentBlockBegin {
        depth: usize,
    },
    CommentBlockEnd,
    CommentBlockFragment {
        depth: usize,
    },
    CommentDatum,
    DirectiveCase(bool),
    Identifier(String),
    IdentifierBegin(String),
    IdentifierDiscard,
    IdentifierEnd(String),
    IdentifierFragment(String),
    Imaginary(Real),
    #[allow(dead_code, reason = "not yet implemented")]
    LabelDef(String),
    #[allow(dead_code, reason = "not yet implemented")]
    LabelRef(String),
    Number(Number),
    PairJoiner,
    ParenLeft,
    ParenRight,
    Quasiquote,
    Quote,
    String(String),
    StringBegin {
        s: String,
        line_cont: bool,
    },
    StringDiscard,
    StringEnd(String),
    StringFragment {
        s: String,
        line_cont: bool,
    },
    Unquote,
    UnquoteSplice,
    Vector,
}

impl TokenKind {
    pub(super) fn to_continuation(&self) -> Option<TokenContinuation> {
        match self {
            Self::CommentBlockBegin { depth } | Self::CommentBlockFragment { depth } => {
                Some(TokenContinuation::BlockComment { depth: *depth })
            }
            Self::IdentifierBegin(_) | Self::IdentifierFragment(_) => {
                Some(TokenContinuation::VerbatimIdentifier)
            }
            Self::IdentifierDiscard => Some(TokenContinuation::SubidentifierError),
            Self::StringBegin { line_cont, .. } | Self::StringFragment { line_cont, .. } => {
                Some(TokenContinuation::StringLiteral {
                    line_cont: *line_cont,
                })
            }
            Self::StringDiscard => Some(TokenContinuation::SubstringError),
            _ => None,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Boolean(_) => f.write_str("BOOL"),
            Self::ByteVector => f.write_str("BYTEVECTOR"),
            Self::Character(_) => f.write_str("CHAR"),
            Self::Comment => f.write_str("COMMENT"),
            Self::CommentBlockBegin { depth } => write!(f, "COMMENTBEGIN<{depth:?}>"),
            Self::CommentBlockEnd => f.write_str("COMMENTEND"),
            Self::CommentBlockFragment { depth } => write!(f, "COMMENTFRAGMENT<{depth:?}>"),
            Self::CommentDatum => f.write_str("DATUMCOMMENT"),
            Self::DirectiveCase(fold) => write!(f, "FOLDCASE<{fold:?}>"),
            Self::Identifier(_) => f.write_str("IDENTIFIER"),
            Self::IdentifierBegin(_) => f.write_str("IDENTBEGIN"),
            Self::IdentifierDiscard => f.write_str("IDENTDISCARD"),
            Self::IdentifierEnd(_) => f.write_str("IDENTEND"),
            Self::IdentifierFragment(_) => f.write_str("IDENTFRAGMENT"),
            Self::Imaginary(r) => write!(f, "IMAGINARY<{}>", r.as_token_descriptor()),
            Self::LabelDef(_) => f.write_str("LABELDEF"),
            Self::LabelRef(_) => f.write_str("LABELREF"),
            Self::Number(n) => write!(f, "NUM<{}>", n.as_token_descriptor()),
            Self::PairJoiner => f.write_str("PAIR"),
            Self::ParenLeft => f.write_str("LEFTPAREN"),
            Self::ParenRight => f.write_str("RIGHTPAREN"),
            Self::Quasiquote => f.write_str("QUASIQUOTE"),
            Self::Quote => f.write_str("QUOTE"),
            Self::String(_) => f.write_str("STR"),
            Self::StringBegin { line_cont, .. } => {
                write!(f, "STRBEGIN{}", line_cont_token(*line_cont))
            }
            Self::StringDiscard => f.write_str("STRDISCARD"),
            Self::StringEnd(_) => f.write_str("STREND"),
            Self::StringFragment { line_cont, .. } => {
                write!(f, "STRFRAGMENT{}", line_cont_token(*line_cont))
            }
            Self::Unquote => f.write_str("UNQUOTE"),
            Self::UnquoteSplice => f.write_str("UNQUOTESPLICE"),
            Self::Vector => f.write_str("VECTOR"),
        }
    }
}

pub(super) type TokenError = TokenType<TokenErrorKind>;

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for TokenError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            TokenErrorKind::NumericError(err) | TokenErrorKind::NumericErrorAt { err, .. } => {
                Some(err)
            }
            _ => None,
        }
    }
}

pub(super) type TokenResult = Result<Token, TokenError>;

#[derive(Debug)]
pub(super) enum TokenErrorKind {
    BlockCommentUnterminated,
    BooleanExpected(bool),
    ByteVectorExpected,
    CharacterExpected,
    CharacterExpectedHex,
    CharacterInvalidHex,
    ComplexInvalid,
    DirectiveExpected,
    DirectiveInvalid,
    ExactnessExpected { at: usize },
    IdentifierEscapeInvalid { at: usize, ch: char },
    IdentifierExpectedHex { at: usize },
    IdentifierInvalid(char),
    IdentifierInvalidHex { at: usize },
    IdentifierUnterminated,
    IdentifierUnterminatedHex { at: usize },
    ImaginaryInvalid,
    ImaginaryMissingSign,
    LabelInvalid,
    LabelUnterminated,
    NumberExpected,
    NumberInvalid,
    NumberInvalidDecimalPoint { at: usize, radix: &'static str },
    NumberInvalidExponent { at: usize, radix: &'static str },
    NumberUnexpectedDecimalPoint { at: usize },
    NumericError(NumericError),
    NumericErrorAt { at: usize, err: NumericError },
    PolarInvalid,
    RadixExpected { at: usize },
    RationalInvalid,
    StringEscapeInvalid { at: usize, ch: char },
    StringExpectedHex { at: usize },
    StringInvalidHex { at: usize },
    StringUnterminated,
    StringUnterminatedHex { at: usize },
    HashInvalid,
    HashUnterminated,
}

impl TokenErrorKind {
    pub(super) fn to_continuation(&self) -> Option<TokenContinuation> {
        match self {
            Self::IdentifierEscapeInvalid { .. }
            | Self::IdentifierExpectedHex { .. }
            | Self::IdentifierInvalidHex { .. }
            | Self::IdentifierUnterminatedHex { .. } => Some(TokenContinuation::SubidentifierError),
            Self::StringEscapeInvalid { .. }
            | Self::StringExpectedHex { .. }
            | Self::StringInvalidHex { .. }
            | Self::StringUnterminatedHex { .. } => Some(TokenContinuation::SubstringError),
            _ => None,
        }
    }

    pub(super) fn sub_idx(&self) -> Option<usize> {
        match self {
            Self::ExactnessExpected { at }
            | Self::IdentifierEscapeInvalid { at, .. }
            | Self::IdentifierExpectedHex { at }
            | Self::IdentifierInvalidHex { at }
            | Self::IdentifierUnterminatedHex { at }
            | Self::NumberInvalidDecimalPoint { at, .. }
            | Self::NumberInvalidExponent { at, .. }
            | Self::NumberUnexpectedDecimalPoint { at }
            | Self::NumericErrorAt { at, .. }
            | Self::RadixExpected { at }
            | Self::StringEscapeInvalid { at, .. }
            | Self::StringExpectedHex { at }
            | Self::StringInvalidHex { at }
            | Self::StringUnterminatedHex { at } => Some(*at),
            _ => None,
        }
    }
}

impl Display for TokenErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::BlockCommentUnterminated => f.write_str("unterminated block comment"),
            Self::BooleanExpected(b) => write!(f, "expected boolean literal: {b}"),
            Self::ByteVectorExpected => f.write_str("expected bytevector literal: #u8(…)"),
            Self::CharacterExpected => f.write_str("expected character literal"),
            Self::CharacterExpectedHex => f.write_str("expected character hex-sequence"),
            Self::CharacterInvalidHex => {
                format_char_range_error("character hex-sequence out of valid range", f)
            }
            Self::ComplexInvalid => f.write_str("invalid complex literal"),
            Self::DirectiveExpected => f.write_str("expected directive: fold-case or no-fold-case"),
            Self::DirectiveInvalid => {
                f.write_str("unsupported directive: expected fold-case or no-fold-case")
            }
            Self::ExactnessExpected { .. } => {
                f.write_str("expected exactness prefix, one of: #e #i")
            }
            Self::HashInvalid => f.write_str("invalid #-literal"),
            Self::HashUnterminated => f.write_str("unterminated #-literal"),
            Self::IdentifierInvalid(ch) => write!(f, "invalid identifier character: {ch}"),
            Self::IdentifierEscapeInvalid { ch, .. } | Self::StringEscapeInvalid { ch, .. } => {
                write!(f, "invalid escape sequence: \\{ch}")
            }
            Self::IdentifierExpectedHex { .. } | Self::StringExpectedHex { .. } => {
                f.write_str("expected hex-escape")
            }
            Self::IdentifierInvalidHex { .. } | Self::StringInvalidHex { .. } => {
                format_char_range_error("hex-escape out of valid range", f)
            }
            Self::IdentifierUnterminatedHex { .. } | Self::StringUnterminatedHex { .. } => {
                f.write_str("unterminated hex-escape")
            }
            Self::IdentifierUnterminated => f.write_str("unterminated verbatim identifier"),
            Self::ImaginaryInvalid => f.write_str("invalid imaginary literal"),
            Self::ImaginaryMissingSign => f.write_str("missing explicit sign on imaginary number"),
            Self::LabelInvalid => f.write_str("invalid datum label: only digits [0, 9] allowed"),
            Self::LabelUnterminated => f.write_str("unterminated datum label"),
            Self::NumberExpected => f.write_str("expected numeric literal"),
            Self::NumberInvalid => f.write_str("invalid numeric literal"),
            Self::NumberInvalidDecimalPoint { radix, .. } => {
                write!(f, "{radix} radix does not support decimal notation")
            }
            Self::NumberInvalidExponent { radix, .. } => {
                write!(f, "{radix} radix does not support scientific notation")
            }
            Self::NumberUnexpectedDecimalPoint { .. } => f.write_str("unexpected decimal point"),
            Self::NumericError(err) | Self::NumericErrorAt { err, .. } => {
                write!(f, "numeric error - {err}")
            }
            Self::PolarInvalid => f.write_str("invalid polar literal"),
            Self::RadixExpected { .. } => f.write_str("expected radix prefix, one of: #b #o #d #x"),
            Self::RationalInvalid => f.write_str("invalid rational literal"),
            Self::StringUnterminated => f.write_str("unterminated string literal"),
        }
    }
}

impl From<TokenContinuation> for TokenErrorKind {
    fn from(value: TokenContinuation) -> Self {
        match value {
            TokenContinuation::BlockComment { .. } => Self::BlockCommentUnterminated,
            TokenContinuation::SubidentifierError | TokenContinuation::VerbatimIdentifier => {
                Self::IdentifierUnterminated
            }
            TokenContinuation::StringLiteral { .. } | TokenContinuation::SubstringError => {
                Self::StringUnterminated
            }
        }
    }
}

impl From<NumericError> for TokenErrorKind {
    fn from(value: NumericError) -> Self {
        Self::NumericError(value)
    }
}

#[derive(Debug)]
pub(super) enum TokenContinuation {
    BlockComment { depth: usize },
    StringLiteral { line_cont: bool },
    SubidentifierError,
    SubstringError,
    VerbatimIdentifier,
}

fn format_char_range_error(msg: &str, f: &mut Formatter) -> fmt::Result {
    write!(f, "{msg}: [{:#x}, {:#x}]", 0, char::MAX as u32)
}

fn line_cont_token(line_cont: bool) -> &'static str {
    if line_cont { "<\\>" } else { "" }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod token {
        use super::*;
        use crate::testutil::ok_or_fail;

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
        fn display_comment_blockbegin() {
            let token = Token {
                kind: TokenKind::CommentBlockBegin { depth: 1 },
                span: 0..10,
            };

            assert_eq!(token.to_string(), "COMMENTBEGIN<1>[0..10]");
        }

        #[test]
        fn display_comment_blockfragment() {
            let token = Token {
                kind: TokenKind::CommentBlockFragment { depth: 1 },
                span: 0..10,
            };

            assert_eq!(token.to_string(), "COMMENTFRAGMENT<1>[0..10]");
        }

        #[test]
        fn display_comment_blockend() {
            let token = Token {
                kind: TokenKind::CommentBlockEnd,
                span: 0..10,
            };

            assert_eq!(token.to_string(), "COMMENTEND[0..10]");
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

            assert_eq!(token.to_string(), "FOLDCASE<false>[0..10]");
        }

        #[test]
        fn bool_token() {
            let token = Token {
                kind: TokenKind::Boolean(false),
                span: 0..2,
            };

            assert_eq!(token.to_string(), "BOOL[0..2]");
        }

        #[test]
        fn char_token() {
            let token = Token {
                kind: TokenKind::Character('a'),
                span: 0..3,
            };

            assert_eq!(token.to_string(), "CHAR[0..3]");
        }

        #[test]
        fn string_token() {
            let token = Token {
                kind: TokenKind::String("foo".to_owned()),
                span: 0..5,
            };

            assert_eq!(token.to_string(), "STR[0..5]");
        }

        #[test]
        fn integer_token() {
            let token = Token {
                kind: TokenKind::Number(Number::real(42)),
                span: 0..5,
            };

            assert_eq!(token.to_string(), "NUM<INT>[0..5]");
        }

        #[test]
        fn float_token() {
            let token = Token {
                kind: TokenKind::Number(Number::real(4.2)),
                span: 0..5,
            };

            assert_eq!(token.to_string(), "NUM<FLT>[0..5]");
        }

        #[test]
        fn rational_token() {
            let r = ok_or_fail!(Real::reduce(4, 5));
            let token = Token {
                kind: TokenKind::Number(Number::real(r)),
                span: 0..5,
            };

            assert_eq!(token.to_string(), "NUM<RAT>[0..5]");
        }

        #[test]
        fn complex_token() {
            let token = Token {
                kind: TokenKind::Number(Number::complex(3, 5)),
                span: 0..5,
            };

            assert_eq!(token.to_string(), "NUM<CPX>[0..5]");
        }

        #[test]
        fn display_label_definition() {
            let token = Token {
                kind: TokenKind::LabelDef("12345".to_owned()),
                span: 0..7,
            };

            assert_eq!(token.to_string(), "LABELDEF[0..7]");
        }

        #[test]
        fn display_label_reference() {
            let token = Token {
                kind: TokenKind::LabelRef("12345".to_owned()),
                span: 0..7,
            };

            assert_eq!(token.to_string(), "LABELREF[0..7]");
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
                kind: TokenKind::StringBegin {
                    s: "foo".to_owned(),
                    line_cont: false,
                },
                span: 0..1,
            };

            assert_eq!(token.to_string(), "STRBEGIN[0..1]");
        }

        #[test]
        fn display_string_begin_with_line_continuation() {
            let token = Token {
                kind: TokenKind::StringBegin {
                    s: "foo\\".to_owned(),
                    line_cont: true,
                },
                span: 0..1,
            };

            assert_eq!(token.to_string(), "STRBEGIN<\\>[0..1]");
        }

        #[test]
        fn display_string_fragment() {
            let token = Token {
                kind: TokenKind::StringFragment {
                    s: "foo".to_owned(),
                    line_cont: false,
                },
                span: 0..1,
            };

            assert_eq!(token.to_string(), "STRFRAGMENT[0..1]");
        }

        #[test]
        fn display_string_fragment_with_line_continuation() {
            let token = Token {
                kind: TokenKind::StringFragment {
                    s: "foo \\".to_owned(),
                    line_cont: true,
                },
                span: 0..1,
            };

            assert_eq!(token.to_string(), "STRFRAGMENT<\\>[0..1]");
        }

        #[test]
        fn display_string_end() {
            let token = Token {
                kind: TokenKind::StringEnd("foo".to_owned()),
                span: 0..1,
            };

            assert_eq!(token.to_string(), "STREND[0..1]");
        }

        #[test]
        fn display_string_discard() {
            let token = Token {
                kind: TokenKind::StringDiscard,
                span: 0..1,
            };

            assert_eq!(token.to_string(), "STRDISCARD[0..1]");
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
        fn display_identifier_start() {
            let token = Token {
                kind: TokenKind::IdentifierBegin("foo".to_owned()),
                span: 0..3,
            };

            assert_eq!(token.to_string(), "IDENTBEGIN[0..3]");
        }

        #[test]
        fn display_identifier_fragment() {
            let token = Token {
                kind: TokenKind::IdentifierFragment("foo".to_owned()),
                span: 0..3,
            };

            assert_eq!(token.to_string(), "IDENTFRAGMENT[0..3]");
        }

        #[test]
        fn display_identifier_end() {
            let token = Token {
                kind: TokenKind::IdentifierEnd("foo".to_owned()),
                span: 0..3,
            };

            assert_eq!(token.to_string(), "IDENTEND[0..3]");
        }

        #[test]
        fn display_identifier_discard() {
            let token = Token {
                kind: TokenKind::IdentifierDiscard,
                span: 0..3,
            };

            assert_eq!(token.to_string(), "IDENTDISCARD[0..3]");
        }

        #[test]
        fn display_imaginary() {
            let token = Token {
                kind: TokenKind::Imaginary(4.into()),
                span: 0..3,
            };

            assert_eq!(token.to_string(), "IMAGINARY<INT>[0..3]");
        }

        #[test]
        fn no_token_continuation() {
            let kind = TokenKind::Quote;

            assert!(kind.to_continuation().is_none());
        }

        #[test]
        fn block_comment_open_continuation() {
            let kind = TokenKind::CommentBlockBegin { depth: 2 };

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::BlockComment { depth: 2 })
            ));
        }

        #[test]
        fn block_comment_fragment_continuation() {
            let kind = TokenKind::CommentBlockFragment { depth: 2 };

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::BlockComment { depth: 2 })
            ));
        }

        #[test]
        fn string_open_continuation() {
            let kind = TokenKind::StringBegin {
                s: "".to_owned(),
                line_cont: false,
            };

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::StringLiteral { line_cont: false })
            ));
        }

        #[test]
        fn string_fragment_continuation() {
            let kind = TokenKind::StringFragment {
                s: "".to_owned(),
                line_cont: true,
            };

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::StringLiteral { line_cont: true })
            ));
        }

        #[test]
        fn identifier_open_continuation() {
            let kind = TokenKind::IdentifierBegin("".to_owned());

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::VerbatimIdentifier)
            ));
        }

        #[test]
        fn identifier_fragment_continuation() {
            let kind = TokenKind::IdentifierFragment("".to_owned());

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::VerbatimIdentifier)
            ));
        }
    }

    mod tokenerror {
        use super::*;
        use crate::testutil::some_or_fail;

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
            let cases = [
                TokenErrorKind::IdentifierEscapeInvalid { at: 1, ch: 'B' },
                TokenErrorKind::StringEscapeInvalid { at: 1, ch: 'B' },
            ];
            for case in cases {
                let err = TokenError {
                    kind: case,
                    span: 0..1,
                };

                assert_eq!(err.to_string(), "invalid escape sequence: \\B");
            }
        }

        #[test]
        fn display_expected_string_hex() {
            let cases = [
                TokenErrorKind::IdentifierExpectedHex { at: 1 },
                TokenErrorKind::StringExpectedHex { at: 1 },
            ];
            for case in cases {
                let err = TokenError {
                    kind: case,
                    span: 0..1,
                };

                assert_eq!(err.to_string(), "expected hex-escape");
            }
        }

        #[test]
        fn display_invalid_string_hex() {
            let cases = [
                TokenErrorKind::IdentifierInvalidHex { at: 1 },
                TokenErrorKind::StringInvalidHex { at: 1 },
            ];
            for case in cases {
                let err = TokenError {
                    kind: case,
                    span: 0..1,
                };

                assert_eq!(
                    err.to_string(),
                    "hex-escape out of valid range: [0x0, 0x10ffff]"
                );
            }
        }

        #[test]
        fn display_unterminated_string_hex() {
            let cases = [
                TokenErrorKind::IdentifierUnterminatedHex { at: 1 },
                TokenErrorKind::StringUnterminatedHex { at: 1 },
            ];
            for case in cases {
                let err = TokenError {
                    kind: case,
                    span: 0..1,
                };

                assert_eq!(err.to_string(), "unterminated hex-escape");
            }
        }

        #[test]
        fn display_invalid_hash() {
            let err = TokenError {
                kind: TokenErrorKind::HashInvalid,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid #-literal");
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
        fn no_continuation() {
            let kind = TokenErrorKind::CharacterExpected;

            assert!(kind.to_continuation().is_none());
        }

        #[test]
        fn string_invalid_sequence_continuation() {
            let kind = TokenErrorKind::StringEscapeInvalid { at: 0, ch: 'c' };

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::SubstringError)
            ));
        }

        #[test]
        fn string_hex_continuation() {
            let kind = TokenErrorKind::StringExpectedHex { at: 1 };

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::SubstringError)
            ));
        }

        #[test]
        fn identifier_invalid_sequence_continuation() {
            let kind = TokenErrorKind::IdentifierEscapeInvalid { at: 0, ch: 'c' };

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::SubidentifierError)
            ));
        }

        #[test]
        fn identifier_hex_continuation() {
            let kind = TokenErrorKind::IdentifierExpectedHex { at: 1 };

            assert!(matches!(
                kind.to_continuation(),
                Some(TokenContinuation::SubidentifierError)
            ));
        }

        #[test]
        fn no_sub_index() {
            let kind = TokenErrorKind::CharacterExpected;

            assert!(kind.sub_idx().is_none());
        }

        #[test]
        fn invalid_sequence_sub_index() {
            let cases = [
                TokenErrorKind::IdentifierEscapeInvalid { at: 3, ch: 'c' },
                TokenErrorKind::StringEscapeInvalid { at: 3, ch: 'c' },
            ];
            for case in cases {
                assert!(matches!(case.sub_idx(), Some(3)));
            }
        }

        #[test]
        fn hex_sub_index() {
            let cases = [
                TokenErrorKind::IdentifierExpectedHex { at: 3 },
                TokenErrorKind::StringExpectedHex { at: 3 },
            ];
            for case in cases {
                assert!(matches!(case.sub_idx(), Some(3)));
            }
        }

        #[test]
        fn number_sub_index() {
            let cases = [
                TokenErrorKind::NumberInvalidDecimalPoint {
                    at: 3,
                    radix: "foo",
                },
                TokenErrorKind::NumberInvalidExponent {
                    at: 3,
                    radix: "foo",
                },
                TokenErrorKind::NumberUnexpectedDecimalPoint { at: 3 },
            ];
            for case in cases {
                assert!(matches!(case.sub_idx(), Some(3)));
            }
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
        fn display_string_unterminated() {
            let err = TokenError {
                kind: TokenErrorKind::StringUnterminated,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unterminated string literal");
        }

        #[test]
        fn display_invalid_identifier() {
            let err = TokenError {
                kind: TokenErrorKind::IdentifierInvalid('{'),
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid identifier character: {");
        }

        #[test]
        fn display_identifier_unterminated() {
            let err = TokenError {
                kind: TokenErrorKind::IdentifierUnterminated,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unterminated verbatim identifier");
        }

        #[test]
        fn display_label_invalid() {
            let err = TokenError {
                kind: TokenErrorKind::LabelInvalid,
                span: 0..1,
            };

            assert_eq!(
                err.to_string(),
                "invalid datum label: only digits [0, 9] allowed"
            );
        }

        #[test]
        fn display_label_unterminated() {
            let err = TokenError {
                kind: TokenErrorKind::LabelUnterminated,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unterminated datum label");
        }

        #[test]
        fn display_number_expected() {
            let err = TokenError {
                kind: TokenErrorKind::NumberExpected,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "expected numeric literal");
        }

        #[test]
        fn display_number_invalid() {
            let err = TokenError {
                kind: TokenErrorKind::NumberInvalid,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid numeric literal");
        }

        #[test]
        fn display_number_invalid_decimal() {
            let err = TokenError {
                kind: TokenErrorKind::NumberInvalidDecimalPoint {
                    at: 1,
                    radix: "foo",
                },
                span: 0..1,
            };

            assert_eq!(
                err.to_string(),
                "foo radix does not support decimal notation"
            );
        }

        #[test]
        fn display_number_invalid_exponent() {
            let err = TokenError {
                kind: TokenErrorKind::NumberInvalidExponent {
                    at: 1,
                    radix: "foo",
                },
                span: 0..1,
            };

            assert_eq!(
                err.to_string(),
                "foo radix does not support scientific notation"
            );
        }

        #[test]
        fn display_number_unexpected_decimal() {
            let err = TokenError {
                kind: TokenErrorKind::NumberUnexpectedDecimalPoint { at: 1 },
                span: 0..1,
            };

            assert_eq!(err.to_string(), "unexpected decimal point");
        }

        #[test]
        fn display_imaginary_missing_sign() {
            let err = TokenError {
                kind: TokenErrorKind::ImaginaryMissingSign,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "missing explicit sign on imaginary number");
        }

        #[test]
        fn display_imaginary_malformed() {
            let err = TokenError {
                kind: TokenErrorKind::ImaginaryInvalid,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid imaginary literal");
        }

        #[test]
        fn display_rational_invalid() {
            let err = TokenError {
                kind: TokenErrorKind::RationalInvalid,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid rational literal");
        }

        #[test]
        fn display_complex_invalid() {
            let err = TokenError {
                kind: TokenErrorKind::ComplexInvalid,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid complex literal");
        }

        #[test]
        fn display_polar_invalid() {
            let err = TokenError {
                kind: TokenErrorKind::PolarInvalid,
                span: 0..1,
            };

            assert_eq!(err.to_string(), "invalid polar literal");
        }

        #[test]
        fn display_numeric_error() {
            let err = TokenError {
                kind: NumericError::DivideByZero.into(),
                span: 0..1,
            };

            assert_eq!(err.to_string(), "numeric error - divide by zero");
        }

        #[test]
        fn display_expected_exactness() {
            let err = TokenError {
                kind: TokenErrorKind::ExactnessExpected { at: 0 },
                span: 0..1,
            };

            assert_eq!(err.to_string(), "expected exactness prefix, one of: #e #i");
        }

        #[test]
        fn display_expected_radix() {
            let err = TokenError {
                kind: TokenErrorKind::RadixExpected { at: 0 },
                span: 0..1,
            };

            assert_eq!(
                err.to_string(),
                "expected radix prefix, one of: #b #o #d #x"
            );
        }

        #[test]
        fn numeric_error_source() {
            let err = TokenError {
                kind: TokenErrorKind::NumericError(NumericError::DivideByZero),
                span: 0..1,
            };

            let inner = some_or_fail!(err.source());

            assert!(matches!(
                inner.downcast_ref::<NumericError>().unwrap(),
                NumericError::DivideByZero
            ));
        }

        #[test]
        fn numeric_errorat_source() {
            let err = TokenError {
                kind: TokenErrorKind::NumericErrorAt {
                    at: 1,
                    err: NumericError::ParseExponentFailure,
                },
                span: 0..1,
            };

            let inner = some_or_fail!(err.source());

            assert!(matches!(
                inner.downcast_ref::<NumericError>().unwrap(),
                NumericError::ParseExponentFailure
            ));
        }

        #[test]
        fn other_error_source() {
            let err = TokenError {
                kind: TokenErrorKind::NumberInvalid,
                span: 0..1,
            };

            let inner = err.source();

            assert!(inner.is_none());
        }
    }

    mod tokencontinuation {
        use super::*;

        #[test]
        fn block_comment() {
            let cont = TokenContinuation::BlockComment { depth: 10 };

            assert!(matches!(
                cont.into(),
                TokenErrorKind::BlockCommentUnterminated
            ));
        }

        #[test]
        fn string_fragment() {
            let cont = TokenContinuation::StringLiteral { line_cont: false };

            assert!(matches!(cont.into(), TokenErrorKind::StringUnterminated));
        }

        #[test]
        fn string_error() {
            let cont = TokenContinuation::SubstringError;

            assert!(matches!(cont.into(), TokenErrorKind::StringUnterminated));
        }

        #[test]
        fn identifier_fragment() {
            let cont = TokenContinuation::VerbatimIdentifier;

            assert!(matches!(
                cont.into(),
                TokenErrorKind::IdentifierUnterminated
            ));
        }

        #[test]
        fn identifier_error() {
            let cont = TokenContinuation::SubidentifierError;

            assert!(matches!(
                cont.into(),
                TokenErrorKind::IdentifierUnterminated
            ));
        }
    }
}
