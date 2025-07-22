#[cfg(test)]
mod tests;

use crate::{
    number::{Number, NumericError, Real},
    txt::TxtSpan,
};
use std::fmt::{self, Display, Formatter};

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
