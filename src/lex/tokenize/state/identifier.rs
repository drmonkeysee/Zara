use super::{FreeText, FreeTextPolicy};
use crate::lex::{
    token::{TokenErrorKind, TokenKind},
    tokenize::{extract::TokenExtractResult, scan::Scanner},
};

pub(in crate::lex::tokenize) struct Identifier<'me, 'str> {
    buf: String,
    peculiar_state: PeculiarState,
    scan: &'me mut Scanner<'str>,
}

impl<'me, 'str> Identifier<'me, 'str> {
    pub(in crate::lex::tokenize) fn new(scan: &'me mut Scanner<'str>) -> Self {
        Self {
            buf: String::new(),
            peculiar_state: PeculiarState::Unspecified,
            scan,
        }
    }

    pub(in crate::lex::tokenize) fn scan(mut self, first: char) -> TokenExtractResult {
        if first == '|' {
            VerbatimIdentifer::new(self.scan).scan()
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
                PeculiarState::MaybeSignedNumber => PeculiarState::MaybeSignedFloat,
                PeculiarState::Unspecified => PeculiarState::MaybeFloat,
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

pub(in crate::lex::tokenize) struct PeriodIdentifier<'me, 'str>(Identifier<'me, 'str>);

impl<'me, 'str> PeriodIdentifier<'me, 'str> {
    pub(in crate::lex::tokenize) fn new(scan: &'me mut Scanner<'str>) -> Self {
        let mut me = Self(Identifier::new(scan));
        me.0.push_peculiar('.');
        me
    }

    pub(in crate::lex::tokenize) fn scan(self, first: char) -> TokenExtractResult {
        self.0.continue_peculiar(Some(first))
    }
}

pub(in crate::lex::tokenize) type VerbatimIdentifer<'me, 'str, M> =
    FreeText<'me, 'str, IdentifierPolicy<M>>;

impl<'me, 'str> VerbatimIdentifer<'me, 'str, ContinueIdentifier> {
    pub(in crate::lex::tokenize) fn cont(scan: &'me mut Scanner<'str>) -> Self {
        Self::init(scan, IdentifierPolicy(ContinueIdentifier))
    }
}

impl<'me, 'str> VerbatimIdentifer<'me, 'str, DiscardIdentifier> {
    pub(in crate::lex::tokenize) fn cleanup(scan: &'me mut Scanner<'str>) -> Self {
        Self::init(scan, IdentifierPolicy(DiscardIdentifier))
    }
}

impl<'me, 'str> VerbatimIdentifer<'me, 'str, StartIdentifier> {
    fn new(scan: &'me mut Scanner<'str>) -> Self {
        Self::init(scan, IdentifierPolicy(StartIdentifier))
    }
}

pub(in crate::lex::tokenize) struct IdentifierPolicy<M>(M);

impl<M: IdentifierPolicyMode> FreeTextPolicy for IdentifierPolicy<M> {
    const TERMINATOR: char = '|';

    fn prelude(&self, _scan: &mut Scanner<'_>) {
        // NOTE: do nothing for verbatim identifiers
    }

    fn escape_invalid(&self, start: usize, ch: char) -> TokenErrorKind {
        TokenErrorKind::IdentifierEscapeInvalid { at: start, ch }
    }

    fn hex_expected(&self, start: usize) -> TokenErrorKind {
        TokenErrorKind::IdentifierExpectedHex { at: start }
    }

    fn hex_invalid(&self, start: usize) -> TokenErrorKind {
        TokenErrorKind::IdentifierInvalidHex { at: start }
    }

    fn hex_unterminated(&self, start: usize) -> TokenErrorKind {
        TokenErrorKind::IdentifierUnterminatedHex { at: start }
    }

    fn terminated(&self, buf: String) -> TokenKind {
        self.0.terminated(buf)
    }

    fn unterminated(&self, buf: String, _line_cont_idx: Option<usize>) -> TokenKind {
        self.0.unterminated(buf)
    }
}

pub(in crate::lex::tokenize) struct ContinueIdentifier;

impl IdentifierPolicyMode for ContinueIdentifier {
    fn terminated(&self, buf: String) -> TokenKind {
        TokenKind::IdentifierEnd(buf)
    }

    fn unterminated(&self, buf: String) -> TokenKind {
        TokenKind::IdentifierFragment(buf)
    }
}

pub(in crate::lex::tokenize) struct DiscardIdentifier;

impl IdentifierPolicyMode for DiscardIdentifier {
    fn terminated(&self, _buf: String) -> TokenKind {
        TokenKind::IdentifierDiscard
    }

    fn unterminated(&self, buf: String) -> TokenKind {
        self.terminated(buf)
    }
}

trait IdentifierPolicyMode {
    fn terminated(&self, buf: String) -> TokenKind;
    fn unterminated(&self, buf: String) -> TokenKind;
}

struct StartIdentifier;

impl IdentifierPolicyMode for StartIdentifier {
    fn terminated(&self, buf: String) -> TokenKind {
        TokenKind::Identifier(buf)
    }

    fn unterminated(&self, buf: String) -> TokenKind {
        TokenKind::IdentifierBegin(buf)
    }
}

enum PeculiarState {
    DefiniteIdentifier,
    MaybeFloat,
    MaybeSignedFloat,
    MaybeSignedNumber,
    Unspecified,
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
