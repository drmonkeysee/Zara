use super::{Decimal, FreeText, FreeTextPolicy, Numeric, Radix};
use crate::lex::{
    token::{TokenErrorKind, TokenKind},
    tokenize::{
        extract::TokenExtractResult,
        scan::{ScanItem, Scanner},
    },
};

pub(in crate::lex::tokenize) struct Identifier<'me, 'str> {
    peculiar_state: Option<PeculiarState>,
    scan: &'me mut Scanner<'str>,
    start: ScanItem<'str>,
}

impl<'me, 'str> Identifier<'me, 'str> {
    pub(in crate::lex::tokenize) fn new(
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        Self {
            peculiar_state: None,
            scan,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn scan(&mut self) -> TokenExtractResult {
        let first = self.start.1;
        if first == '|' {
            VerbatimIdentifer::new(self.scan).scan()
        } else if is_peculiar_initial(first) {
            self.peculiar(first)
        } else if is_initial(first) {
            self.standard()
        } else {
            self.invalid(first)
        }
    }

    fn standard(&mut self) -> TokenExtractResult {
        while let Some(ch) = self.scan.char_if_not_delimiter() {
            if !is_standard(ch) {
                return self.invalid(ch);
            }
        }
        let txt = self.extract_str();
        Ok(super::numeric_label(txt).unwrap_or_else(|| TokenKind::Identifier(txt.to_owned())))
    }

    fn peculiar(&mut self, ch: char) -> TokenExtractResult {
        self.classify_peculiar(ch);
        let next_ch = self.scan.char_if_not_delimiter();
        self.continue_peculiar(next_ch)
    }

    fn continue_peculiar(&mut self, next_ch: Option<char>) -> TokenExtractResult {
        if let Some(ch) = next_ch {
            if Decimal.is_digit(ch) {
                if let Some(ps) = &self.peculiar_state {
                    match ps {
                        PeculiarState::DefiniteIdentifier => self.standard(),
                        // CASE: .<digit>
                        PeculiarState::MaybeFloat => {
                            Numeric::try_float(self.scan, self.start).scan()
                        }
                        // CASE: +/-.<digit>
                        PeculiarState::MaybeSignedFloat(ch) => Numeric::try_signed_float(
                            super::char_to_sign(*ch),
                            self.scan,
                            self.start,
                        )
                        .scan(),
                        // CASE: +/-<digit>
                        PeculiarState::MaybeSignedNumber(ch) => Numeric::try_signed_number(
                            super::char_to_sign(*ch),
                            self.scan,
                            self.start,
                        )
                        .scan(),
                    }
                } else {
                    // NOTE: this method is only reachable from a peculiar state
                    unreachable!();
                }
            } else if is_peculiar_initial(ch) {
                self.peculiar(ch)
            } else if is_initial(ch) {
                self.standard()
            } else {
                self.invalid(ch)
            }
        } else {
            // NOTE: a single '.' is invalid but Tokenizer handles '.'
            // before attempting Identifier so this case never happens.
            let txt = self.extract_txt();
            debug_assert!(txt != ".");
            Ok(TokenKind::Identifier(txt))
        }
    }

    fn invalid(&mut self, ch: char) -> TokenExtractResult {
        self.scan.end_of_token();
        Err(TokenErrorKind::IdentifierInvalid(ch))
    }

    fn classify_peculiar(&mut self, ch: char) {
        // NOTE: only 3 cases: + | - | .
        self.peculiar_state = Some(match ch {
            '+' | '-' => match self.peculiar_state {
                None => PeculiarState::MaybeSignedNumber(ch),
                _ => PeculiarState::DefiniteIdentifier,
            },
            '.' => match self.peculiar_state {
                Some(PeculiarState::MaybeSignedNumber(sn)) => PeculiarState::MaybeSignedFloat(sn),
                None => PeculiarState::MaybeFloat,
                _ => PeculiarState::DefiniteIdentifier,
            },
            _ => unreachable!(),
        });
    }

    fn not_implemented(&mut self) -> TokenExtractResult {
        self.scan.rest_of_token();
        Err(TokenErrorKind::Unimplemented(self.extract_txt()))
    }

    fn extract_txt(&mut self) -> String {
        self.extract_str().to_owned()
    }

    fn extract_str(&mut self) -> &str {
        let end = self.scan.pos();
        self.scan.lexeme(self.start.0..end)
    }
}

pub(in crate::lex::tokenize) struct PeriodIdentifier<'me, 'str>(Identifier<'me, 'str>);

impl<'me, 'str> PeriodIdentifier<'me, 'str> {
    pub(in crate::lex::tokenize) fn new(
        scan: &'me mut Scanner<'str>,
        start: ScanItem<'str>,
    ) -> Self {
        debug_assert_eq!(start.1, '.');
        let mut me = Self(Identifier::new(scan, start));
        me.0.classify_peculiar(start.1);
        me
    }

    pub(in crate::lex::tokenize) fn scan(&mut self, next: char) -> TokenExtractResult {
        self.0.continue_peculiar(Some(next))
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
    MaybeSignedFloat(char),
    MaybeSignedNumber(char),
}

fn is_initial(ch: char) -> bool {
    // TODO: disallow Mc, Me, Nd
    is_letter(ch) || is_special_initial(ch)
}

fn is_standard(ch: char) -> bool {
    is_initial(ch) || is_digit(ch) || is_peculiar_initial(ch)
}

fn is_letter(ch: char) -> bool {
    // TODO: support Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So, Co, U+200C, U+200D
    ch.is_ascii_alphabetic()
}

fn is_digit(ch: char) -> bool {
    // TODO: support Nd, Nl, No
    ch.is_ascii_digit()
}

fn is_special_initial(ch: char) -> bool {
    matches!(
        ch,
        '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '_' | '~'
    )
}

fn is_peculiar_initial(ch: char) -> bool {
    matches!(ch, '+' | '-' | '.')
}
