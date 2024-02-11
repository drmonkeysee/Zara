use super::{ComplexKind, Exactness, FreeText, FreeTextPolicy, RealNumber};
use crate::{
    lex::{
        token::{TokenErrorKind, TokenKind},
        tokenize::{
            scan::{ScanItem, Scanner},
            TokenExtractResult,
        },
    },
    literal::Literal,
    number::{Decimal, Number, Radix},
};

pub(in crate::lex::tokenize) struct Identifier<'me, 'txt> {
    exactness: Option<Exactness>,
    peculiar_state: Option<PeculiarState>,
    scan: &'me mut Scanner<'txt>,
    start: ScanItem<'txt>,
}

impl<'me, 'txt> Identifier<'me, 'txt> {
    pub(in crate::lex::tokenize) fn new(
        scan: &'me mut Scanner<'txt>,
        start: ScanItem<'txt>,
    ) -> Self {
        Self::with_exactness(scan, start, None)
    }

    pub(super) fn with_exactness(
        scan: &'me mut Scanner<'txt>,
        start: ScanItem<'txt>,
        exactness: Option<Exactness>,
    ) -> Self {
        Self {
            exactness,
            peculiar_state: None,
            scan,
            start,
        }
    }

    pub(in crate::lex::tokenize) fn scan(mut self) -> TokenExtractResult {
        let first = self.start.1;
        if first == '|' {
            VerbatimIdentifer::new(self.scan).scan()
        } else if Decimal.is_digit(first) {
            RealNumber::new(self.scan, self.start.0, self.exactness).scan()
        } else if is_peculiar_initial(first) {
            self.peculiar(first)
        } else if is_initial(first) {
            self.standard()
        } else {
            self.invalid(first)
        }
    }

    fn standard(&mut self) -> TokenExtractResult {
        while let Some(item) = self.scan.next_if_not_delimiter() {
            let ch = item.1;
            match ch {
                '+' | '-' => return self.maybe_infnan_complex(item, ComplexKind::Cartesian),
                '@' => return self.maybe_infnan_complex(item, ComplexKind::Polar),
                _ if is_standard(ch) => (),
                _ => return self.invalid(ch),
            }
        }
        let exactness = self.exactness;
        let txt = self.get_lexeme();
        Ok(super::numeric_label(txt, exactness)
            .unwrap_or_else(|| TokenKind::Identifier(txt.to_owned())))
    }

    fn peculiar(&mut self, ch: char) -> TokenExtractResult {
        self.classify_peculiar(ch);
        let next_ch = self.scan.char_if_not_delimiter();
        self.continue_peculiar(next_ch)
    }

    fn continue_peculiar(&mut self, next_ch: Option<char>) -> TokenExtractResult {
        if let Some(ch) = next_ch {
            if Decimal.is_digit(ch) {
                debug_assert!(self.peculiar_state.is_some());
                match self.peculiar_state.as_ref().unwrap() {
                    PeculiarState::DefiniteIdentifier => self.standard(),
                    // CASE: .<digit>
                    PeculiarState::MaybeFloat => {
                        RealNumber::try_float(self.scan, self.start.0, self.exactness).scan()
                    }
                    // CASE: +/-.<digit>
                    PeculiarState::MaybeSignedFloat => RealNumber::try_signed_float(
                        super::char_to_sign(self.start.1),
                        self.scan,
                        self.start.0,
                        self.exactness,
                    )
                    .scan(),
                    // CASE: +/-<digit>
                    PeculiarState::MaybeSignedNumber => RealNumber::try_signed_number(
                        super::char_to_sign(self.start.1),
                        self.scan,
                        self.start.0,
                        self.exactness,
                    )
                    .scan(),
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
            let txt = self.get_lexeme();
            debug_assert!(txt != ".");
            Ok(TokenKind::Identifier(txt.to_owned()))
        }
    }

    fn maybe_infnan_complex(&mut self, item: ScanItem, kind: ComplexKind) -> TokenExtractResult {
        if let Some(TokenKind::Literal(Literal::Number(Number::Real(real)))) =
            super::numeric_label(self.scan.lexeme(self.start.0..item.0), self.exactness)
        {
            let invalid_tok = match kind {
                ComplexKind::Cartesian => {
                    let result = Identifier::new(self.scan, item).scan();
                    if let Ok(TokenKind::Imaginary(imag)) = result {
                        return Ok(TokenKind::Literal(Literal::Number(Number::complex(
                            real, imag,
                        ))));
                    }
                    Some(result)
                }
                ComplexKind::Polar => {
                    if let Some(first) = self.scan.next_if_not_delimiter() {
                        let result = Identifier::new(self.scan, first).scan();
                        if let Ok(TokenKind::Literal(Literal::Number(Number::Real(rads)))) = result
                        {
                            return Ok(TokenKind::Literal(Literal::Number(Number::polar(
                                real, rads,
                            ))));
                        }
                        Some(result)
                    } else {
                        None
                    }
                }
            };
            if let Some(err @ Err(TokenErrorKind::IdentifierInvalid(_))) = invalid_tok {
                return err;
            }
        }
        self.rest_of_standard()
    }

    fn rest_of_standard(&mut self) -> TokenExtractResult {
        while let Some(ch) = self.scan.char_if_not_delimiter() {
            if !is_standard(ch) {
                return self.invalid(ch);
            }
        }
        Ok(TokenKind::Identifier(self.get_lexeme().to_owned()))
    }

    fn invalid(&mut self, ch: char) -> TokenExtractResult {
        Err(TokenErrorKind::IdentifierInvalid(ch))
    }

    fn classify_peculiar(&mut self, ch: char) {
        // NOTE: only 3 cases: + | - | .
        self.peculiar_state = Some(match ch {
            '+' | '-' => match self.peculiar_state {
                None => PeculiarState::MaybeSignedNumber,
                _ => PeculiarState::DefiniteIdentifier,
            },
            '.' => match self.peculiar_state {
                Some(PeculiarState::MaybeSignedNumber) => PeculiarState::MaybeSignedFloat,
                None => PeculiarState::MaybeFloat,
                _ => PeculiarState::DefiniteIdentifier,
            },
            _ => unreachable!(),
        });
    }

    fn get_lexeme(&mut self) -> &'txt str {
        self.scan.current_lexeme_at(self.start.0)
    }
}

pub(in crate::lex::tokenize) struct PeriodIdentifier<'me, 'txt>(Identifier<'me, 'txt>);

impl<'me, 'txt> PeriodIdentifier<'me, 'txt> {
    pub(in crate::lex::tokenize) fn new(
        scan: &'me mut Scanner<'txt>,
        start: ScanItem<'txt>,
    ) -> Self {
        debug_assert_eq!(start.1, '.');
        let mut me = Self(Identifier::new(scan, start));
        me.0.classify_peculiar(start.1);
        me
    }

    pub(in crate::lex::tokenize) fn scan(mut self, next: char) -> TokenExtractResult {
        self.0.continue_peculiar(Some(next))
    }
}

pub(in crate::lex::tokenize) type VerbatimIdentifer<'me, 'txt, M> =
    FreeText<'me, 'txt, IdentifierPolicy<M>>;

impl<'me, 'txt> VerbatimIdentifer<'me, 'txt, ContinueIdentifier> {
    pub(in crate::lex::tokenize) fn cont(scan: &'me mut Scanner<'txt>) -> Self {
        Self::init(scan, IdentifierPolicy(ContinueIdentifier))
    }
}

impl<'me, 'txt> VerbatimIdentifer<'me, 'txt, DiscardIdentifier> {
    pub(in crate::lex::tokenize) fn cleanup(scan: &'me mut Scanner<'txt>) -> Self {
        Self::init(scan, IdentifierPolicy(DiscardIdentifier))
    }
}

impl<'me, 'txt> VerbatimIdentifer<'me, 'txt, StartIdentifier> {
    fn new(scan: &'me mut Scanner<'txt>) -> Self {
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
