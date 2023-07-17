mod tokenize;
mod tokens;

pub(super) use self::tokens::{Token, TokenKind};
use self::{tokenize::TokenStream, tokens::TokenError};
use crate::txt::TextContext;
use std::fmt::{Display, Formatter};

type LexerResult = Result<Vec<Token>, LexerError>;

pub(super) fn tokenize(ctx: &TextContext) -> LexerResult {
    let mut errors: Vec<TokenError> = Vec::new();
    let tokens = TokenStream::on(&ctx.line)
        .filter_map(|tr| tr.map_err(|err| errors.push(err)).ok())
        .collect();
    if errors.is_empty() {
        Ok(tokens)
    } else {
        // TODO: can this clone be removed in favor of packaging errors at lib level
        Err(LexerError(errors, ctx.clone()))
    }
}

#[derive(Debug)]
pub struct LexerError(Vec<TokenError>, TextContext);

impl LexerError {
    pub(crate) fn verbose_display(&self) -> VerboseLexerError<'_> {
        VerboseLexerError(self)
    }
}

pub(crate) struct VerboseLexerError<'a>(&'a LexerError);

impl Display for VerboseLexerError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: lex/parse/eval err types will likely have to be unified into a (specific_err, span, ctx) pairing
        let LexerError(errs, ctx) = self.0;
        write!(f, "{}:{}", ctx.library, ctx.lineno)?;
        write!(f, "{}\n", ctx.filename.as_ref().map_or("", String::as_str))?;
        write!(f, "\t{}\n", ctx.line)?;
        write!(f, "\t{}\n", "           ^^^^^^^               ^^^^^^^^")?;
        for err in errs {
            write!(f, "{}:{:?}", err.span.start, err)?;
        }
        Ok(())
    }
}
