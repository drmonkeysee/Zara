mod tokenize;
mod tokens;

pub(crate) use self::tokens::{Token, TokenKind};
use self::{tokenize::TokenStream, tokens::TokenError};
use crate::txt::TextContext;
use std::fmt::{Display, Formatter};

type LexerResult = Result<Vec<Token>, LexerError>;

pub(crate) fn tokenize(ctx: &TextContext) -> LexerResult {
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
        if let Some(name) = &ctx.filename {
            write!(f, " ({})", name)?;
        }
        write!(f, "\n\t{}\n", ctx.line)?;

        if errs.is_empty() {
            return Ok(());
        }

        let mut cursor = 0;
        f.write_str("\t")?;
        for span in errs
            .iter()
            .filter_map(|err| (!err.span.is_empty()).then_some(&err.span))
        {
            write!(
                f,
                "{0:>1$}{2:^<3$}",
                "^",
                span.start + 1 - cursor,
                "",
                span.len() - 1
            )?;
            cursor = span.end;
        }
        f.write_str("\n")?;
        for err in errs {
            write!(f, "{}: {}\n", err.span.start + 1, err)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use self::tokens::TokenErrorKind;
    use super::*;

    #[test]
    fn display_empty_errors() {
        let err = LexerError(
            Vec::new(),
            TextContext {
                filename: Some(String::from("lib/mylib.scm")),
                library: String::from("mylib"),
                line: String::from("line of source code"),
                lineno: 1,
            },
        );

        assert_eq!(
            format!("{}", err.verbose_display()),
            "mylib:1 (lib/mylib.scm)\n\
            \tline of source code\n"
        );
    }

    #[test]
    fn display_single_error() {
        let err = LexerError(
            vec![TokenError {
                kind: TokenErrorKind::Unimplemented(String::from("myerr")),
                span: 5..7,
            }],
            TextContext {
                filename: Some(String::from("lib/mylib.scm")),
                library: String::from("mylib"),
                line: String::from("line of source code"),
                lineno: 1,
            },
        );

        assert_eq!(
            format!("{}", err.verbose_display()),
            "mylib:1 (lib/mylib.scm)\n\
            \tline of source code\n\
            \t     ^^\
            \n6: unimplemented tokenization: \"myerr\"\n"
        );
    }

    #[test]
    fn display_single_error_at_beginning_of_line() {
        let err = LexerError(
            vec![TokenError {
                kind: TokenErrorKind::Unimplemented(String::from("myerr")),
                span: 0..4,
            }],
            TextContext {
                filename: Some(String::from("lib/mylib.scm")),
                library: String::from("mylib"),
                line: String::from("line of source code"),
                lineno: 1,
            },
        );

        assert_eq!(
            format!("{}", err.verbose_display()),
            "mylib:1 (lib/mylib.scm)\n\
            \tline of source code\n\
            \t^^^^\
            \n1: unimplemented tokenization: \"myerr\"\n"
        );
    }

    #[test]
    fn display_multiple_errors() {
        let err = LexerError(
            vec![
                TokenError {
                    kind: TokenErrorKind::Unimplemented(String::from("myerr")),
                    span: 5..7,
                },
                TokenError {
                    kind: TokenErrorKind::CharacterExpected,
                    span: 15..19,
                },
            ],
            TextContext {
                filename: Some(String::from("lib/mylib.scm")),
                library: String::from("mylib"),
                line: String::from("line of source code"),
                lineno: 1,
            },
        );

        assert_eq!(
            format!("{}", err.verbose_display()),
            "mylib:1 (lib/mylib.scm)\n\
            \tline of source code\n\
            \t     ^^        ^^^^\
            \n6: unimplemented tokenization: \"myerr\"\
            \n16: expected character literal\n"
        );
    }

    #[test]
    fn display_single_error_no_filename() {
        let err = LexerError(
            vec![TokenError {
                kind: TokenErrorKind::Unimplemented(String::from("myerr")),
                span: 5..7,
            }],
            TextContext {
                filename: None,
                library: String::from("mylib"),
                line: String::from("line of source code"),
                lineno: 1,
            },
        );

        assert_eq!(
            format!("{}", err.verbose_display()),
            "mylib:1\n\
            \tline of source code\n\
            \t     ^^\
            \n6: unimplemented tokenization: \"myerr\"\n"
        );
    }

    #[test]
    fn display_single_error_invalid_span() {
        let err = LexerError(
            vec![TokenError {
                kind: TokenErrorKind::Unimplemented(String::from("myerr")),
                span: 5..2,
            }],
            TextContext {
                filename: Some(String::from("lib/mylib.scm")),
                library: String::from("mylib"),
                line: String::from("line of source code"),
                lineno: 1,
            },
        );

        assert_eq!(
            format!("{}", err.verbose_display()),
            "mylib:1 (lib/mylib.scm)\n\
            \tline of source code\n\
            \n6: unimplemented tokenization: \"myerr\"\n"
        );
    }
}
