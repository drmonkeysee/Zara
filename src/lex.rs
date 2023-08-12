mod tokenize;
mod tokens;

pub(crate) use self::tokens::{Token, TokenKind};
use self::{tokenize::TokenStream, tokens::TokenError};
use crate::txt::{TextLine, TextSource};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

pub(crate) type LexerResult = Result<Vec<LexLine>, LexerError>;
type LexerLineResult = Result<LexLine, LexerError>;

pub(crate) fn tokenize(src: &mut impl TextSource) -> LexerResult {
    src.map(tokenize_line).collect::<LexerResult>()
}

fn tokenize_line(text: TextLine) -> LexerLineResult {
    let mut errors: Vec<TokenError> = Vec::new();
    let tokens = TokenStream::on(&text.line)
        .filter_map(|tr| tr.map_err(|err| errors.push(err)).ok())
        .collect();
    if errors.is_empty() {
        Ok(LexLine(tokens, text))
    } else {
        Err(LexerError(errors, text))
    }
}

pub(crate) struct LexLine(Vec<Token>, TextLine);

impl Display for LexLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let LexLine(tokens, txt) = self;
        let token_txt: Vec<_> = tokens
            .iter()
            .map(|t| format_token_with_source(t, txt))
            .collect();
        write!(f, "{}:{}", txt.lineno, token_txt.join(", "))
    }
}

fn format_token_with_source(t: &Token, txt: &TextLine) -> String {
    format!(
        "{}(\"{}\")",
        t,
        txt.line
            .get(t.span.clone())
            .unwrap_or("#<token-invalid-range>")
    )
}

// NOTE: used by .flatten()
impl IntoIterator for LexLine {
    type Item = Token;
    type IntoIter = <Vec<Token> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug)]
pub struct LexerError(Vec<TokenError>, TextLine);

impl LexerError {
    pub(crate) fn verbose_display(&self) -> VerboseLexerError<'_> {
        VerboseLexerError(self)
    }
}

pub(crate) struct VerboseLexerError<'a>(&'a LexerError);

impl Display for VerboseLexerError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // TODO: lex/parse/eval err types will likely have to be unified into a (specific_err, span, ctx) pairing
        let LexerError(errs, txtline) = self.0;
        write!(f, "{}:{}", txtline.ctx.name, txtline.lineno)?;
        if let Some(p) = &txtline.ctx.path {
            write!(f, " ({})", p)?;
        }
        write!(f, "\n\t{}\n", txtline.line)?;

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
    use super::*;
    use crate::txt::TextContext;

    mod result {
        use super::*;

        #[test]
        fn display_empty_line() {
            let line = LexLine(Vec::new(), make_textline());

            assert_eq!(line.to_string(), "1:");
        }

        #[test]
        fn display_single_token() {
            let line = LexLine(
                vec![Token {
                    kind: TokenKind::ParenLeft,
                    span: 8..14,
                }],
                make_textline(),
            );

            assert_eq!(line.to_string(), "1:LPAREN[8..14](\"source\")");
        }

        #[test]
        fn display_multiple_tokens() {
            let line = LexLine(
                vec![
                    Token {
                        kind: TokenKind::ParenLeft,
                        span: 0..4,
                    },
                    Token {
                        kind: TokenKind::ParenRight,
                        span: 5..7,
                    },
                    Token {
                        kind: TokenKind::ParenLeft,
                        span: 8..14,
                    },
                ],
                make_textline(),
            );

            assert_eq!(
                line.to_string(),
                "1:LPAREN[0..4](\"line\"), \
                RPAREN[5..7](\"of\"), \
                LPAREN[8..14](\"source\")"
            );
        }

        #[test]
        fn display_invalid_span() {
            let line = LexLine(
                vec![Token {
                    kind: TokenKind::ParenLeft,
                    span: 8..4,
                }],
                make_textline(),
            );

            assert_eq!(
                line.to_string(),
                "1:LPAREN[8..4](\"#<token-invalid-range>\")"
            );
        }

        #[test]
        fn display_span_out_of_range() {
            let line = LexLine(
                vec![Token {
                    kind: TokenKind::ParenLeft,
                    span: 8..50,
                }],
                make_textline(),
            );

            assert_eq!(
                line.to_string(),
                "1:LPAREN[8..50](\"#<token-invalid-range>\")"
            );
        }
    }

    mod error {
        use self::tokens::TokenErrorKind;
        use super::*;

        #[test]
        fn display_empty_errors() {
            let err = LexerError(Vec::new(), make_textline());

            assert_eq!(
                err.verbose_display().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n"
            );
        }

        #[test]
        fn display_single_error() {
            let err = LexerError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 5..7,
                }],
                make_textline(),
            );

            assert_eq!(
                err.verbose_display().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t     ^^\n\
                6: unimplemented tokenization: \"myerr\"\n"
            );
        }

        #[test]
        fn display_single_error_at_beginning_of_line() {
            let err = LexerError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 0..4,
                }],
                make_textline(),
            );

            assert_eq!(
                err.verbose_display().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t^^^^\n\
                1: unimplemented tokenization: \"myerr\"\n"
            );
        }

        #[test]
        fn display_multiple_errors() {
            let err = LexerError(
                vec![
                    TokenError {
                        kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                        span: 5..7,
                    },
                    TokenError {
                        kind: TokenErrorKind::CharacterExpected,
                        span: 15..19,
                    },
                ],
                make_textline(),
            );

            assert_eq!(
                err.verbose_display().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t     ^^        ^^^^\n\
                6: unimplemented tokenization: \"myerr\"\n\
                16: expected character literal\n"
            );
        }

        #[test]
        fn display_single_error_no_filename() {
            let err = LexerError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 5..7,
                }],
                TextLine {
                    ctx: TextContext {
                        name: "mylib".to_owned(),
                        path: None,
                    }
                    .into(),
                    line: "line of source code".to_owned(),
                    lineno: 1,
                },
            );

            assert_eq!(
                err.verbose_display().to_string(),
                "mylib:1\n\
                \tline of source code\n\
                \t     ^^\n\
                6: unimplemented tokenization: \"myerr\"\n"
            );
        }

        #[test]
        fn display_single_error_invalid_span() {
            let err = LexerError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 5..2,
                }],
                make_textline(),
            );

            assert_eq!(
                err.verbose_display().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t\n\
                6: unimplemented tokenization: \"myerr\"\n"
            );
        }

        #[test]
        fn display_single_error_span_out_of_range() {
            let err = LexerError(
                vec![TokenError {
                    kind: TokenErrorKind::Unimplemented("myerr".to_owned()),
                    span: 15..25,
                }],
                make_textline(),
            );

            assert_eq!(
                err.verbose_display().to_string(),
                "mylib:1 (lib/mylib.scm)\n\
                \tline of source code\n\
                \t               ^^^^^^^^^^\n\
                16: unimplemented tokenization: \"myerr\"\n"
            );
        }
    }

    fn make_textline() -> TextLine {
        TextLine {
            ctx: TextContext {
                name: "mylib".to_owned(),
                path: Some("lib/mylib.scm".to_owned()),
            }
            .into(),
            line: "line of source code".to_owned(),
            lineno: 1,
        }
    }
}
