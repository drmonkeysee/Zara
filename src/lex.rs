mod tokenize;
mod tokens;

pub(crate) use self::tokens::{Token, TokenKind};
use self::{tokenize::TokenStream, tokens::TokenError};
use crate::txt::{TextLine, TextSource};
use std::{
    error::Error,
    fmt,
    fmt::{Display, Formatter},
};

#[derive(Debug)]
pub struct LexLine(Vec<Token>, TextLine);

impl Display for LexLine {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let LexLine(tokens, txt) = self;
        let token_txt = tokens
            .iter()
            .map(|t| TokenWithSource(t, txt).to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}:{}", txt.lineno, token_txt)
    }
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
    pub(crate) fn extended_display(&self) -> ExtendedLexerError<'_> {
        ExtendedLexerError(self)
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("Fatal error: tokenization failure")
    }
}

impl Error for LexerError {}

pub(crate) type LexerResult = Result<Vec<LexLine>, LexerError>;

pub(crate) struct ExtendedLexerError<'a>(&'a LexerError);

impl Display for ExtendedLexerError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        // TODO: lex/parse/eval err types will likely have to be unified into a (specific_err, span, ctx) pairing
        let LexerError(errs, txtline) = self.0;
        write!(f, "{}:{}", txtline.ctx.name, txtline.lineno)?;
        if let Some(p) = &txtline.ctx.path {
            write!(f, " ({p})")?;
        }
        writeln!(f, "\n\t{}", txtline.line)?;

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
            writeln!(f, "{}: {err}", err.span.start + 1)?;
        }
        Ok(())
    }
}

pub(crate) struct DisplayLexLines<'a>(pub(crate) &'a [LexLine]);

impl DisplayLexLines<'_> {
    fn flatten_to_string(&self, cvt: impl FnMut(&LexLine) -> String) -> String {
        self.0.iter().map(cvt).collect()
    }
}

impl Display for DisplayLexLines<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.0.len() < 2 {
            write!(f, "[{}]", self.flatten_to_string(|ln| ln.to_string()))
        } else {
            write!(
                f,
                "[\n{}]",
                self.flatten_to_string(|ln| format!("\t{ln},\n"))
            )
        }
    }
}

pub(crate) struct ExtendedDisplayLexLines<'a>(pub(crate) &'a [LexLine]);

impl Display for ExtendedDisplayLexLines<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for line in self.0 {
            ExtendedLexLine(line).fmt(f)?;
        }
        Ok(())
    }
}

pub(crate) fn tokenize(src: &mut impl TextSource) -> LexerResult {
    src.map(tokenize_line).collect()
}

type LexerLineResult = Result<LexLine, LexerError>;

struct ExtendedLexLine<'a>(&'a LexLine);

impl Display for ExtendedLexLine<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let LexLine(tokens, txt) = self.0;
        for token in tokens {
            ExtendedTokenWithSource(token, txt).fmt(f)?
        }
        Ok(())
    }
}

struct TokenWithSource<'a>(&'a Token, &'a TextLine);

impl Display for TokenWithSource<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self(t, txt) = *self;
        write!(
            f,
            "{t}(\"{}\")",
            txt.line
                .get(t.span.clone())
                .unwrap_or("#<token-invalid-range>")
        )
    }
}

struct ExtendedTokenWithSource<'a>(&'a Token, &'a TextLine);

impl Display for ExtendedTokenWithSource<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Self(t, txt) = *self;
        writeln!(
            f,
            "{:20}{:30}\"{}\"",
            format!("{}:{:?}", txt.lineno, t.span),
            t.kind.to_string(),
            txt.line.get(t.span.clone()).unwrap_or("INVALID RANGE")
        )
    }
}

fn tokenize_line(text: TextLine) -> LexerLineResult {
    let mut errors: Vec<TokenError> = Vec::new();
    let tokens = TokenStream::new(&text.line)
        .filter_map(|tr| tr.map_err(|err| errors.push(err)).ok())
        .collect();
    if errors.is_empty() {
        Ok(LexLine(tokens, text))
    } else {
        Err(LexerError(errors, text))
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
                err.extended_display().to_string(),
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
                err.extended_display().to_string(),
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
                err.extended_display().to_string(),
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
                err.extended_display().to_string(),
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
                err.extended_display().to_string(),
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
                err.extended_display().to_string(),
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
                err.extended_display().to_string(),
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

    mod display {
        use super::*;
        use crate::literal::Literal;

        #[test]
        fn display_empty_token_stream() {
            let lines = Vec::new();

            let target = DisplayLexLines(&lines);

            assert_eq!(target.to_string(), "[]");
        }

        #[test]
        fn display_token_stream() {
            let lines = vec![LexLine(
                vec![
                    Token {
                        kind: TokenKind::ParenLeft,
                        span: 0..1,
                    },
                    Token {
                        kind: TokenKind::Literal(Literal::Boolean(false)),
                        span: 1..3,
                    },
                    Token {
                        kind: TokenKind::ParenRight,
                        span: 3..4,
                    },
                ],
                TextLine {
                    ctx: TextContext {
                        name: "mylib".to_owned(),
                        path: None,
                    }
                    .into(),
                    line: "(#f)".to_owned(),
                    lineno: 1,
                },
            )];

            let target = DisplayLexLines(&lines);

            assert_eq!(
                target.to_string(),
                "[1:LPAREN[0..1](\"(\"), LITERAL<Boolean(false)>[1..3](\"#f\"), RPAREN[3..4](\")\")]"
            );
        }

        #[test]
        fn display_multiline_token_stream() {
            let lines = vec![
                LexLine(
                    vec![
                        Token {
                            kind: TokenKind::ParenLeft,
                            span: 0..1,
                        },
                        Token {
                            kind: TokenKind::Literal(Literal::Boolean(false)),
                            span: 1..3,
                        },
                        Token {
                            kind: TokenKind::ParenRight,
                            span: 3..4,
                        },
                    ],
                    TextLine {
                        ctx: TextContext {
                            name: "mylib".to_owned(),
                            path: None,
                        }
                        .into(),
                        line: "(#f)".to_owned(),
                        lineno: 1,
                    },
                ),
                LexLine(
                    vec![
                        Token {
                            kind: TokenKind::ParenLeft,
                            span: 0..1,
                        },
                        Token {
                            kind: TokenKind::Literal(Literal::Boolean(true)),
                            span: 2..4,
                        },
                        Token {
                            kind: TokenKind::ParenRight,
                            span: 5..6,
                        },
                    ],
                    TextLine {
                        ctx: TextContext {
                            name: "mylib".to_owned(),
                            path: None,
                        }
                        .into(),
                        line: "( #t )".to_owned(),
                        lineno: 2,
                    },
                ),
                LexLine(
                    vec![
                        Token {
                            kind: TokenKind::ParenLeft,
                            span: 0..1,
                        },
                        Token {
                            kind: TokenKind::Literal(Literal::Character('a')),
                            span: 1..4,
                        },
                        Token {
                            kind: TokenKind::ParenRight,
                            span: 4..5,
                        },
                    ],
                    TextLine {
                        ctx: TextContext {
                            name: "mylib".to_owned(),
                            path: None,
                        }
                        .into(),
                        line: "(#\\a)".to_owned(),
                        lineno: 3,
                    },
                ),
            ];

            let target = DisplayLexLines(&lines);

            assert_eq!(
                target.to_string(),
                "[\n\
                \t1:LPAREN[0..1](\"(\"), LITERAL<Boolean(false)>[1..3](\"#f\"), RPAREN[3..4](\")\"),\n\
                \t2:LPAREN[0..1](\"(\"), LITERAL<Boolean(true)>[2..4](\"#t\"), RPAREN[5..6](\")\"),\n\
                \t3:LPAREN[0..1](\"(\"), LITERAL<Character('a')>[1..4](\"#\\a\"), RPAREN[4..5](\")\"),\n\
                ]"
            );
        }
    }
}
