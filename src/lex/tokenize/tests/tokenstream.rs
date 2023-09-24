use super::*;
use std::ops::Range;

#[test]
fn empty_string() {
    let s = TokenStream::new("", None);

    let r: Vec<_> = s.collect();

    assert!(r.is_empty());
}

#[test]
fn whitespace() {
    let s = TokenStream::new("   \t  \r  \n  ", None);

    let r: Vec<_> = s.collect();

    assert!(r.is_empty());
}

#[test]
fn single_token() {
    let s = TokenStream::new("(", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 1);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::ParenLeft,
            span: Range { start: 0, end: 1 }
        })
    ));
}

#[test]
fn single_token_with_whitespace() {
    let s = TokenStream::new("  (   ", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 1);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::ParenLeft,
            span: Range { start: 2, end: 3 }
        })
    ));
}

#[test]
fn multiple_tokens() {
    let s = TokenStream::new("(#t)", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 3);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::ParenLeft,
            span: Range { start: 0, end: 1 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 1, end: 3 }
        })
    ));
    assert!(matches!(
        r[2],
        Ok(Token {
            kind: TokenKind::ParenRight,
            span: Range { start: 3, end: 4 }
        })
    ));
}

#[test]
fn multiple_tokens_with_whitespace() {
    let s = TokenStream::new("   (   #t    )   ", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 3);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::ParenLeft,
            span: Range { start: 3, end: 4 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 7, end: 9 }
        })
    ));
    assert!(matches!(
        r[2],
        Ok(Token {
            kind: TokenKind::ParenRight,
            span: Range { start: 13, end: 14 }
        })
    ));
}

#[test]
fn tokens_with_invalid_token() {
    let s = TokenStream::new("(#tdf)", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 3);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::ParenLeft,
            span: Range { start: 0, end: 1 }
        })
    ));
    assert!(matches!(
        r[1],
        Err(TokenError {
            kind: TokenErrorKind::BooleanExpected(true),
            span: Range { start: 1, end: 5 }
        })
    ));
    assert!(matches!(
        r[2],
        Ok(Token {
            kind: TokenKind::ParenRight,
            span: Range { start: 5, end: 6 }
        })
    ));
}

#[test]
fn tokens_with_unterminated_token() {
    let s = TokenStream::new("(#)", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 3);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::ParenLeft,
            span: Range { start: 0, end: 1 }
        })
    ));
    assert!(matches!(
        r[1],
        Err(TokenError {
            kind: TokenErrorKind::HashUnterminated,
            span: Range { start: 1, end: 2 }
        })
    ));
    assert!(matches!(
        r[2],
        Ok(Token {
            kind: TokenKind::ParenRight,
            span: Range { start: 2, end: 3 }
        })
    ));
}

#[test]
fn tokens_with_unterminated_token_to_whitespace() {
    let s = TokenStream::new("# #f", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 2);
    assert!(matches!(
        r[0],
        Err(TokenError {
            kind: TokenErrorKind::HashUnterminated,
            span: Range { start: 0, end: 1 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 2, end: 4 }
        })
    ));
}

#[test]
fn hash_is_a_token_boundary() {
    let s = TokenStream::new("#t#f", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 2);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 0, end: 2 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 2, end: 4 }
        })
    ));
}

#[test]
fn quote_is_a_token_boundary() {
    let s = TokenStream::new("#t'#f", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 3);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 0, end: 2 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Quote,
            span: Range { start: 2, end: 3 }
        })
    ));
    assert!(matches!(
        r[2],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 3, end: 5 }
        })
    ));
}

#[test]
fn quasiquote_is_a_token_boundary() {
    let s = TokenStream::new("#t`#f", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 3);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 0, end: 2 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Quasiquote,
            span: Range { start: 2, end: 3 }
        })
    ));
    assert!(matches!(
        r[2],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 3, end: 5 }
        })
    ));
}

#[test]
fn unquote_is_a_token_boundary() {
    let s = TokenStream::new("#t,#f", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 3);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 0, end: 2 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Unquote,
            span: Range { start: 2, end: 3 }
        })
    ));
    assert!(matches!(
        r[2],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 3, end: 5 }
        })
    ));
}

#[test]
fn pair_join_is_not_a_token_boundary() {
    let s = TokenStream::new("#t.#f", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 2);
    assert!(matches!(
        r[0],
        Err(TokenError {
            kind: TokenErrorKind::BooleanExpected(true),
            span: Range { start: 0, end: 3 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 3, end: 5 }
        })
    ));
}

#[test]
fn block_comment_fragment_uses_whole_line() {
    let s = TokenStream::new(
        "continued comment",
        Some(TokenContinuation::BlockComment(2)),
    );

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 1);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::CommentBlockFragment(2),
            span: Range { start: 0, end: 17 }
        })
    ));
}

#[test]
fn block_comment_end_continues_tokenizing() {
    let s = TokenStream::new(
        "end comment |# #f",
        Some(TokenContinuation::BlockComment(0)),
    );

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 2);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::CommentBlockEnd,
            span: Range { start: 0, end: 14 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 15, end: 17 }
        })
    ));
}

#[test]
fn string_fragment_uses_whole_line() {
    let s = TokenStream::new(
        "continued string",
        Some(TokenContinuation::StringLiteral(false)),
    );

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 1);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::StringFragment(false),
            span: Range { start: 0, end: 16 }
        })
    ));
}

#[test]
fn string_end_continues_tokenizing() {
    let s = TokenStream::new(
        "end string \" #f",
        Some(TokenContinuation::StringLiteral(false)),
    );

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 2);
    assert!(matches!(
        r[0],
        Ok(Token {
            kind: TokenKind::StringEnd,
            span: Range { start: 0, end: 12 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(false)),
            span: Range { start: 13, end: 15 }
        })
    ));
}

#[test]
fn finishes_parsing_string_if_error() {
    let s = TokenStream::new("\"foo \\e bar\" #t", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 3);
    assert!(matches!(
        r[0],
        Err(TokenError {
            kind: TokenErrorKind::StringEscapeInvalid(5, 'e'),
            span: Range { start: 5, end: 7 }
        })
    ));
    assert!(matches!(
        r[1],
        Ok(Token {
            kind: TokenKind::StringDiscard,
            span: Range { start: 7, end: 12 }
        })
    ));
    assert!(matches!(
        r[2],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 13, end: 15 }
        })
    ));
}

#[test]
fn multiple_string_errors() {
    let s = TokenStream::new("\"foo \\xdeadbeef; bar \\e baz\" #t", None);

    let r: Vec<_> = s.collect();

    assert_eq!(r.len(), 4);
    assert!(matches!(
        r[0],
        Err(TokenError {
            kind: TokenErrorKind::StringInvalidHex(5),
            span: Range { start: 5, end: 16 }
        })
    ));
    assert!(matches!(
        r[1],
        Err(TokenError {
            kind: TokenErrorKind::StringEscapeInvalid(21, 'e'),
            span: Range { start: 21, end: 23 }
        })
    ));
    assert!(matches!(
        r[2],
        Ok(Token {
            kind: TokenKind::StringDiscard,
            span: Range { start: 23, end: 28 }
        })
    ));
    assert!(matches!(
        r[3],
        Ok(Token {
            kind: TokenKind::Literal(Literal::Boolean(true)),
            span: Range { start: 29, end: 31 }
        })
    ));
}
