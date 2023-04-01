use std::ops::Range;

pub fn tokenize(text: String) -> TokenStream {
    TokenStream {
        text: text,
        tokens: Vec::new(),
    }
}

#[derive(Debug)]
pub struct TokenStream {
    text: String,
    tokens: Vec<Token>,
}

#[derive(Debug)]
struct Token {
    kind: TokenKind,
    lexeme: Range<usize>,
}

// TODO: include label? #{n}={datum}, #{n}#
#[derive(Debug)]
enum TokenKind {
    Boolean,        // #t|#f
    Character,      // \#{character}
    Identifier,     // {name}
    Number,         // integer|rational|real|complex
    OpenByteVector, // #u8(
    OpenVector,     // #(
    Pair,           // .
    ParenLeft,      // (
    ParenRight,     // )
    QuasiQuote,     // `
    Quote,          // '
    String,         // "{characters}"
    Unquote,        // ,
    UnquoteSplice,  // ,@
}
