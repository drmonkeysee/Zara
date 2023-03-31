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
    tokens: Vec<Lexeme>,
}

#[derive(Debug)]
struct Lexeme {
    token: Token,
    word: Range<usize>,
}

// TODO: include label? #{n}={datum}, #{n}#
#[derive(Debug)]
enum Token {
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
