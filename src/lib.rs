mod lex;

use crate::lex::TokenStream;

pub fn eval(text: String) -> TokenStream {
    lex::tokenize(text)
}
