mod tokenizer;
mod tokenstream;

use super::*;
use crate::{
    lex::token::{Token, TokenError, TokenErrorKind},
    literal::Literal,
    testutil::{err_or_fail, ok_or_fail},
};
use std::ops::Range;
