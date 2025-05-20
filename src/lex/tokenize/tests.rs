mod tokenizer;
mod tokenstream;

use super::*;
use crate::{
    constant::Constant,
    testutil::{err_or_fail, ok_or_fail, some_or_fail},
    txt::TxtSpan,
};
