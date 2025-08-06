macro_rules! extract_or_fail {
    ($exp:expr, $variant:path) => {{
        let var = $exp;
        assert!(matches!(var, $variant(..)));
        if let $variant(inner) = var {
            inner
        } else {
            unreachable!();
        }
    }};
}

macro_rules! ok_or_fail {
    ($exp:expr) => {{
        let var = $exp;
        assert!(var.is_ok());
        var.unwrap()
    }};
}

macro_rules! err_or_fail {
    ($exp:expr) => {{
        let var = $exp;
        assert!(var.is_err());
        var.unwrap_err()
    }};
}

macro_rules! some_or_fail {
    ($exp:expr) => {{
        let var = $exp;
        assert!(var.is_some());
        var.unwrap()
    }};
}

use crate::{
    eval::{Binding, Frame, Namespace, System},
    lex::{Token, TokenKind, TokenLine},
    string::SymbolTable,
    syntax::{ExpressionTree, Parser, ParserOutput, Sequence},
    txt::{LineNumber, TextContext, TextLine},
};
use std::{iter, path::Path, rc::Rc};
pub(crate) use {err_or_fail, extract_or_fail, ok_or_fail, some_or_fail};

pub(crate) fn make_textline() -> TextLine {
    make_textline_no(1)
}

pub(crate) fn make_textline_no(lineno: LineNumber) -> TextLine {
    TextLine {
        ctx: TextContext {
            name: "mylib".to_owned(),
            path: Some(Path::new("lib/mylib.scm").to_path_buf()),
        }
        .into(),
        line: "line of source code".to_owned(),
        lineno,
    }
}

pub(crate) fn make_tokenline(kinds: impl IntoIterator<Item = TokenKind>) -> TokenLine {
    make_tokenline_no(kinds, 1)
}

pub(crate) fn make_tokenline_no(
    kinds: impl IntoIterator<Item = TokenKind>,
    lineno: LineNumber,
) -> TokenLine {
    let tokens = kinds.into_iter().enumerate().map(|(i, kind)| Token {
        kind,
        span: i..i + 1,
    });
    TokenLine(tokens.collect(), make_textline_no(lineno))
}

pub(crate) struct TestEnv {
    pub(crate) binding: Rc<Binding>,
    pub(crate) symbols: SymbolTable,
    pub(crate) system: System,
}

impl TestEnv {
    pub(crate) fn new_frame(&self) -> Frame {
        Frame {
            scope: Rc::clone(&self.binding),
            sym: &self.symbols,
            sys: &self.system,
        }
    }

    pub(crate) fn new_namespace(&self) -> Namespace {
        Namespace(self.new_frame())
    }
}

impl Default for TestEnv {
    fn default() -> Self {
        Self {
            binding: Binding::default().into(),
            symbols: SymbolTable::default(),
            system: System::new(iter::empty()),
        }
    }
}

pub(crate) fn procedure_body(tokens: impl IntoIterator<Item = TokenKind>) -> Sequence {
    let mut et = ExpressionTree::default();
    let env = TestEnv::default();

    let r = et.parse([make_tokenline(tokens)].into(), env.new_namespace());

    extract_or_fail!(ok_or_fail!(r), ParserOutput::Complete)
}

pub(crate) fn empty_procedure_body() -> Sequence {
    procedure_body([])
}
