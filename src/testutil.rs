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
    eval::{Binding, Frame, SymbolTable, System},
    txt::{LineNumber, TextContext, TextLine},
};
use std::{iter, path::Path};
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

pub(crate) struct TestEnv {
    pub(crate) binding: Binding,
    pub(crate) symbols: SymbolTable,
    pub(crate) system: System,
}

impl TestEnv {
    pub(crate) fn new_frame(&mut self) -> Frame {
        Frame {
            scope: &mut self.binding,
            sym: &self.symbols,
            sys: &self.system,
        }
    }
}

impl Default for TestEnv {
    fn default() -> Self {
        Self {
            binding: Binding::default(),
            symbols: SymbolTable,
            system: System::new(iter::empty()),
        }
    }
}
