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
    eval::{Frame, SymbolTable, System},
    txt::{LineNumber, TextContext, TextLine},
};
use std::{path::Path, rc::Rc};
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
    pub(crate) frame: Rc<Frame>,
    pub(crate) symbols: Rc<SymbolTable>,
    pub(crate) system: Rc<System>,
}

impl Default for TestEnv {
    fn default() -> Self {
        let sym = SymbolTable::default().into();
        let sys = System::new().into();
        Self {
            frame: Frame::root(Rc::downgrade(&sym), Rc::downgrade(&sys)).into(),
            symbols: sym,
            system: sys,
        }
    }
}
