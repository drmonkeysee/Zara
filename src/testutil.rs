macro_rules! extract_or_fail {
    ($val:expr, $variant:path) => {{
        assert!(matches!($val, $variant(..)));
        if let $variant(inner) = $val {
            inner
        } else {
            unreachable!();
        }
    }};
}

macro_rules! ok_or_fail {
    ($val:expr) => {{
        assert!($val.is_ok());
        $val.unwrap()
    }};
}

macro_rules! err_or_fail {
    ($val:expr) => {{
        assert!($val.is_err());
        $val.unwrap_err()
    }};
}

pub(crate) use {err_or_fail, extract_or_fail, ok_or_fail};
