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

pub(crate) use {extract_or_fail, ok_or_fail};
