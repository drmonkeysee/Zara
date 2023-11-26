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

pub(crate) use extract_or_fail;
