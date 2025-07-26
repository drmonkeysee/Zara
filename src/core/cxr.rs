// (scheme cxr)
use super::{first, pcar, pcdr};
use crate::{
    eval::{EvalResult, Frame},
    value::Value,
};

pub(super) fn load(env: &mut Frame) {
    super::bind_intrinsic(env, "caaar", 1..1, caaar);
    super::bind_intrinsic(env, "caadr", 1..1, caadr);
    super::bind_intrinsic(env, "cadar", 1..1, cadar);
    super::bind_intrinsic(env, "caddr", 1..1, caddr);
    super::bind_intrinsic(env, "cdaar", 1..1, cdaar);
    super::bind_intrinsic(env, "cdadr", 1..1, cdadr);
    super::bind_intrinsic(env, "cddar", 1..1, cddar);
    super::bind_intrinsic(env, "cdddr", 1..1, cdddr);
    super::bind_intrinsic(env, "caaaar", 1..1, caaaar);
    super::bind_intrinsic(env, "caaadr", 1..1, caaadr);
    super::bind_intrinsic(env, "caadar", 1..1, caadar);
    super::bind_intrinsic(env, "caaddr", 1..1, caaddr);
    super::bind_intrinsic(env, "cadaar", 1..1, cadaar);
    super::bind_intrinsic(env, "cadadr", 1..1, cadadr);
    super::bind_intrinsic(env, "caddar", 1..1, caddar);
    super::bind_intrinsic(env, "cadddr", 1..1, cadddr);
    super::bind_intrinsic(env, "cdaaar", 1..1, cdaaar);
    super::bind_intrinsic(env, "cdaadr", 1..1, cdaadr);
    super::bind_intrinsic(env, "cdadar", 1..1, cdadar);
    super::bind_intrinsic(env, "cdaddr", 1..1, cdaddr);
    super::bind_intrinsic(env, "cddaar", 1..1, cddaar);
    super::bind_intrinsic(env, "cddadr", 1..1, cddadr);
    super::bind_intrinsic(env, "cdddar", 1..1, cdddar);
    super::bind_intrinsic(env, "cddddr", 1..1, cddddr);
}

cadr_func!(caaar, a, a, a);
cadr_func!(caadr, a, a, d);
cadr_func!(cadar, a, d, a);
cadr_func!(caddr, a, d, d);
cadr_func!(cdaar, d, a, a);
cadr_func!(cdadr, d, a, d);
cadr_func!(cddar, d, d, a);
cadr_func!(cdddr, d, d, d);
cadr_func!(caaaar, a, a, a, a);
cadr_func!(caaadr, a, a, a, d);
cadr_func!(caadar, a, a, d, a);
cadr_func!(caaddr, a, a, d, d);
cadr_func!(cadaar, a, d, a, a);
cadr_func!(cadadr, a, d, a, d);
cadr_func!(caddar, a, d, d, a);
cadr_func!(cadddr, a, d, d, d);
cadr_func!(cdaaar, d, a, a, a);
cadr_func!(cdaadr, d, a, a, d);
cadr_func!(cdadar, d, a, d, a);
cadr_func!(cdaddr, d, a, d, d);
cadr_func!(cddaar, d, d, a, a);
cadr_func!(cddadr, d, d, a, d);
cadr_func!(cdddar, d, d, d, a);
cadr_func!(cddddr, d, d, d, d);
