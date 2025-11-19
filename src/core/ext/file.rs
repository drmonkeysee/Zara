use crate::{
    eval::{EvalResult, Frame},
    value::{Condition, TypeName, Value},
};
use std::fs;

pub(super) fn load(env: &Frame) {
    // TODO ADD
    /*
    is-directory?
    is-file?
    canoncalize-path
    copy-file
    rename-file
    delete-directories
    metadata?
    */

    super::bind_intrinsic(env, "make-directory", 1..1, mk_dir);
    super::bind_intrinsic(env, "make-directories", 1..1, mk_dirs);
    super::bind_intrinsic(env, "delete-directory", 1..1, rm_dir);
}

fn mk_dir(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::create_dir(p))
}

fn mk_dirs(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::create_dir_all(p))
}

fn rm_dir(args: &[Value], env: &Frame) -> EvalResult {
    super::fs_cmd(super::first(args), env, |p| fs::remove_dir(p))
}
