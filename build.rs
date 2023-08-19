use std::{
    io,
    io::{Result, Write},
    process::{Command, Output},
};

fn main() {
    set_env_compiler_version();
    set_env_git_hash();
}

fn set_env_compiler_version() {
    set_env_from_output(
        "ZARA_COMPILER_VERSION",
        Command::new("rustc").arg("-V").output(),
    )
}

fn set_env_git_hash() {
    set_env_from_output(
        "ZARA_GIT_HASH",
        Command::new("git")
            .args(["rev-parse", "--short", "HEAD"])
            .output(),
    )
}

fn set_env_from_output(var: &str, result: Result<Output>) {
    let output = result.expect("fatal error executing command");
    if output.status.success() {
        let val = String::from_utf8(output.stdout).expect("command output conversion failure");
        println!("cargo:rustc-env={var}={val}");
    } else {
        io::stderr()
            .write_all(&output.stderr)
            .expect("print stderr failure");
        panic!("command run error: {:?}", output.status);
    }
}
