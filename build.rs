use std::{
    io::{self, Result, Write},
    process::{Command, Output},
};

fn main() {
    set_compiler_version_env();
    set_dependencies_env();
}

fn set_compiler_version_env() {
    set_env_from_output(
        "ZARA_COMPILER_VERSION",
        Command::new("rustc").arg("-V").output(),
    );
}

fn set_dependencies_env() {
    set_env_from_converted_output(
        "ZARA_DEPENDENCIES",
        Command::new("cargo")
            .args(["tree", "-e", "normal", "--depth", "1", "--prefix", "none"])
            .output(),
        |val| val.lines().skip(1).collect::<Vec<_>>().join(","),
    );
}

fn set_env_from_output(var: &str, result: Result<Output>) {
    set_env_from_converted_output(var, result, |val| val);
}

fn set_env_from_converted_output(
    name: &str,
    result: Result<Output>,
    convert: impl FnOnce(String) -> String,
) {
    let output = result.expect("fatal error executing command");
    if output.status.success() {
        let val = String::from_utf8(output.stdout).expect("command output conversion failure");
        let val = convert(val);
        println!("cargo:rustc-env={name}={val}");
    } else {
        io::stderr()
            .write_all(&output.stderr)
            .expect("print stderr failure");
        panic!("command run error: {:?}", output.status);
    }
}
