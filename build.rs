use std::{
    io,
    io::{Result, Write},
    process::{Command, Output},
};

fn main() {
    set_env_compiler_version();
    set_env_dependencies();
}

fn set_env_compiler_version() {
    set_env_from_output(
        "ZARA_COMPILER_VERSION",
        Command::new("rustc").arg("-V").output(),
    );
}

fn set_env_dependencies() {
    set_env_from_converted_output(
        "ZARA_DEPENDENCIES",
        Command::new("cargo")
            .args(["tree", "-e", "normal", "--depth", "1", "--prefix", "none"])
            .output(),
        |val| val.split("\n").skip(1).collect::<Vec<_>>().join(","),
    );
}

fn set_env_from_output(var: &str, result: Result<Output>) {
    set_env_from_converted_output(var, result, |val| val);
}

fn set_env_from_converted_output(
    var: &str,
    result: Result<Output>,
    convert: impl FnOnce(String) -> String,
) {
    let output = result.expect("fatal error executing command");
    if output.status.success() {
        let val = String::from_utf8(output.stdout).expect("command output conversion failure");
        let val = convert(val);
        println!("cargo:rustc-env={var}={val}");
    } else {
        io::stderr()
            .write_all(&output.stderr)
            .expect("print stderr failure");
        panic!("command run error: {:?}", output.status);
    }
}
