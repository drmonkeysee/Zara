use std::{
    ops::{Add, AddAssign},
    path::PathBuf,
};
use zara::RunMode;

const ARGS_PREFIX: &str = "--";
const AST_SHORT: &str = "-S";
const AST_LONG: &str = "--syntax";
const AST_TOKEN_SHORT: &str = "-ST";
const HELP_SHORT: &str = "-h";
const HELP_LONG: &str = "--help";
const LIB_SHORT: &str = "-l";
const LIB_LONG: &str = "--lib";
const STDIN_SHORT: &str = "-";
const TOKEN_SHORT: &str = "-T";
const TOKEN_LONG: &str = "--tokens";
const TOKEN_AST_SHORT: &str = "-TS";
const VERSION_SHORT: &str = "-V";
const VERSION_LONG: &str = "--version";

const ARG_WIDTH: usize = 16;
const OPT_WIDTH: usize = 14;

#[derive(Debug, Default)]
pub(crate) struct Args {
    pub(crate) cmd: Cmd,
    pub(crate) input: Input,
    pub(crate) me: String,
    pub(crate) mode: RunMode,
    pub(crate) runargs: Option<Vec<String>>,
}

impl Args {
    pub(crate) fn parse(args: impl IntoIterator<Item = String>) -> Self {
        let mut args = args.into_iter();
        let this = Self {
            me: args
                .next()
                .unwrap_or_else(|| env!("CARGO_PKG_NAME").to_owned()),
            ..Default::default()
        };
        args.fold(this, Self::match_arg)
    }

    pub(crate) fn decompose(self) -> (Input, RunMode, Vec<String>) {
        let runargs = [if let Input::File(p) = &self.input {
            p.to_str().unwrap_or(self.me.as_str()).to_owned()
        } else {
            self.me.clone()
        }]
        .into_iter()
        .chain(
            self.runargs
                .as_ref()
                .into_iter()
                .flatten()
                .map(String::clone),
        )
        .collect();
        (self.input, self.mode, runargs)
    }

    fn has_run_target(&self) -> bool {
        !matches!(self.input, Input::Repl) || self.runargs.is_some()
    }

    fn expecting_prg_text(&self) -> bool {
        matches!(self.input, Input::Stdin) && self.runargs.is_none()
    }

    fn expecting_file_path(&self) -> bool {
        matches!(self.input, Input::Repl) && self.runargs.is_none()
    }

    fn push_runarg(&mut self, arg: String) {
        match self.runargs.as_mut() {
            None => {
                self.init_runargs();
                self.push_runarg(arg);
            }
            Some(ra) => ra.push(arg),
        }
    }

    fn init_runargs(&mut self) {
        self.runargs = Some(Vec::new());
    }

    fn match_arg(self, arg: String) -> Self {
        if self.has_run_target() {
            self.match_target_arg(arg)
        } else {
            self.match_command_arg(arg)
        }
    }

    fn match_target_arg(mut self, arg: String) -> Self {
        if arg == ARGS_PREFIX {
            self.init_runargs();
        } else if self.expecting_prg_text() {
            self.input = Input::Prg(arg);
        } else {
            self.push_runarg(arg);
        }
        self
    }

    fn match_command_arg(mut self, arg: String) -> Self {
        match arg.as_str() {
            ARGS_PREFIX => self.init_runargs(),
            AST_SHORT | AST_LONG => self.mode += RunMode::SyntaxTree,
            HELP_SHORT | HELP_LONG => self.cmd += Cmd::Help,
            TOKEN_SHORT | TOKEN_LONG => self.mode += RunMode::Tokenize,
            VERSION_SHORT | VERSION_LONG => self.cmd += Cmd::Version,
            STDIN_SHORT => self.input = Input::Stdin,
            AST_TOKEN_SHORT | TOKEN_AST_SHORT => {
                self.mode += RunMode::SyntaxTree + RunMode::Tokenize;
            }
            _ if self.expecting_file_path() => {
                let mut p = PathBuf::new();
                p.push(arg);
                self.input = Input::File(p);
            }
            _ => (),
        }
        self
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub(crate) enum Cmd {
    Help,
    #[default]
    Run,
    Version,
}

impl Add for Cmd {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match self {
            Self::Help => self,
            Self::Run => rhs,
            Self::Version => match rhs {
                Self::Help => rhs,
                _ => self,
            },
        }
    }
}

impl AddAssign for Cmd {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[derive(Debug, Default)]
pub(crate) enum Input {
    File(PathBuf),
    Prg(String),
    #[default]
    Repl,
    Stdin,
}

pub(crate) fn usage(me: &str) {
    println!("---=== {} Usage ===---", app_title());
    println!("{me} [options...] [command | file | -] [--] [args...]");
    println!();
    println!("options");
    println!(
        "  {:OPT_WIDTH$}: print abstract syntax tree",
        format!("{AST_SHORT}, {AST_LONG}")
    );
    println!(
        "  {:OPT_WIDTH$}: print tokenized input",
        format!("{TOKEN_SHORT}, {TOKEN_LONG}")
    );
    println!();
    println!("commands");
    println!(
        "  {:OPT_WIDTH$}: print usage",
        format!("{HELP_SHORT}, {HELP_LONG}")
    );
    println!(
        "  {:OPT_WIDTH$}: print version",
        format!("{VERSION_SHORT}, {VERSION_LONG}")
    );
    println!("  {LIB_SHORT} [name],");
    println!(
        "  {:OPT_WIDTH$}: run program from named library (omit name for usage)",
        format!("{LIB_LONG} [name]")
    );
    println!();
    println!("{:ARG_WIDTH$}: run program from script file", "file");
    println!(
        "{:ARG_WIDTH$}: run program string if provided, otherwise run program from stdin",
        format!("{STDIN_SHORT} [prg]")
    );
    println!("{:ARG_WIDTH$}: launch REPL", "<no target>");
    println!("{ARGS_PREFIX:ARG_WIDTH$}: disambiguate program args from other inputs,");
    println!(
        "{:ARG_WIDTH$}  such as when running from stdin or launching the REPL",
        ""
    );
    println!("{:ARG_WIDTH$}: arguments passed to program", "args");
}

pub(crate) fn version() {
    println!(
        "{} {} ({})",
        app_title(),
        env!("CARGO_PKG_VERSION"),
        env!("ZARA_COMPILER_VERSION")
    );
    let deps = env!("ZARA_DEPENDENCIES");
    if !deps.is_empty() {
        println!("{}", deps.replace(',', "\n"));
    }
}

fn app_title() -> String {
    let mut title = env!("CARGO_PKG_NAME").to_owned();
    if let Some(t) = title.get_mut(..1) {
        t.make_ascii_uppercase();
    }
    title
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cmd_ordering() {
        let c = Cmd::default();

        assert_eq!(c, Cmd::Run);
        assert_eq!(c + Cmd::Version, Cmd::Version);
        assert_eq!(c + Cmd::Help, Cmd::Help);
        assert_eq!(c + Cmd::Help + Cmd::Version, Cmd::Help);
    }

    #[test]
    fn empty_args() {
        let args: [String; 0] = [];

        let result = Args::parse(args);

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::Repl,
                me,
                mode: RunMode::Evaluate,
                runargs: None,
            } if me == "zara"
        ));
    }

    #[test]
    fn just_me() {
        let args = ["foo/me"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::Repl,
                me,
                mode: RunMode::Evaluate,
                runargs: None,
            } if me == "foo/me"
        ));
    }

    #[test]
    fn just_me_with_args() {
        let args = ["foo/me", "--", "arg1", "arg2", "--arg3=1"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::Repl,
                me,
                mode: RunMode::Evaluate,
                runargs: Some(_),
            } if me == "foo/me"
        ));
        assert_eq!(result.runargs.unwrap(), ["arg1", "arg2", "--arg3=1"]);
    }

    #[test]
    fn help() {
        let program = "foo/me";
        let cases = [[program, "-h"], [program, "--help"]];

        for case in cases {
            let result = Args::parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        cmd: Cmd::Help,
                        input: Input::Repl,
                        me,
                        mode: RunMode::Evaluate,
                        runargs: None,
                    } if me == program
                ),
                "Unexpected match for argument {}",
                case[1]
            );
        }
    }

    #[test]
    fn stdin() {
        let args = ["foo/me", "-"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::Stdin,
                me: _,
                mode: RunMode::Evaluate,
                runargs: None,
            },
        ));
    }

    #[test]
    fn stdin_prg() {
        let input = "stdin input";
        let args = ["foo/me", "-", input];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::Prg(s),
                me: _,
                mode: RunMode::Evaluate,
                runargs: None,
            } if s == input,
        ));
    }

    #[test]
    fn stdin_args() {
        let args = ["foo/me", "-", "--", "arg1", "arg2", "--arg3=1"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::Stdin,
                me: _,
                mode: RunMode::Evaluate,
                runargs: Some(_),
            },
        ));
        assert_eq!(result.runargs.unwrap(), ["arg1", "arg2", "--arg3=1"]);
    }

    #[test]
    fn stdin_prg_args() {
        let input = "stdin input";
        let args = ["foo/me", "-", input, "arg1", "arg2", "--arg3=1"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::Prg(s),
                me: _,
                mode: RunMode::Evaluate,
                runargs: Some(_),
            } if s == input,
        ));
        assert_eq!(result.runargs.unwrap(), ["arg1", "arg2", "--arg3=1"]);
    }

    #[test]
    fn syntax_tree() {
        let cases = [["foo/me", "-S"], ["foo/me", "--syntax"]];

        for case in cases {
            let result = Args::parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        cmd: Cmd::Run,
                        input: Input::Repl,
                        me: _,
                        mode: RunMode::SyntaxTree,
                        runargs: None,
                    }
                ),
                "Unexpected match for argument {}",
                case[1]
            );
        }
    }

    #[test]
    fn tokens() {
        let cases = [["foo/me", "-T"], ["foo/me", "--tokens"]];

        for case in cases {
            let result = Args::parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        cmd: Cmd::Run,
                        input: Input::Repl,
                        me: _,
                        mode: RunMode::Tokenize,
                        runargs: None,
                    }
                ),
                "Unexpected match for argument {}",
                case[1]
            );
        }
    }

    #[test]
    fn tokens_and_ast() {
        let cases = [["foo/me", "-ST"], ["foo/me", "-TS"]];

        for case in cases {
            let result = Args::parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        cmd: Cmd::Run,
                        input: Input::Repl,
                        me: _,
                        mode: RunMode::TokenTree,
                        runargs: None,
                    }
                ),
                "Unexpected match for argument {}",
                case[1]
            );
        }
    }

    #[test]
    fn version() {
        let cases = [["foo/me", "-V"], ["foo/me", "--version"]];

        for case in cases {
            let result = Args::parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        cmd: Cmd::Version,
                        input: Input::Repl,
                        me: _,
                        mode: RunMode::Evaluate,
                        runargs: None,
                    }
                ),
                "Unexpected match for argument {}",
                case[1]
            );
        }
    }

    #[test]
    fn file() {
        let args = ["foo/me", "my/file"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::File(p),
                me: _,
                mode: RunMode::Evaluate,
                runargs: None,
            } if p.to_str().unwrap() == "my/file"
        ));
    }

    #[test]
    fn file_args() {
        let args = ["foo/me", "my/file", "arg1", "arg2", "--arg3=1"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::File(p),
                me: _,
                mode: RunMode::Evaluate,
                runargs: Some(_),
            } if p.to_str().unwrap() == "my/file"
        ));
        assert_eq!(result.runargs.unwrap(), ["arg1", "arg2", "--arg3=1"]);
    }

    #[test]
    fn zara_options_ignored_after_run_target() {
        let args = ["foo/me", "my/file", "--version", "-T", "-ST"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::File(p),
                me,
                mode: RunMode::Evaluate,
                runargs: Some(_),
            } if me == "foo/me" && p.to_str().unwrap() == "my/file"
        ));
        assert_eq!(result.runargs.unwrap(), ["--version", "-T", "-ST"]);
    }

    #[test]
    fn zara_options_ignored_after_runargs_prefix() {
        let args = ["foo/me", "--", "--version", "-T", "-ST"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                cmd: Cmd::Run,
                input: Input::Repl,
                me,
                mode: RunMode::Evaluate,
                runargs: Some(_),
            } if me == "foo/me"
        ));
        assert_eq!(result.runargs.unwrap(), ["--version", "-T", "-ST"]);
    }

    #[test]
    fn decompose_includes_me_in_args() {
        let args = ["foo/me", "-", "--", "arg1", "arg2", "--arg3=1"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert_eq!(result.decompose().2, ["foo/me", "arg1", "arg2", "--arg3=1"]);
    }

    #[test]
    fn decompose_includes_file_in_args() {
        let args = ["foo/me", "my/file", "arg1", "arg2", "--arg3=1"];

        let result = Args::parse(args.into_iter().map(String::from));

        assert_eq!(
            result.decompose().2,
            ["my/file", "arg1", "arg2", "--arg3=1"]
        );
    }
}
