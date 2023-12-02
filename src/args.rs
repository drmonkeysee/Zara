use std::path::PathBuf;

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

pub(crate) fn parse(args: impl IntoIterator<Item = String>) -> Args {
    let mut args = args.into_iter();
    let mut arg_parse = Args {
        me: args.next().unwrap_or(env!("CARGO_PKG_NAME").to_owned()),
        ..Default::default()
    };
    for arg in args {
        arg_parse.match_arg(arg);
    }
    arg_parse
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

#[derive(Debug, Default)]
pub(crate) enum Input {
    File(PathBuf),
    Prg(String),
    #[default]
    Repl,
    Stdin,
}

#[derive(Debug, Default)]
pub(crate) struct Args {
    pub(crate) ast: bool,
    pub(crate) input: Input,
    pub(crate) help: bool,
    pub(crate) me: String,
    pub(crate) runargs: Vec<String>,
    pub(crate) runargs_prefix: bool,
    pub(crate) tokens: bool,
    pub(crate) ver: bool,
}

impl Args {
    fn match_arg(&mut self, arg: String) {
        if self.has_run_target() {
            self.match_target_arg(arg);
        } else {
            self.match_command_arg(arg);
        }
    }

    fn has_run_target(&self) -> bool {
        !matches!(self.input, Input::Repl) || self.runargs_prefix
    }

    fn expecting_prg_text(&self) -> bool {
        matches!(self.input, Input::Stdin) && !self.runargs_prefix
    }

    fn expecting_file_path(&self) -> bool {
        matches!(self.input, Input::Repl) && !self.runargs_prefix
    }

    fn match_target_arg(&mut self, arg: String) {
        match arg.as_str() {
            ARGS_PREFIX => self.runargs_prefix = true,
            _ if self.expecting_prg_text() => self.input = Input::Prg(arg),
            _ => self.runargs.push(arg),
        }
    }

    fn match_command_arg(&mut self, arg: String) {
        match arg.as_str() {
            ARGS_PREFIX => self.runargs_prefix = true,
            AST_SHORT | AST_LONG => self.ast = true,
            HELP_SHORT | HELP_LONG => self.help = true,
            TOKEN_SHORT | TOKEN_LONG => self.tokens = true,
            VERSION_SHORT | VERSION_LONG => self.ver = true,
            STDIN_SHORT => self.input = Input::Stdin,
            AST_TOKEN_SHORT | TOKEN_AST_SHORT => {
                self.ast = true;
                self.tokens = true;
            }
            _ if self.expecting_file_path() => {
                let mut p = PathBuf::new();
                p.push(arg);
                self.input = Input::File(p);
            }
            _ => (),
        }
    }
}

fn app_title() -> String {
    let mut title = env!("CARGO_PKG_NAME").to_owned();
    if let Some(t) = title.get_mut(0..1) {
        t.make_ascii_uppercase();
    }
    title
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_args() {
        let args: [String; 0] = [];

        let result = parse(args.into_iter());

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::Repl,
                help: false,
                me,
                runargs,
                runargs_prefix: false,
                tokens: false,
                ver: false,
            } if me == "zara" && runargs.len() == 0
        ));
    }

    #[test]
    fn just_me() {
        let args = ["foo/me"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::Repl,
                help: false,
                me,
                runargs,
                runargs_prefix: false,
                tokens: false,
                ver: false,
            } if me == "foo/me" && runargs.len() == 0
        ));
    }

    #[test]
    fn just_me_with_args() {
        let args = ["foo/me", "--", "arg1", "arg2", "--arg3=1"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::Repl,
                help: false,
                me,
                runargs: _,
                runargs_prefix: true,
                tokens: false,
                ver: false,
            } if me == "foo/me"
        ));
        assert_eq!(result.runargs, ["arg1", "arg2", "--arg3=1"]);
    }

    #[test]
    fn help() {
        let program = "foo/me";
        let cases = [[program, "-h"], [program, "--help"]];

        for case in cases {
            let result = parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        ast: false,
                        input: Input::Repl,
                        help: true,
                        me,
                        runargs: _,
                        runargs_prefix: false,
                        tokens: false,
                        ver: false,
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

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::Stdin,
                help: false,
                me: _,
                runargs: _,
                runargs_prefix: false,
                tokens: false,
                ver: false,
            },
        ));
    }

    #[test]
    fn stdin_prg() {
        let input = "stdin input";
        let args = ["foo/me", "-", input];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::Prg(s),
                help: false,
                me: _,
                runargs: _,
                runargs_prefix: false,
                tokens: false,
                ver: false,
            } if s == input,
        ));
    }

    #[test]
    fn stdin_args() {
        let args = ["foo/me", "-", "--", "arg1", "arg2", "--arg3=1"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::Stdin,
                help: false,
                me: _,
                runargs: _,
                runargs_prefix: true,
                tokens: false,
                ver: false,
            },
        ));
        assert_eq!(result.runargs, ["arg1", "arg2", "--arg3=1"]);
    }

    #[test]
    fn stdin_prg_args() {
        let input = "stdin input";
        let args = ["foo/me", "-", input, "arg1", "arg2", "--arg3=1"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::Prg(s),
                help: false,
                me: _,
                runargs: _,
                runargs_prefix: false,
                tokens: false,
                ver: false,
            } if s == input,
        ));
        assert_eq!(result.runargs, ["arg1", "arg2", "--arg3=1"]);
    }

    #[test]
    fn syntax_tree() {
        let cases = [["foo/me", "-S"], ["foo/me", "--syntax"]];

        for case in cases {
            let result = parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        ast: true,
                        input: Input::Repl,
                        help: false,
                        me: _,
                        runargs: _,
                        runargs_prefix: false,
                        tokens: false,
                        ver: false,
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
            let result = parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        ast: false,
                        input: Input::Repl,
                        help: false,
                        me: _,
                        runargs: _,
                        runargs_prefix: false,
                        tokens: true,
                        ver: false,
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
            let result = parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        ast: true,
                        input: Input::Repl,
                        help: false,
                        me: _,
                        runargs: _,
                        runargs_prefix: false,
                        tokens: true,
                        ver: false,
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
            let result = parse(case.into_iter().map(String::from));

            assert!(
                matches!(
                    result,
                    Args {
                        ast: false,
                        input: Input::Repl,
                        help: false,
                        me: _,
                        runargs: _,
                        runargs_prefix: false,
                        tokens: false,
                        ver: true,
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

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::File(p),
                help: false,
                me: _,
                runargs: _,
                runargs_prefix: false,
                tokens: false,
                ver: false,
            } if p.to_str().unwrap() == "my/file"
        ));
    }

    #[test]
    fn file_args() {
        let args = ["foo/me", "my/file", "arg1", "arg2", "--arg3=1"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::File(p),
                help: false,
                me: _,
                runargs: _,
                runargs_prefix: false,
                tokens: false,
                ver: false,
            } if p.to_str().unwrap() == "my/file"
        ));
        assert_eq!(result.runargs, ["arg1", "arg2", "--arg3=1"]);
    }

    #[test]
    fn zara_options_ignored_after_run_target() {
        let args = ["foo/me", "my/file", "--version", "-T", "-ST"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::File(p),
                help: false,
                me,
                runargs: _,
                runargs_prefix: false,
                tokens: false,
                ver: false,
            } if me == "foo/me" && p.to_str().unwrap() == "my/file"
        ));
        assert_eq!(result.runargs, ["--version", "-T", "-ST"]);
    }

    #[test]
    fn zara_options_ignored_after_runargs_prefix() {
        let args = ["foo/me", "--", "--version", "-T", "-ST"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                input: Input::Repl,
                help: false,
                me,
                runargs: _,
                runargs_prefix: true,
                tokens: false,
                ver: false,
            } if me == "foo/me"
        ));
        assert_eq!(result.runargs, ["--version", "-T", "-ST"]);
    }
}
