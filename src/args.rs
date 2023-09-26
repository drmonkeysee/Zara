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
    let mut parsed = Args {
        me: args.next().unwrap_or(env!("CARGO_PKG_NAME").to_owned()),
        ..Default::default()
    };
    for arg in args {
        if parsed.stdin {
            parsed.prg = Some(arg);
            break;
        }
        match arg.as_str() {
            AST_SHORT | AST_LONG => parsed.ast = true,
            HELP_SHORT | HELP_LONG => parsed.help = true,
            TOKEN_SHORT | TOKEN_LONG => parsed.tokens = true,
            VERSION_SHORT | VERSION_LONG => parsed.ver = true,
            STDIN_SHORT => parsed.stdin = true,
            AST_TOKEN_SHORT | TOKEN_AST_SHORT => {
                parsed.ast = true;
                parsed.tokens = true;
            }
            _ => (),
        }
    }
    parsed
}

pub(crate) fn usage(me: &str) {
    println!("---=== {} Usage ===---", app_title());
    println!("{me} [options...] [command | file | -] [args...]");
    println!();
    println!("options");
    println!(
        "  {:1$}: print abstract syntax tree",
        format!("{AST_SHORT}, {AST_LONG}"),
        OPT_WIDTH
    );
    println!(
        "  {:1$}: print tokenized input",
        format!("{TOKEN_SHORT}, {TOKEN_LONG}"),
        OPT_WIDTH
    );
    println!();
    println!("commands");
    println!(
        "  {:1$}: print usage",
        format!("{HELP_SHORT}, {HELP_LONG}"),
        OPT_WIDTH
    );
    println!(
        "  {:1$}: print version",
        format!("{VERSION_SHORT}, {VERSION_LONG}"),
        OPT_WIDTH
    );
    println!("  {LIB_SHORT} [name],");
    println!(
        "  {:1$}: run program from named library (omit name for usage)",
        format!("{LIB_LONG} [name]"),
        OPT_WIDTH
    );
    println!();
    println!("{:1$}: run program from script file", "file", ARG_WIDTH);
    println!(
        "{:1$}: run program string if provided, otherwise run program from stdin",
        format!("{STDIN_SHORT} [prg]"),
        ARG_WIDTH
    );
    println!("{:1$}: arguments passed to program", "args", ARG_WIDTH);
    println!("{:1$}: launch REPL", "<no input>", ARG_WIDTH);
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
pub(crate) struct Args {
    pub(crate) ast: bool,
    pub(crate) help: bool,
    pub(crate) me: String,
    pub(crate) prg: Option<String>,
    pub(crate) stdin: bool,
    pub(crate) tokens: bool,
    pub(crate) ver: bool,
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
                help: false,
                me,
                prg: None,
                stdin: false,
                tokens: false,
                ver: false,
            } if me == "zara"
        ));
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
                        help: true,
                        me,
                        prg: None,
                        stdin: false,
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
                help: false,
                me: _,
                prg: None,
                stdin: true,
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
                help: false,
                me: _,
                prg: Some(s),
                stdin: true,
                tokens: false,
                ver: false,
            } if s == input,
        ));
    }

    #[test]
    fn stdin_prg_args() {
        let input = "stdin input arg1 arg2 --arg3=1";
        let args = ["foo/me", "-", input];

        let result = parse(args.into_iter().map(String::from));

        todo!();
        assert!(matches!(
            result,
            Args {
                ast: false,
                help: false,
                me: _,
                prg: Some(s),
                stdin: true,
                tokens: false,
                ver: false,
            } if s == input,
        ));
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
                        help: false,
                        me: _,
                        prg: None,
                        stdin: false,
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
                        help: false,
                        me: _,
                        prg: None,
                        stdin: false,
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
                        help: false,
                        me: _,
                        prg: None,
                        stdin: false,
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
                        help: false,
                        me: _,
                        prg: None,
                        stdin: false,
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
    fn no_matched_args() {
        let args = ["foo/me", "--not-an-option"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                help: false,
                me: _,
                prg: None,
                stdin: false,
                tokens: false,
                ver: false,
            }
        ));
    }
}
