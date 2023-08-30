const AST_SHORT: &str = "-S";
const AST_LONG: &str = "--syntax";
const HELP_SHORT: &str = "-h";
const HELP_LONG: &str = "--help";
const LIB_SHORT: &str = "-l";
const LIB_LONG: &str = "--lib";
const TOKEN_SHORT: &str = "-T";
const TOKEN_LONG: &str = "--tokens";
const VERSION_SHORT: &str = "-V";
const VERSION_LONG: &str = "--version";

pub(crate) fn parse(args: impl IntoIterator<Item = String>) -> Args {
    let mut args = args.into_iter();
    let mut parsed = Args {
        me: args.next().unwrap_or(env!("CARGO_PKG_NAME").to_owned()),
        ..Default::default()
    };
    let mut stdin = false;
    for arg in args {
        if stdin {
            parsed.stdin = Some(arg);
            break;
        }
        match arg.as_str() {
            AST_SHORT | AST_LONG => parsed.ast = true,
            HELP_SHORT | HELP_LONG => parsed.help = true,
            TOKEN_SHORT | TOKEN_LONG => parsed.tokens = true,
            VERSION_SHORT | VERSION_LONG => parsed.ver = true,
            "-" => stdin = true,
            _ => (),
        }
    }
    parsed
}

#[derive(Debug, Default)]
pub(crate) struct Args {
    pub(crate) ast: bool,
    pub(crate) help: bool,
    pub(crate) me: String,
    pub(crate) stdin: Option<String>,
    pub(crate) tokens: bool,
    pub(crate) ver: bool,
}

pub(crate) fn usage(me: &str) {
    println!("---=== {} Usage ===---", app_title());
    println!("{me} [options...] [command | file | -] [args...]");
    println!();
    println!("options");
    println!("  {AST_SHORT}, {AST_LONG}\t: print abstract syntax tree");
    println!("  {TOKEN_SHORT}, {TOKEN_LONG}\t: print tokenized input");
    println!();
    println!("commands");
    println!("  {HELP_SHORT}, {HELP_LONG}\t: print usage");
    println!("  {VERSION_SHORT}, {VERSION_LONG}\t: print version");
    println!(
        "  {LIB_SHORT} [name],\n  {LIB_LONG} [name]\t: run program from named library (omit name for usage)"
    );
    println!();
    println!("file\t\t: run program from script file");
    println!("-\t\t: run program from stdin");
    println!("args\t\t: arguments passed to program");
    println!("<no input>\t: launch REPL");
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
        print!("{}", deps.replace(',', "\n"));
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
                help: false,
                me,
                stdin: None,
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
                        stdin: None,
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
        let input = "stdin input";
        let args = ["foo/me", "-", input];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                help: false,
                me: _,
                stdin: Some(s),
                tokens: false,
                ver: false,
            } if s == input,
        ));
    }

    #[test]
    fn stdin_noinput() {
        let args = ["foo/me", "-"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Args {
                ast: false,
                help: false,
                me: _,
                stdin: None,
                tokens: false,
                ver: false,
            },
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
                        stdin: None,
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
                        stdin: None,
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
                        stdin: None,
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
                stdin: None,
                tokens: false,
                ver: false,
            }
        ));
    }
}
