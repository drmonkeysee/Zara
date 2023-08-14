use crate::repl::Repl;
use rustyline::error::ReadlineError;
use std::result;

const HELP_SHORT: &str = "-h";
const HELP_LONG: &str = "--help";
const LIB_SHORT: &str = "-l";
const LIB_LONG: &str = "--lib";
const TOKEN_SHORT: &str = "-T";
const TOKEN_LONG: &str = "--token";
const VERSION_SHORT: &str = "-V";
const VERSION_LONG: &str = "--version";

pub(crate) type Result = result::Result<(), CmdError>;

pub(crate) fn parse(mut args: impl Iterator<Item = String>) -> Cmd {
    let mut parsed: Parsed = Default::default();
    parsed.me = args.next().unwrap_or(env!("CARGO_PKG_NAME").to_owned());
    for arg in args {
        match arg.as_str() {
            HELP_SHORT | HELP_LONG => parsed.help = true,
            TOKEN_SHORT | TOKEN_LONG => parsed.tokenize = true,
            VERSION_SHORT | VERSION_LONG => parsed.ver = true,
            "-" => parsed.stdin = true,
            _ => (),
        }
    }
    // NOTE: enforce command precedence here
    if parsed.help {
        Cmd::Help(parsed.me)
    } else if parsed.ver {
        Cmd::Version
    } else {
        Cmd::Repl(Opts)
    }
}

#[derive(Debug)]
pub(crate) enum CmdError {
    Repl(ReadlineError),
}

impl From<ReadlineError> for CmdError {
    fn from(value: ReadlineError) -> Self {
        Self::Repl(value)
    }
}

pub(crate) enum Cmd {
    Help(String),
    Repl(Opts),
    Version,
}

impl Cmd {
    pub(crate) fn execute(self) -> Result {
        match self {
            Self::Help(me) => usage(me),
            Self::Repl(opts) => repl(opts),
            Self::Version => version(),
        }
    }
}

pub(crate) struct Opts;

#[derive(Default)]
struct Parsed {
    help: bool,
    me: String,
    stdin: bool,
    tokenize: bool,
    ver: bool,
}

fn usage(me: String) -> Result {
    println!("---=== {} Usage ===---", app_title());
    println!("{me} [options...] [command | file | -] [args...]");
    println!();
    println!("options");
    println!("  {TOKEN_SHORT}, {TOKEN_LONG}\t: tokenize output only");
    println!();
    println!("commands");
    println!("  {HELP_SHORT}, {HELP_LONG}\t: print usage");
    println!("  {VERSION_SHORT}, {VERSION_LONG}\t: print version");
    println!(
        "  {LIB_SHORT} [name],\n  {LIB_LONG} [name]\t: run program from named library (omit name for usage)"
    );
    println!("");
    println!("file\t\t: run program from script file");
    println!("-\t\t: run program from stdin");
    println!("args\t\t: arguments passed to program");
    println!("<no input>\t: launch REPL");
    Ok(())
}

fn repl(opts: Opts) -> Result {
    let mut r = Repl::new(opts)?;
    r.run()?;
    Ok(())
}

fn version() -> Result {
    println!("{} {}", app_title(), env!("CARGO_PKG_VERSION"));
    Ok(())
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

        assert!(matches!(result, Cmd::Repl(Opts)));
    }

    #[test]
    fn short_help() {
        let program = "foo/me";
        let args = [program, "-h"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Cmd::Help(me) if me == program
        ));
    }

    #[test]
    fn long_help() {
        let program = "foo/me";
        let args = [program, "--help"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Cmd::Help(me) if me == program
        ));
    }

    #[test]
    fn short_version() {
        let args = ["foo/me", "-V"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(result, Cmd::Version));
    }

    #[test]
    fn long_version() {
        let args = ["foo/me", "--version"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(result, Cmd::Version));
    }

    #[test]
    fn no_matched_args() {
        let args = ["foo/me", "--not-an-option"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(result, Cmd::Repl(Opts)));
    }
}
