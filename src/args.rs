const HELP_SHORT: &str = "-h";
const HELP_LONG: &str = "--help";
const LIB_SHORT: &str = "-l";
const LIB_LONG: &str = "--lib";
const TOKEN_SHORT: &str = "-T";
const TOKEN_LONG: &str = "--token";
const VERSION_SHORT: &str = "-V";
const VERSION_LONG: &str = "--version";

pub(crate) fn parse(mut args: impl Iterator<Item = String>) -> Parsed {
    let me = args.next().unwrap_or(env!("CARGO_PKG_NAME").to_owned());
    let mut help = false;
    let mut ver = false;
    for arg in args {
        match arg.as_str() {
            HELP_SHORT | HELP_LONG => help = true,
            VERSION_SHORT | VERSION_LONG => ver = true,
            _ => (),
        }
    }
    // NOTE: enforce command precedence here
    if help {
        Parsed::Command(Cmd::Help(me))
    } else if ver {
        Parsed::Command(Cmd::Version)
    } else {
        Parsed::Options(Opts)
    }
}

pub(crate) enum Parsed {
    Command(Cmd),
    Options(Opts),
}

pub(crate) enum Cmd {
    Help(String),
    Version,
}

impl Cmd {
    pub(crate) fn execute(&self) {
        match self {
            Self::Help(me) => usage(me),
            Self::Version => version(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Opts;

fn usage(me: &str) {
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
}

fn version() {
    println!("{} {}", app_title(), env!("CARGO_PKG_VERSION"));
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

        assert!(matches!(result, Parsed::Options(Opts)));
    }

    #[test]
    fn short_help() {
        let program = "foo/me";
        let args = [program, "-h"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Parsed::Command(Cmd::Help(me)) if me == program
        ));
    }

    #[test]
    fn long_help() {
        let program = "foo/me";
        let args = [program, "--help"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(
            result,
            Parsed::Command(Cmd::Help(me)) if me == program
        ));
    }

    #[test]
    fn short_version() {
        let args = ["foo/me", "-V"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(result, Parsed::Command(Cmd::Version)));
    }

    #[test]
    fn long_version() {
        let args = ["foo/me", "--version"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(result, Parsed::Command(Cmd::Version)));
    }

    #[test]
    fn no_matched_args() {
        let args = ["foo/me", "--not-an-option"];

        let result = parse(args.into_iter().map(String::from));

        assert!(matches!(result, Parsed::Options(Opts)));
    }
}
