pub(crate) fn parse(mut args: impl Iterator<Item = String>) -> Parsed {
    let me = args.next().unwrap_or(env!("CARGO_PKG_NAME").to_string());
    let mut help = false;
    let mut ver = false;
    for arg in args {
        match arg.as_str() {
            "-h" | "--help" => help = true,
            "-V" | "--version" => ver = true,
            _ => (),
        }
    }
    // NOTE: enforce command precedence here
    if help {
        Parsed::Command(Action::Help(me))
    } else if ver {
        Parsed::Command(Action::Version)
    } else {
        Parsed::Options(Opts {})
    }
}

pub(crate) enum Parsed {
    Command(Action),
    Options(Opts),
}

pub(crate) enum Action {
    Help(String),
    Version,
}

impl Action {
    pub(crate) fn run(&self) {
        match self {
            Action::Help(me) => usage(me),
            Action::Version => version(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Opts {}

fn usage(me: &str) {
    println!("---=== {} Usage ===---", app_title());
    println!("{me} [options...] [command]");
    println!();
    println!("commands");
    println!("  -h, --help\t: print usage");
    println!("  -V, --version\t: print version");
}

fn version() {
    println!("{} {}", app_title(), env!("CARGO_PKG_VERSION"));
}

fn app_title() -> String {
    let mut title = env!("CARGO_PKG_NAME").to_string();
    if let Some(t) = title.get_mut(0..1) {
        t.make_ascii_uppercase()
    }
    title
}
