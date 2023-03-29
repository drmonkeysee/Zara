pub(crate) fn parse(mut args: impl Iterator<Item = String>) -> Option<Opts> {
    let me = args.next();
    let mut help = false;
    let mut ver = false;
    for arg in args {
        match arg.as_str() {
            "-h" | "--help" => help = true,
            "-V" | "--version" => ver = true,
            _ => (),
        }
    }
    // NOTE: order of application matters here to ensure
    // "-h -V" and "-V -h" behave consistently.
    if help {
        usage(me);
        None
    } else if ver {
        version();
        None
    } else {
        Some(Opts {})
    }
}

#[derive(Debug)]
pub(crate) struct Opts {}

fn usage(me: Option<String>) {
    println!("---=== {} Usage ===---", app_title());
    println!(
        "{} [options...]\n",
        me.unwrap_or(env!("CARGO_PKG_NAME").to_string())
    );
    println!("options");
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
