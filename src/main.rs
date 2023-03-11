fn main() {
    let mut name = String::from(zara::LIB_INFO.name);
    make_titlecase(&mut name);
    let args = parse_args(std::env::args());
    if args.version {
        print_version(&name);
    } else {
        print_usage(&args.me, &name);
    }
}

struct ParsedArgs {
    me: String,
    version: bool,
}

fn parse_args(mut args: impl Iterator<Item = String>) -> ParsedArgs {
    let mut parsed = ParsedArgs {
        me: args.next().unwrap_or(String::from(zara::LIB_INFO.name)),
        version: false,
    };
    for arg in args {
        match arg.as_str() {
            "-V" | "--version" => parsed.version = true,
            _ => (),
        }
    }
    parsed
}

fn make_titlecase(n: &mut str) {
    if let Some(t) = n.get_mut(0..1) {
        t.make_ascii_uppercase()
    }
}

fn print_version(n: &str) {
    println!("{n} {}", zara::LIB_INFO.version.full);
}

fn print_usage(me: &str, n: &str) {
    println!("---=== {n} Usage ===---");
    println!("{me} [options]");
}
