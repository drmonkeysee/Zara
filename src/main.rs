use clap::Parser;
use std::error::Error;
use std::fs::File;
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();
    println!("Input: {cli:?}");
    if let Some(p) = cli.script_path {
        let md = File::open(p)?.metadata()?;
        println!("File data is {md:?}");
    }
    Ok(())
}

#[derive(Debug, Parser)]
#[command(about, version)]
struct Cli {
    #[arg(value_name = "FILE")]
    /// Script file to execute.
    script_path: Option<PathBuf>,
}
