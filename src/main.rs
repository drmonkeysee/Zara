use rustyline::{DefaultEditor, Result};

fn main() -> Result<()> {
    println!("Starting zara cli...");
    zara::lib_hello();
    repl()
}

fn repl() -> Result<()> {
    let mut ed = DefaultEditor::new()?;
    for readline in ed.iter("λ:> ") {
        let line = readline?;
        let result = zara::eval(line.as_str());
        println!("{result}");
    }
    eprintln!("Saw EOF");
    Ok(())
}
