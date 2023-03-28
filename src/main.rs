use rustyline::{DefaultEditor, Result};

fn main() -> Result<()> {
    println!("Starting zara cli...");
    zara::hello();
    repl()
}

fn repl() -> Result<()> {
    let mut ed = DefaultEditor::new()?;
    for readline in ed.iter("λ:> ") {
        let result = zara::eval(readline?.as_str());
        println!("{result}");
    }
    eprintln!("Saw EOF");
    Ok(())
}
