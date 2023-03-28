pub fn lib_hello() {
    println!("Hello from Zara!");
}

pub fn eval(text: &str) -> String {
    let mut s = String::from("Evaluated text: ");
    s.push_str(text);
    s
}
