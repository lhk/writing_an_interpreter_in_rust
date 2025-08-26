use std::env;
use std::io;
use writing_an_interpreter_in_rust::repl;

fn main() {
    let username = match env::var("USER") {
        Ok(name) => name,
        Err(_) => "user".to_string(),
    };
    println!(
        "Hello, {}! This is the Monkey programming language!",
        username
    );
    println!("Feel free to type in commands");
    repl::start(io::stdin().lock(), io::stdout());
}
