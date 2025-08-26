use crate::environment::Environment;
use crate::evaluator::eval_program;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::cell::RefCell;
use std::io::{BufRead, Write};
use std::rc::Rc;

const PROMPT: &str = ">> ";

pub fn start<R: BufRead, W: Write>(mut input: R, mut output: W) {
    let env = Rc::new(RefCell::new(Environment::new()));
    loop {
        write!(output, "{}", PROMPT).unwrap();
        output.flush().unwrap();
        let mut line = String::new();
        let bytes_read = input.read_line(&mut line).unwrap();
        if bytes_read == 0 {
            break;
        }
        let l = Lexer::new(line.clone());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        if !p.errors.is_empty() {
            print_parser_errors(&mut output, &p.errors);
            continue;
        }
        let evaluated = eval_program(&program, Rc::clone(&env));
        if evaluated.to_string() != "null" {
            writeln!(output, "{}", evaluated).unwrap();
        }
    }
}

const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

fn print_parser_errors<W: Write>(output: &mut W, errors: &[String]) {
    writeln!(output, "{}", MONKEY_FACE).unwrap();
    writeln!(output, "Woops! We ran into some monkey business here!").unwrap();
    writeln!(output, " parser errors:").unwrap();
    for msg in errors {
        writeln!(output, "\t{}", msg).unwrap();
    }
}
