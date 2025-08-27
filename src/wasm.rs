
use wasm_bindgen::prelude::*;
use std::cell::RefCell;
use std::rc::Rc;
use crate::{environment::Environment, evaluator::eval_program, lexer::Lexer, parser::Parser};
use wasm_bindgen::JsValue;
use web_sys::console;

thread_local! {
    static ENV: Rc<RefCell<Environment>> = Rc::new(RefCell::new(Environment::new()));
}

#[wasm_bindgen]
pub fn interpret(input: &str) -> String {
    console::log_1(&format!("Interpreting: {}", input).into());
    ENV.with(|env| {
        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        if !p.errors.is_empty() {
            return format!("Parser errors: {:?}", p.errors);
        }
        let evaluated = eval_program(&program, Rc::clone(env));
        evaluated.to_string()
    })
}

#[wasm_bindgen]
pub fn reset_env() {
    ENV.with(|env| {
        *env.borrow_mut() = Environment::new();
    });
}