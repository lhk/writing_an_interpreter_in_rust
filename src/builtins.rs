use crate::object::{Array, Builtin, Object};
use std::collections::HashMap;

pub fn get_builtins() -> HashMap<&'static str, Object> {
    let mut builtins = HashMap::new();

    builtins.insert(
        "len",
        Object::Builtin(Builtin {
            func: |args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(
                        format!("wrong number of arguments. got={}, want=1", args.len())
                            .to_string(),
                    );
                }
                match &args[0] {
                    Object::Array(arr) => Object::Integer(arr.elements.len() as i64),
                    Object::String(s) => Object::Integer(s.len() as i64),
                    _ => Object::Error(format!(
                        "argument to `len` not supported, got {}",
                        crate::evaluator::type_name(&args[0])
                    )),
                }
            },
        }),
    );

    builtins.insert(
        "puts",
        Object::Builtin(Builtin {
            func: |args: Vec<Object>| {
                for arg in &args {
                    println!("{}", arg);
                }
                Object::Null
            },
        }),
    );

    builtins.insert(
        "first",
        Object::Builtin(Builtin {
            func: |args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(
                        format!("wrong number of arguments. got={}, want=1", args.len())
                            .to_string(),
                    );
                }
                match &args[0] {
                    Object::Array(arr) => {
                        if let Some(first) = arr.elements.first() {
                            first.clone()
                        } else {
                            Object::Null
                        }
                    }
                    _ => Object::Error(format!(
                        "argument to `first` must be ARRAY, got {}",
                        crate::evaluator::type_name(&args[0]).to_string()
                    )),
                }
            },
        }),
    );

    builtins.insert(
        "last",
        Object::Builtin(Builtin {
            func: |args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(
                        format!("wrong number of arguments. got={}, want=1", args.len())
                            .to_string(),
                    );
                }
                match &args[0] {
                    Object::Array(arr) => {
                        if let Some(last) = arr.elements.last() {
                            last.clone()
                        } else {
                            Object::Null
                        }
                    }
                    _ => Object::Error(format!(
                        "argument to `last` must be ARRAY, got {}",
                        crate::evaluator::type_name(&args[0]).to_string()
                    )),
                }
            },
        }),
    );

    builtins.insert(
        "rest",
        Object::Builtin(Builtin {
            func: |args: Vec<Object>| {
                if args.len() != 1 {
                    return Object::Error(
                        format!("wrong number of arguments. got={}, want=1", args.len())
                            .to_string(),
                    );
                }
                match &args[0] {
                    Object::Array(arr) => {
                        if arr.elements.len() > 0 {
                            let rest = arr.elements[1..].to_vec();
                            Object::Array(Array { elements: rest })
                        } else {
                            Object::Null
                        }
                    }
                    _ => Object::Error(format!(
                        "argument to `rest` must be ARRAY, got {}",
                        crate::evaluator::type_name(&args[0]).to_string()
                    )),
                }
            },
        }),
    );

    builtins.insert(
        "push",
        Object::Builtin(Builtin {
            func: |args: Vec<Object>| {
                if args.len() != 2 {
                    return Object::Error(
                        format!("wrong number of arguments. got={}, want=2", args.len())
                            .to_string(),
                    );
                }
                match &args[0] {
                    Object::Array(arr) => {
                        let mut new_elements = arr.elements.clone();
                        new_elements.push(args[1].clone());
                        Object::Array(Array {
                            elements: new_elements,
                        })
                    }
                    _ => Object::Error(format!(
                        "argument to `push` must be ARRAY, got {}",
                        crate::evaluator::type_name(&args[0]).to_string()
                    )),
                }
            },
        }),
    );

    builtins
}

// fn new_error(msg: &str) -> Object {
//     Object::Error(msg.to_string())
// }
