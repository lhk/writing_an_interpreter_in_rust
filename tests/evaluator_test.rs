use std::cell::RefCell;
use std::rc::Rc;
use writing_an_interpreter_in_rust::environment::Environment;
use writing_an_interpreter_in_rust::evaluator::eval_program;
use writing_an_interpreter_in_rust::lexer::Lexer;
use writing_an_interpreter_in_rust::object::Object;
use writing_an_interpreter_in_rust::parser::Parser;

fn test_eval(input: &str) -> Object {
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    let env = Rc::new(RefCell::new(Environment::new()));
    eval_program(&program, env)
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(val) = obj {
        *val
    } else {
        obj
    }
}

fn test_integer_object(evaluated: Object, expected: i64) {
    match evaluated {
        Object::Integer(val) => assert_eq!(val, expected),
        _ => panic!("object is not Integer. got={:?}", evaluated),
    }
}

fn test_boolean_object(evaluated: Object, expected: bool) {
    match evaluated {
        Object::Boolean(val) => assert_eq!(val, expected),
        _ => panic!("object is not Boolean. got={:?}", evaluated),
    }
}

#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];
    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];
    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];
    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, expected);
    }
}

fn test_null_object(evaluated: Object) {
    match evaluated {
        Object::Null => (),
        _ => panic!("object is not Null. got={:?}", evaluated),
    }
}

#[test]
fn test_if_else_expressions() {
    let tests = vec![
        ("if (true) { 10 }", Some(10)),
        ("if (false) { 10 }", None),
        ("if (1) { 10 }", Some(10)),
        ("if (1 < 2) { 10 }", Some(10)),
        ("if (1 > 2) { 10 }", None),
        ("if (1 > 2) { 10 } else { 20 }", Some(20)),
        ("if (1 < 2) { 10 } else { 20 }", Some(10)),
    ];
    for (input, expected) in tests {
        let evaluated = test_eval(input);
        match expected {
            Some(val) => test_integer_object(evaluated, val),
            None => test_null_object(evaluated),
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("if (10 > 1) { return 10; }", 10),
        (
            r#"
if (10 > 1) {
  if (10 > 1) {
	return 10;
  }
  return 1;
}
"#,
            10,
        ),
        (
            r#"
let f = fn(x) {
  return x;
  x + 10;
};
f(10);"#,
            10,
        ),
        (
            r#"
let f = fn(x) {
   let result = x + 10;
   return result;
   return 10;
};
f(10);"#,
            20,
        ),
    ];
    for (input, expected) in tests {
        let evaluated = unwrap_return_value(test_eval(input));
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_error_handling() {
    let tests = vec![
        ("5 + true;", "type mismatch: Integer + Boolean"),
        ("5 + true; 5;", "type mismatch: Integer + Boolean"),
        ("-true", "unknown operator: -Boolean"),
        ("true + false;", "unknown operator: Boolean + Boolean"),
        (
            "true + false + true + false;",
            "unknown operator: Boolean + Boolean",
        ),
        ("5; true + false; 5", "unknown operator: Boolean + Boolean"),
        (r#""Hello" - "World""#, "unknown operator: String - String"),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: Boolean + Boolean",
        ),
        (
            r#"
if (10 > 1) {
  if (10 > 1) {
	return true + false;
  }
  return 1;
}
"#,
            "unknown operator: Boolean + Boolean",
        ),
        ("foobar", "identifier not found: foobar"),
        (
            r#"{"name": "Monkey"}[fn(x) { x }];"#,
            "unusable as hash key: Function",
        ),
        ("999[1]", "index operator not supported: Integer"),
    ];
    for (input, expected_message) in tests {
        let evaluated = test_eval(input);
        match evaluated {
            Object::Error(msg) => assert_eq!(
                msg, expected_message,
                "wrong error message. expected={}, got={}",
                expected_message, msg
            ),
            _ => panic!("no error object returned. got={:?}", evaluated),
        }
    }
}

#[test]
fn test_let_statements() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];
    for (input, expected) in tests {
        test_integer_object(test_eval(input), expected);
    }
}

#[test]
fn test_builtin_functions() {
    use Object::*;
    let tests: Vec<(&str, Box<dyn Fn(&Object)>)> = vec![
        (
            r#"len("")"#,
            Box::new(|o| test_integer_object(o.clone(), 0)),
        ),
        (
            r#"len("four")"#,
            Box::new(|o| test_integer_object(o.clone(), 4)),
        ),
        (
            r#"len("hello world")"#,
            Box::new(|o| test_integer_object(o.clone(), 11)),
        ),
        (
            r#"len(1)"#,
            Box::new(|o| match o {
                Error(msg) => assert_eq!(msg, "argument to `len` not supported, got Integer"),
                _ => panic!("object is not Error. got={:?}", o),
            }),
        ),
        (
            r#"len("one", "two")"#,
            Box::new(|o| match o {
                Error(msg) => assert_eq!(msg, "wrong number of arguments. got=2, want=1"),
                _ => panic!("object is not Error. got={:?}", o),
            }),
        ),
        (
            r#"len([1, 2, 3])"#,
            Box::new(|o| test_integer_object(o.clone(), 3)),
        ),
        (
            r#"len([])"#,
            Box::new(|o| test_integer_object(o.clone(), 0)),
        ),
        (
            r#"puts("hello", "world!")"#,
            Box::new(|o| test_null_object(o.clone())),
        ),
        (
            r#"first([1, 2, 3])"#,
            Box::new(|o| test_integer_object(o.clone(), 1)),
        ),
        (r#"first([])"#, Box::new(|o| test_null_object(o.clone()))),
        (
            r#"first(1)"#,
            Box::new(|o| match o {
                Error(msg) => assert_eq!(msg, "argument to `first` must be ARRAY, got Integer"),
                _ => panic!("object is not Error. got={:?}", o),
            }),
        ),
        (
            r#"last([1, 2, 3])"#,
            Box::new(|o| test_integer_object(o.clone(), 3)),
        ),
        (r#"last([])"#, Box::new(|o| test_null_object(o.clone()))),
        (
            r#"last(1)"#,
            Box::new(|o| match o {
                Error(msg) => assert_eq!(msg, "argument to `last` must be ARRAY, got Integer"),
                _ => panic!("object is not Error. got={:?}", o),
            }),
        ),
        (
            r#"rest([1, 2, 3])"#,
            Box::new(|o| match o {
                Array(arr) => {
                    assert_eq!(arr.elements.len(), 2);
                    test_integer_object(arr.elements[0].clone(), 2);
                    test_integer_object(arr.elements[1].clone(), 3);
                }
                _ => panic!("object is not Array. got={:?}", o),
            }),
        ),
        (r#"rest([])"#, Box::new(|o| test_null_object(o.clone()))),
        (
            r#"push([], 1)"#,
            Box::new(|o| match o {
                Array(arr) => {
                    assert_eq!(arr.elements.len(), 1);
                    test_integer_object(arr.elements[0].clone(), 1);
                }
                _ => panic!("object is not Array. got={:?}", o),
            }),
        ),
        (
            r#"push(1, 1)"#,
            Box::new(|o| match o {
                Error(msg) => assert_eq!(msg, "argument to `push` must be ARRAY, got Integer"),
                _ => panic!("object is not Error. got={:?}", o),
            }),
        ),
    ];
    for (input, check) in tests {
        let evaluated = test_eval(input);
        check(&evaluated);
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let evaluated = test_eval(input);
    match evaluated {
        Object::Array(arr) => {
            assert_eq!(
                arr.elements.len(),
                3,
                "array has wrong num of elements. got={}",
                arr.elements.len()
            );
            test_integer_object(arr.elements[0].clone(), 1);
            test_integer_object(arr.elements[1].clone(), 4);
            test_integer_object(arr.elements[2].clone(), 6);
        }
        _ => panic!("object is not Array. got={:?}", evaluated),
    }
}

#[test]
fn test_array_index_expressions() {
    let tests = vec![
        ("[1, 2, 3][0]", Some(1)),
        ("[1, 2, 3][1]", Some(2)),
        ("[1, 2, 3][2]", Some(3)),
        ("let i = 0; [1][i];", Some(1)),
        ("[1, 2, 3][1 + 1];", Some(3)),
        ("let myArray = [1, 2, 3]; myArray[2];", Some(3)),
        (
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            Some(6),
        ),
        (
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            Some(2),
        ),
        ("[1, 2, 3][3]", None),
        ("[1, 2, 3][-1]", None),
    ];
    for (input, expected) in tests {
        let evaluated = test_eval(input);
        match expected {
            Some(val) => test_integer_object(evaluated, val),
            None => test_null_object(evaluated),
        }
    }
}

#[test]
fn test_hash_literals() {
    let input = r#"let two = "two";
{"one": 10 - 9, two: 1 + 1, "thr" + "ee": 6 / 2, 4: 4, true: 5, false: 6}"#;
    let evaluated = test_eval(input);
    match evaluated {
        Object::Hash(hash) => {
            let mut expected = std::collections::HashMap::new();
            expected.insert("one".to_string(), 1);
            expected.insert("two".to_string(), 2);
            expected.insert("three".to_string(), 3);
            expected.insert(4.to_string(), 4);
            expected.insert("true".to_string(), 5);
            expected.insert("false".to_string(), 6);
            assert_eq!(
                hash.pairs.len(),
                expected.len(),
                "Hash has wrong num of pairs. got={}",
                hash.pairs.len()
            );
            for (k, v) in &hash.pairs {
                let key_str = format!("{:?}", k);
                // The Debug impl for HashKey will look like String("one"), Int(4), Bool(true), etc.
                // We'll match the expected keys accordingly.
                let expected_value = if key_str == "String(\"one\")" {
                    expected.get("one")
                } else if key_str == "String(\"two\")" {
                    expected.get("two")
                } else if key_str == "String(\"three\")" {
                    expected.get("three")
                } else if key_str == "Int(4)" {
                    expected.get("4")
                } else if key_str == "Bool(true)" {
                    expected.get("true")
                } else if key_str == "Bool(false)" {
                    expected.get("false")
                } else {
                    None
                };
                let expected_value = expected_value.expect("unexpected key in hash");
                test_integer_object(v.clone(), *expected_value);
            }
        }
        _ => panic!("Eval didn't return Hash. got={:?}", evaluated),
    }
}

#[test]
fn test_hash_index_expressions() {
    let tests = vec![
        (r#"{"foo": 5}["foo"]"#, Some(5)),
        (r#"{"foo": 5}["bar"]"#, None),
        (r#"let key = "foo"; {"foo": 5}[key]"#, Some(5)),
        (r#"{}["foo"]"#, None),
        (r#"{5: 5}[5]"#, Some(5)),
        (r#"{true: 5}[true]"#, Some(5)),
        (r#"{false: 5}[false]"#, Some(5)),
    ];
    for (input, expected) in tests {
        let evaluated = test_eval(input);
        match expected {
            Some(val) => test_integer_object(evaluated, val),
            None => test_null_object(evaluated),
        }
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";
    let evaluated = test_eval(input);
    match evaluated {
        Object::Function(func) => {
            assert_eq!(
                func.parameters.len(),
                1,
                "function has wrong parameters. Parameters={:?}",
                func.parameters
            );
            assert_eq!(
                func.parameters[0], "x",
                "parameter is not 'x'. got={:?}",
                func.parameters[0]
            );
            let expected_body = "(x + 2)";
            assert_eq!(
                format!("{}", func.body),
                expected_body,
                "body is not {}. got={}",
                expected_body,
                func.body
            );
        }
        _ => panic!("object is not Function. got={:?}", evaluated),
    }
}

#[test]
fn test_function_application() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ];
    for (input, expected) in tests {
        test_integer_object(test_eval(input), expected);
    }
}

#[test]
fn test_enclosing_environments() {
    let input = r#"
let first = 10;
let second = 10;
let third = 10;

let ourFunction = fn(first) {
  let second = 20;
  first + second + third;
};

ourFunction(20) + first + second;
"#;
    test_integer_object(test_eval(input), 70);
}

#[test]
fn test_closures() {
    let input = r#"
let newAdder = fn(x) {
  fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
"#;
    test_integer_object(test_eval(input), 4);
}

#[test]
fn test_string_literal() {
    let input = r#""Hello World!""#;
    let evaluated = test_eval(input);
    match evaluated {
        Object::String(s) => assert_eq!(s, "Hello World!"),
        _ => panic!("object is not String. got={:?}", evaluated),
    }
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;
    let evaluated = test_eval(input);
    match evaluated {
        Object::String(s) => assert_eq!(s, "Hello World!"),
        _ => panic!("object is not String. got={:?}", evaluated),
    }
}
