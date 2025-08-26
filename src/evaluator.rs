use crate::ast::{Expression, HashKey, Node, Program, Statement};
use crate::builtins::get_builtins;
use crate::environment::Environment;
use crate::object::{Array, Function, Hash, Object};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn type_name(obj: &Object) -> &'static str {
    match obj {
        Object::Integer(_) => "Integer",
        Object::Boolean(_) => "Boolean",
        Object::String(_) => "String",
        Object::Array(_) => "Array",
        Object::Hash(_) => "Hash",
        Object::Null => "Null",
        Object::ReturnValue(_) => "ReturnValue",
        Object::Error(_) => "Error",
        Object::Function(_) => "Function",
        Object::Builtin(_) => "Builtin",
    }
}

pub fn eval(node: &Node, env: Rc<RefCell<Environment>>) -> Object {
    match node {
        Node::Statement(stmt) => eval_statement(stmt, env),
        Node::Expression(expr) => eval_expression(expr, env),
    }
}

pub fn eval_program(program: &Program, env: Rc<RefCell<Environment>>) -> Object {
    let mut result = Object::Null;
    for stmt in &program.statements {
        result = eval_statement(stmt, env.clone());
        match result {
            Object::Error(_) | Object::ReturnValue(_) => return result,
            _ => {}
        }
    }
    result
}

fn eval_statement(stmt: &Statement, env: Rc<RefCell<Environment>>) -> Object {
    match stmt {
        Statement::Block { statements, .. } => eval_block_statement(statements, env),
        Statement::ExpressionStatement { expression, .. } => eval_expression(expression, env),
        Statement::Return { return_value, .. } => {
            let val = eval_expression(return_value, env.clone());
            if is_error(&val) {
                return val;
            }
            Object::ReturnValue(Box::new(val))
        }
        Statement::Let { name, value, .. } => {
            let val = eval_expression(value, env.clone());
            if is_error(&val) {
                return val;
            }
            if let Expression::Identifier {
                value: name_str, ..
            } = &**name
            {
                env.borrow_mut().set(name_str.clone(), val.clone());
            }
            Object::Null
        }
    }
}

fn eval_expression(expr: &Expression, env: Rc<RefCell<Environment>>) -> Object {
    match expr {
        Expression::Identifier { value, .. } => eval_identifier(value, env),
        Expression::IntegerLiteral { value, .. } => Object::Integer(*value),
        Expression::StringLiteral { value, .. } => Object::String(value.clone()),
        Expression::Boolean { value, .. } => native_bool_to_boolean_object(*value),
        Expression::Prefix {
            operator, right, ..
        } => {
            let right = eval_expression(right, env);
            if is_error(&right) {
                return right;
            }
            eval_prefix_expression(operator, right)
        }
        Expression::Infix {
            operator,
            left,
            right,
            ..
        } => {
            let left = eval_expression(left, env.clone());
            if is_error(&left) {
                return left;
            }
            let right = eval_expression(right, env);
            if is_error(&right) {
                return right;
            }
            eval_infix_expression(operator, left, right)
        }
        Expression::If {
            condition,
            consequence,
            alternative,
            ..
        } => eval_if_expression(condition, consequence, alternative, env),
        Expression::FunctionLiteral {
            parameters, body, ..
        } => {
            let params = parameters
                .iter()
                .filter_map(|p| {
                    if let Expression::Identifier { value, .. } = p {
                        Some(value.clone())
                    } else {
                        None
                    }
                })
                .collect();
            Object::Function(Function {
                parameters: params,
                body: body.clone(),
                env: env.clone(),
            })
        }
        Expression::Call {
            function,
            arguments,
            ..
        } => {
            let function = eval_expression(function, env.clone());
            if is_error(&function) {
                return function;
            }
            let args = eval_expressions(arguments, env);
            if args.len() == 1 && is_error(&args[0]) {
                return args[0].clone();
            }
            apply_function(function, args)
        }
        Expression::ArrayLiteral { elements, .. } => {
            let elems = eval_expressions(elements, env);
            if elems.len() == 1 && is_error(&elems[0]) {
                return elems[0].clone();
            }
            Object::Array(Array { elements: elems })
        }
        Expression::IndexExpression { left, index, .. } => {
            let left = eval_expression(left, env.clone());
            if is_error(&left) {
                return left;
            }
            let index = eval_expression(index, env);
            if is_error(&index) {
                return index;
            }
            eval_index_expression(left, index)
        }
        Expression::HashLiteral { pairs, .. } => eval_hash_literal(pairs, env),
    }
}

fn eval_block_statement(statements: &Vec<Statement>, env: Rc<RefCell<Environment>>) -> Object {
    let mut result = Object::Null;
    for stmt in statements {
        result = eval_statement(stmt, env.clone());
        match &result {
            Object::ReturnValue(_) | Object::Error(_) => return result,
            _ => {}
        }
    }
    result
}

fn native_bool_to_boolean_object(input: bool) -> Object {
    if input {
        Object::Boolean(true)
    } else {
        Object::Boolean(false)
    }
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Error(format!("unknown operator: -{}", type_name(&right))),
    }
}

fn eval_infix_expression(operator: &str, left: Object, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => eval_integer_infix_expression(operator, l, r),
        (Object::String(l), Object::String(r)) => eval_string_infix_expression(operator, &l, &r),
        (l, r) if operator == "==" => native_bool_to_boolean_object(l == r),
        (l, r) if operator == "!=" => native_bool_to_boolean_object(l != r),
        (l, r) if std::mem::discriminant(&l) != std::mem::discriminant(&r) => Object::Error(
            format!("type mismatch: {} + {}", type_name(&l), type_name(&r)),
        ),
        (l, r) => Object::Error(format!(
            "unknown operator: {} {} {}",
            type_name(&l),
            operator,
            type_name(&r)
        )),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(i) => Object::Integer(-i),
        _ => Object::Error(format!("unknown operator: -{}", type_name(&right))),
    }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => native_bool_to_boolean_object(left < right),
        ">" => native_bool_to_boolean_object(left > right),
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => Object::Error(format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn eval_string_infix_expression(operator: &str, left: &str, right: &str) -> Object {
    match operator {
        "+" => Object::String(format!("{}{}", left, right)),
        _ => Object::Error(format!("unknown operator: String {} String", operator)),
    }
}

fn eval_if_expression(
    condition: &Box<Expression>,
    consequence: &Box<Statement>,
    alternative: &Option<Box<Statement>>,
    env: Rc<RefCell<Environment>>,
) -> Object {
    let cond = eval_expression(condition, env.clone());
    if is_error(&cond) {
        return cond;
    }
    if is_truthy(&cond) {
        eval_statement(consequence, env)
    } else if let Some(alt) = alternative {
        eval_statement(alt, env)
    } else {
        Object::Null
    }
}

fn eval_identifier(name: &str, env: Rc<RefCell<Environment>>) -> Object {
    if let Some(obj) = env.borrow().get(name) {
        return obj;
    }
    let builtins = get_builtins();
    if let Some(obj) = builtins.get(name) {
        return obj.clone();
    }
    Object::Error(format!("identifier not found: {}", name))
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(true) => true,
        Object::Boolean(false) => false,
        _ => true,
    }
}

fn is_error(obj: &Object) -> bool {
    matches!(obj, Object::Error(_))
}

fn eval_expressions(exprs: &Vec<Expression>, env: Rc<RefCell<Environment>>) -> Vec<Object> {
    let mut result = Vec::new();
    for e in exprs {
        let evaluated = eval_expression(e, env.clone());
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    result
}

fn apply_function(function: Object, args: Vec<Object>) -> Object {
    match function {
        Object::Function(func) => {
            let extended_env = extend_function_env(&func, &args);
            let evaluated = eval_statement(&func.body, extended_env);
            unwrap_return_value(evaluated)
        }
        Object::Builtin(builtin) => (builtin.func)(args),
        _ => Object::Error(format!("not a function: {:?}", function)),
    }
}

fn extend_function_env(func: &Function, args: &Vec<Object>) -> Rc<RefCell<Environment>> {
    let env = Rc::new(RefCell::new(Environment::new_enclosed(func.env.clone())));
    for (param, arg) in func.parameters.iter().zip(args.iter()) {
        env.borrow_mut().set(param.clone(), arg.clone());
    }
    env
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(val) = obj {
        *val
    } else {
        obj
    }
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (left, index) {
        (Object::Array(arr), Object::Integer(idx)) => eval_array_index_expression(&arr, idx),
        (Object::Hash(hash), idx) => eval_hash_index_expression(&hash, idx),
        (l, _) => Object::Error(format!("index operator not supported: {}", type_name(&l))),
    }
}

fn eval_array_index_expression(array: &Array, idx: i64) -> Object {
    let max = array.elements.len() as i64 - 1;
    if idx < 0 || idx > max {
        Object::Null
    } else {
        array.elements[idx as usize].clone()
    }
}

fn eval_hash_literal(
    pairs: &Vec<(Expression, Expression)>,
    env: Rc<RefCell<Environment>>,
) -> Object {
    let mut hash_pairs = HashMap::<crate::ast::HashKey, crate::object::Object>::new();
    for (key_expr, value_expr) in pairs {
        let key_obj = eval_expression(&Box::new(key_expr.clone()), env.clone());
        if is_error(&key_obj) {
            return key_obj;
        }
        let hash_key = match HashKey::from_expr(&object_to_expression(&key_obj)) {
            Some(k) => k,
            None => return Object::Error(format!("unusable as hash key: {}", type_name(&key_obj))),
        };
        let value = eval_expression(&Box::new(value_expr.clone()), env.clone());
        if is_error(&value) {
            return value;
        }
        hash_pairs.insert(hash_key, value);
    }
    Object::Hash(Hash { pairs: hash_pairs })
}

// Helper to convert evaluated Object back to Expression for HashKey::from_expr
fn object_to_expression(obj: &Object) -> Expression {
    use crate::token::{Token, TokenType};
    match obj {
        Object::String(s) => Expression::StringLiteral {
            token: Token {
                token_type: TokenType::String,
                literal: s.clone(),
            },
            value: s.clone(),
        },
        Object::Integer(i) => Expression::IntegerLiteral {
            token: Token {
                token_type: TokenType::Int,
                literal: i.to_string(),
            },
            value: *i,
        },
        Object::Boolean(b) => Expression::Boolean {
            token: Token {
                token_type: if *b {
                    TokenType::True
                } else {
                    TokenType::False
                },
                literal: b.to_string(),
            },
            value: *b,
        },
        _ => panic!("Cannot convert object to expression for hash key"),
    }
}

fn eval_hash_index_expression(hash: &Hash, index: Object) -> Object {
    match index {
        Object::Integer(i) => hash
            .pairs
            .get(&HashKey::Int(i))
            .cloned()
            .unwrap_or_else(|| Object::Null),
        Object::Boolean(b) => hash
            .pairs
            .get(&HashKey::Bool(b))
            .cloned()
            .unwrap_or_else(|| Object::Null),
        Object::String(s) => hash
            .pairs
            .get(&HashKey::String(s))
            .cloned()
            .unwrap_or_else(|| Object::Null),
        _ => Object::Error(format!("unusable as hash key: {}", type_name(&index))),
    }
}
