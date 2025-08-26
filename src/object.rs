use crate::ast::HashKey;
use crate::ast::Statement;
use crate::environment::Environment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Array),
    Hash(Hash),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    Function(Function),
    Builtin(Builtin),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "{}", s),
            Object::Array(arr) => write!(f, "{}", arr),
            Object::Hash(hash) => write!(f, "{}", hash),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(obj) => write!(f, "{}", obj),
            Object::Error(msg) => write!(f, "ERROR: {}", msg),
            Object::Function(func) => write!(f, "{}", func),
            Object::Builtin(_) => write!(f, "builtin function"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub elements: Vec<Object>,
}
impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let elems: Vec<String> = self.elements.iter().map(|e| e.to_string()).collect();
        write!(f, "[{}]", elems.join(", "))
    }
}

// In the AST, a Hash is a sequence of (Expression, Expression) pairs
// That's because the key can be something like 2+3, i.e, {2+3: "value"}
// The evaluator converts that to a primitive value and we have a wrapper enum around all valid hashkeys which is used as a key here
#[derive(Debug, Clone, PartialEq)]
pub struct Hash {
    pub pairs: HashMap<HashKey, Object>,
}

impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut pairs = Vec::new();
        for (key, value) in self.pairs.iter() {
            pairs.push(format!("{:?}: {}", key, value));
        }
        write!(f, "{{{}}}", pairs.join(", "))
    }
}
#[derive(Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<String>, // identifiers as parameters
    pub body: Box<Statement>,
    pub env: Rc<RefCell<Environment>>,
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn({}) {{ ... }}", self.parameters.join(", "))
    }
}

#[derive(Clone)]
pub struct Builtin {
    pub func: fn(Vec<Object>) -> Object,
}

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin function>")
    }
}

impl PartialEq for Builtin {
    fn eq(&self, _other: &Self) -> bool {
        false // Builtins are not comparable
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn({}) {{\n{}\n}}",
            self.parameters.join(", "),
            self.body
        )
    }
}
