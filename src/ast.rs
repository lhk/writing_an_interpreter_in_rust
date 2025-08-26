use crate::token::Token;
use std::fmt;
use std::hash::Hash;

// Very important change from Go to Rust.
// Instead of various interfaces that are implicitly implemented, we now get the type safety of enums.
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

// Display implementations
impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Statement(stmt) => write!(f, "{}", stmt),
            Node::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Statement {
    Let {
        token: Token,
        name: Box<Expression>,
        value: Box<Expression>,
    },
    Return {
        token: Token,
        return_value: Box<Expression>,
    },
    ExpressionStatement {
        token: Token,
        expression: Box<Expression>,
    },
    Block {
        token: Token,
        statements: Vec<Statement>,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let {
                token: _,
                name,
                value,
            } => {
                write!(f, "let {} = {};", name, value)
            }
            Statement::Return {
                token: _,
                return_value,
            } => {
                write!(f, "return {};", return_value)
            }
            Statement::ExpressionStatement {
                token: _,
                expression,
            } => {
                write!(f, "{}", expression)
            }
            Statement::Block {
                token: _,
                statements,
            } => {
                for stmt in statements {
                    write!(f, "{}", stmt)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Expression {
    Identifier {
        token: Token,
        value: String,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    IntegerLiteral {
        token: Token,
        value: i64,
    },
    Prefix {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        token: Token,
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    If {
        token: Token,
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
    FunctionLiteral {
        token: Token,
        parameters: Vec<Expression>,
        body: Box<Statement>,
    },
    Call {
        token: Token,
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    StringLiteral {
        token: Token,
        value: String,
    },
    ArrayLiteral {
        token: Token,
        elements: Vec<Expression>,
    },
    IndexExpression {
        token: Token,
        left: Box<Expression>,
        index: Box<Expression>,
    },
    HashLiteral {
        token: Token,
        pairs: Vec<(Expression, Expression)>,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier { value, .. } => write!(f, "{}", value),
            Expression::Boolean { token, .. } => write!(f, "{}", token.literal),
            Expression::IntegerLiteral { token, .. } => write!(f, "{}", token.literal),
            Expression::Prefix {
                operator, right, ..
            } => write!(f, "({}{})", operator, right),
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::If {
                condition,
                consequence,
                alternative,
                ..
            } => {
                write!(f, "if{} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, "else {}", alt)?;
                }
                Ok(())
            }
            Expression::FunctionLiteral {
                token,
                parameters,
                body,
            } => {
                let params: Vec<String> = parameters.iter().map(|p| p.to_string()).collect();
                write!(f, "{}({}) {}", token.literal, params.join(", "), body)
            }
            Expression::Call {
                function,
                arguments,
                ..
            } => {
                let args: Vec<String> = arguments.iter().map(|a| a.to_string()).collect();
                write!(f, "{}({})", function, args.join(", "))
            }
            Expression::StringLiteral { token, .. } => write!(f, "{}", token.literal),
            Expression::ArrayLiteral { elements, .. } => {
                let elems: Vec<String> = elements.iter().map(|e| e.to_string()).collect();
                write!(f, "[{}]", elems.join(", "))
            }
            Expression::IndexExpression { left, index, .. } => write!(f, "({}[{}])", left, index),
            Expression::HashLiteral { pairs, .. } => {
                let mut pair_strs = Vec::new();
                for (k, v) in pairs {
                    pair_strs.push(format!("{}:{}", k, v));
                }
                write!(f, "{{{}}}", pair_strs.join(", "))
            }
        }
    }
}

// I'm tempted to move this into evaluator.rs
// When the parser outputs the AST, a Hash is still a vector of (Expression, Expression) pairs
// That's because we can't evaluate expressions to hash keys at parsing time
// In the evaluator we convert expressions to these keys
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum HashKey {
    String(String),
    Int(i64),
    Bool(bool),
}

impl HashKey {
    pub fn from_expr(expr: &Expression) -> Option<HashKey> {
        match expr {
            Expression::StringLiteral { value, .. } => Some(HashKey::String(value.clone())),
            Expression::IntegerLiteral { value, .. } => Some(HashKey::Int(*value)),
            Expression::Boolean { value, .. } => Some(HashKey::Bool(*value)),
            _ => None,
        }
    }
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HashKey::String(s) => write!(f, "{}", s),
            HashKey::Int(i) => write!(f, "{}", i),
            HashKey::Bool(b) => write!(f, "{}", b),
        }
    }
}
pub struct Program {
    pub statements: Vec<Statement>,
}

// Display implementation for Program
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
