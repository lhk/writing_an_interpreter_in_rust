use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

#[derive(PartialEq, PartialOrd, Copy, Clone, Debug)]
pub enum Precedence {
    Lowest = 1,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index]
}

pub fn token_precedence(token_type: &TokenType) -> Precedence {
    match token_type {
        TokenType::Eq | TokenType::NotEq => Precedence::Equals,
        TokenType::Lt | TokenType::Gt => Precedence::LessGreater,
        TokenType::Plus | TokenType::Minus => Precedence::Sum,
        TokenType::Slash | TokenType::Asterisk => Precedence::Product,
        TokenType::LParen => Precedence::Call,
        TokenType::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

// note to self, here's the pseudocode for a Pratt parser:
// function parse_expression(precedence):
//     left = parse_prefix()  // parse the leftmost (prefix) part

//     while precedence < next_token_precedence():
//         advance_token()
//         left = parse_infix(left)  // parse infix using left as left operand

//     return left

// function parse_prefix():
//     // dispatch based on current token type (number, identifier, prefix op, etc.)
//     // return an expression node

// function parse_infix(left):
//     // dispatch based on current token type (infix op, call, index, etc.)
//     // parse right side with correct precedence
//     // return an expression node

pub struct Parser {
    lexer: Lexer,
    pub errors: Vec<String>,
    pub cur_token: Token,
    pub peek_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            errors: Vec::new(),
            cur_token,
            peek_token,
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn cur_token_is(&self, t: &TokenType) -> bool {
        &self.cur_token.token_type == t
    }

    pub fn peek_token_is(&self, t: &TokenType) -> bool {
        &self.peek_token.token_type == t
    }

    pub fn expect_peek(&mut self, t: &TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    pub fn peek_error(&mut self, t: &TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.token_type
        );
        self.errors.push(msg);
    }

    pub fn no_prefix_parse_fn_error(&mut self, t: TokenType) {
        let msg = format!("no prefix parse function for {:?} found", t);
        self.errors.push(msg);
    }

    // --- Main entry point ---
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.cur_token.token_type != TokenType::Eof {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        if !self.expect_peek(&TokenType::Ident) {
            return None;
        }
        let name = self.parse_identifier()?;
        if !self.expect_peek(&TokenType::Assign) {
            return None;
        }
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }
        Some(Statement::Let {
            token,
            name: Box::new(name),
            value: Box::new(value),
        })
    }

    pub fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        self.next_token();
        let return_value = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }
        Some(Statement::Return {
            token,
            return_value: Box::new(return_value),
        })
    }

    pub fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token_is(&TokenType::Semicolon) {
            self.next_token();
        }
        Some(Statement::ExpressionStatement {
            token,
            expression: Box::new(expression),
        })
    }

    pub fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let left = match self.cur_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::String => self.parse_string_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::True | TokenType::False => self.parse_boolean(),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(),
            TokenType::Function => self.parse_function_literal(),
            TokenType::LBracket => self.parse_array_literal(),
            TokenType::LBrace => self.parse_hash_literal(),
            _ => {
                self.no_prefix_parse_fn_error(self.cur_token.token_type);
                return None;
            }
        };

        let mut left = left;
        while !self.peek_token_is(&TokenType::Semicolon)
            && precedence < token_precedence(&self.peek_token.token_type)
        {
            left = match self.peek_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Slash
                | TokenType::Asterisk
                | TokenType::Eq
                | TokenType::NotEq
                | TokenType::Lt
                | TokenType::Gt => {
                    self.next_token();
                    self.parse_infix_expression(left?) // left is Option<Expression>
                }
                TokenType::LParen => {
                    self.next_token();
                    self.parse_call_expression(left?)
                }
                TokenType::LBracket => {
                    self.next_token();
                    self.parse_index_expression(left?)
                }
                _ => break,
            };
        }
        left
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        })
    }
    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        match self.cur_token.literal.parse::<i64>() {
            Ok(value) => Some(Expression::IntegerLiteral { token, value }),
            Err(_) => {
                self.errors.push(format!(
                    "could not parse '{}' as integer",
                    self.cur_token.literal
                ));
                None
            }
        }
    }
    fn parse_string_literal(&mut self) -> Option<Expression> {
        Some(Expression::StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        })
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();
        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Some(Expression::Prefix {
            token,
            operator,
            right: Box::new(right),
        })
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token.literal == "true",
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();
        let precedence = token_precedence(&self.cur_token.token_type);
        self.next_token();
        let right = self.parse_expression(precedence)?;
        Some(Expression::Infix {
            token,
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }
        exp
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        if !self.expect_peek(&TokenType::LParen) {
            return None;
        }
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }
        if !self.expect_peek(&TokenType::LBrace) {
            return None;
        }
        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token_is(&TokenType::Else) {
            self.next_token();
            if !self.expect_peek(&TokenType::LBrace) {
                return None;
            }
            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };
        Some(Expression::If {
            token,
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        let mut statements = Vec::new();
        self.next_token();
        while !self.cur_token_is(&TokenType::RBrace) && !self.cur_token_is(&TokenType::Eof) {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }
        Some(Statement::Block { token, statements })
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        if !self.expect_peek(&TokenType::LParen) {
            return None;
        }
        let parameters = self.parse_function_parameters()?;
        if !self.expect_peek(&TokenType::LBrace) {
            return None;
        }
        let body = self.parse_block_statement()?;
        Some(Expression::FunctionLiteral {
            token,
            parameters,
            body: Box::new(body),
        })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Expression>> {
        let mut identifiers = Vec::new();
        if self.peek_token_is(&TokenType::RParen) {
            self.next_token();
            return Some(identifiers);
        }
        self.next_token();
        identifiers.push(self.parse_identifier()?);
        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            identifiers.push(self.parse_identifier()?);
        }
        if !self.expect_peek(&TokenType::RParen) {
            return None;
        }
        Some(identifiers)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let arguments = self.parse_expression_list(TokenType::RParen)?;
        Some(Expression::Call {
            token,
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Expression>> {
        let mut list = Vec::new();
        if self.peek_token_is(&end) {
            self.next_token();
            return Some(list);
        }
        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?);
        while self.peek_token_is(&TokenType::Comma) {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }
        if !self.expect_peek(&end) {
            return None;
        }
        Some(list)
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(TokenType::RBracket)?;
        Some(Expression::ArrayLiteral { token, elements })
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(&TokenType::RBracket) {
            return None;
        }
        Some(Expression::IndexExpression {
            token,
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let mut pairs = Vec::new();
        while !self.peek_token_is(&TokenType::RBrace) {
            self.next_token();
            let key_expr = self.parse_expression(Precedence::Lowest)?;
            if !self.expect_peek(&TokenType::Colon) {
                return None;
            }
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.push((key_expr, value));
            if !self.peek_token_is(&TokenType::RBrace) && !self.expect_peek(&TokenType::Comma) {
                return None;
            }
        }
        if !self.expect_peek(&TokenType::RBrace) {
            return None;
        }
        Some(Expression::HashLiteral { token, pairs })
    }
}
