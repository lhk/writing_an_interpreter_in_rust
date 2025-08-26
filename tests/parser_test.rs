use writing_an_interpreter_in_rust::ast::{Expression, Statement};
use writing_an_interpreter_in_rust::lexer::Lexer;
use writing_an_interpreter_in_rust::parser::Parser;

#[derive(Debug)]
pub enum ExpectedValue<'a> {
    Int(i64),
    Bool(bool),
    Ident(&'a str),
}

pub struct TestCase<'a> {
    pub input: &'a str,
    pub expected_ident: &'a str,
    pub expected_value: ExpectedValue<'a>,
}

#[test]
fn test_let_statements() {
    let tests = vec![
        TestCase {
            input: "let x = 5;",
            expected_ident: "x",
            expected_value: ExpectedValue::Int(5),
        },
        TestCase {
            input: "let y = true;",
            expected_ident: "y",
            expected_value: ExpectedValue::Bool(true),
        },
        TestCase {
            input: "let foobar = y;",
            expected_ident: "foobar",
            expected_value: ExpectedValue::Ident("y"),
        },
    ];

    for test in tests {
        let l = Lexer::new(test.input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Let { name, value, .. } => {
                match &**name {
                    Expression::Identifier { value: ident, .. } => {
                        assert_eq!(ident, test.expected_ident)
                    }
                    _ => panic!("letStmt.Name is not Identifier"),
                }
                match (&test.expected_value, &**value) {
                    (ExpectedValue::Int(i), Expression::IntegerLiteral { value: v, .. }) => {
                        assert_eq!(v, i)
                    }
                    (ExpectedValue::Bool(b), Expression::Boolean { value: v, .. }) => {
                        assert_eq!(v, b)
                    }
                    (ExpectedValue::Ident(s), Expression::Identifier { value: v, .. }) => {
                        assert_eq!(v, s)
                    }
                    _ => panic!("letStmt.Value type mismatch"),
                }
            }
            _ => panic!("stmt is not LetStatement"),
        }
    }
}

#[test]
fn test_return_statements() {
    let tests = vec![
        TestCase {
            input: "return 5;",
            expected_ident: "",
            expected_value: ExpectedValue::Int(5),
        },
        TestCase {
            input: "return true;",
            expected_ident: "",
            expected_value: ExpectedValue::Bool(true),
        },
        TestCase {
            input: "return foobar;",
            expected_ident: "",
            expected_value: ExpectedValue::Ident("foobar"),
        },
    ];

    for test in tests {
        let l = Lexer::new(test.input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Return { return_value, .. } => {
                match (&test.expected_value, &**return_value) {
                    (ExpectedValue::Int(i), Expression::IntegerLiteral { value: v, .. }) => {
                        assert_eq!(v, i)
                    }
                    (ExpectedValue::Bool(b), Expression::Boolean { value: v, .. }) => {
                        assert_eq!(v, b)
                    }
                    (ExpectedValue::Ident(s), Expression::Identifier { value: v, .. }) => {
                        assert_eq!(v, s)
                    }
                    _ => panic!("returnStmt.Value type mismatch"),
                }
            }
            _ => panic!("stmt is not ReturnStatement"),
        }
    }
}

// Additional tests should use the same TestCase and ExpectedValue pattern as above.

#[test]
fn test_identifier_expression() {
    let input = "foobar;";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(
        program.statements.len(),
        1,
        "program has not enough statements. got={}",
        program.statements.len()
    );
    match &program.statements[0] {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::Identifier { value, token } => {
                assert_eq!(value, "foobar", "ident.Value not foobar. got={}", value);
                assert_eq!(
                    &token.literal, "foobar",
                    "ident.TokenLiteral not foobar. got={}",
                    token.literal
                );
            }
            _ => panic!("exp not Identifier. got={:?}", expression),
        },
        _ => panic!(
            "program.Statements[0] is not ExpressionStatement. got={:?}",
            program.statements[0]
        ),
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(
        program.statements.len(),
        1,
        "program has not enough statements. got={}",
        program.statements.len()
    );
    match &program.statements[0] {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::IntegerLiteral { value, token } => {
                assert_eq!(*value, 5, "literal.Value not 5. got={}", value);
                assert_eq!(
                    &token.literal, "5",
                    "literal.TokenLiteral not 5. got={}",
                    token.literal
                );
            }
            _ => panic!("exp not IntegerLiteral. got={:?}", expression),
        },
        _ => panic!(
            "program.Statements[0] is not ExpressionStatement. got={:?}",
            program.statements[0]
        ),
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    struct PrefixTest<'a> {
        input: &'a str,
        operator: &'a str,
        value: ExpectedValue<'a>,
    }
    let tests = vec![
        PrefixTest {
            input: "!5;",
            operator: "!",
            value: ExpectedValue::Int(5),
        },
        PrefixTest {
            input: "-15;",
            operator: "-",
            value: ExpectedValue::Int(15),
        },
        PrefixTest {
            input: "!foobar;",
            operator: "!",
            value: ExpectedValue::Ident("foobar"),
        },
        PrefixTest {
            input: "-foobar;",
            operator: "-",
            value: ExpectedValue::Ident("foobar"),
        },
        PrefixTest {
            input: "!true;",
            operator: "!",
            value: ExpectedValue::Bool(true),
        },
        PrefixTest {
            input: "!false;",
            operator: "!",
            value: ExpectedValue::Bool(false),
        },
    ];
    for test in tests {
        let l = Lexer::new(test.input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(
            program.statements.len(),
            1,
            "program.Statements does not contain 1 statements. got={}",
            program.statements.len()
        );
        match &program.statements[0] {
            Statement::ExpressionStatement { expression, .. } => match &**expression {
                Expression::Prefix {
                    operator, right, ..
                } => {
                    assert_eq!(
                        operator, test.operator,
                        "exp.Operator is not '{}'. got={}",
                        test.operator, operator
                    );
                    match (&test.value, &**right) {
                        (ExpectedValue::Int(i), Expression::IntegerLiteral { value: v, .. }) => {
                            assert_eq!(v, i)
                        }
                        (ExpectedValue::Bool(b), Expression::Boolean { value: v, .. }) => {
                            assert_eq!(v, b)
                        }
                        (ExpectedValue::Ident(s), Expression::Identifier { value: v, .. }) => {
                            assert_eq!(v, s)
                        }
                        _ => panic!(
                            "prefix right value type mismatch: expected {:?}, got {:?}",
                            test.value, right
                        ),
                    }
                }
                _ => panic!(
                    "stmt.Expression is not PrefixExpression. got={:?}",
                    expression
                ),
            },
            _ => panic!(
                "program.Statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    struct InfixTest<'a> {
        input: &'a str,
        left: ExpectedValue<'a>,
        operator: &'a str,
        right: ExpectedValue<'a>,
    }
    let tests = vec![
        InfixTest {
            input: "5 + 5;",
            left: ExpectedValue::Int(5),
            operator: "+",
            right: ExpectedValue::Int(5),
        },
        InfixTest {
            input: "5 - 5;",
            left: ExpectedValue::Int(5),
            operator: "-",
            right: ExpectedValue::Int(5),
        },
        InfixTest {
            input: "5 * 5;",
            left: ExpectedValue::Int(5),
            operator: "*",
            right: ExpectedValue::Int(5),
        },
        InfixTest {
            input: "5 / 5;",
            left: ExpectedValue::Int(5),
            operator: "/",
            right: ExpectedValue::Int(5),
        },
        InfixTest {
            input: "5 > 5;",
            left: ExpectedValue::Int(5),
            operator: ">",
            right: ExpectedValue::Int(5),
        },
        InfixTest {
            input: "5 < 5;",
            left: ExpectedValue::Int(5),
            operator: "<",
            right: ExpectedValue::Int(5),
        },
        InfixTest {
            input: "5 == 5;",
            left: ExpectedValue::Int(5),
            operator: "==",
            right: ExpectedValue::Int(5),
        },
        InfixTest {
            input: "5 != 5;",
            left: ExpectedValue::Int(5),
            operator: "!=",
            right: ExpectedValue::Int(5),
        },
        InfixTest {
            input: "foobar + barfoo;",
            left: ExpectedValue::Ident("foobar"),
            operator: "+",
            right: ExpectedValue::Ident("barfoo"),
        },
        InfixTest {
            input: "foobar - barfoo;",
            left: ExpectedValue::Ident("foobar"),
            operator: "-",
            right: ExpectedValue::Ident("barfoo"),
        },
        InfixTest {
            input: "foobar * barfoo;",
            left: ExpectedValue::Ident("foobar"),
            operator: "*",
            right: ExpectedValue::Ident("barfoo"),
        },
        InfixTest {
            input: "foobar / barfoo;",
            left: ExpectedValue::Ident("foobar"),
            operator: "/",
            right: ExpectedValue::Ident("barfoo"),
        },
        InfixTest {
            input: "foobar > barfoo;",
            left: ExpectedValue::Ident("foobar"),
            operator: ">",
            right: ExpectedValue::Ident("barfoo"),
        },
        InfixTest {
            input: "foobar < barfoo;",
            left: ExpectedValue::Ident("foobar"),
            operator: "<",
            right: ExpectedValue::Ident("barfoo"),
        },
        InfixTest {
            input: "foobar == barfoo;",
            left: ExpectedValue::Ident("foobar"),
            operator: "==",
            right: ExpectedValue::Ident("barfoo"),
        },
        InfixTest {
            input: "foobar != barfoo;",
            left: ExpectedValue::Ident("foobar"),
            operator: "!=",
            right: ExpectedValue::Ident("barfoo"),
        },
        InfixTest {
            input: "true == true",
            left: ExpectedValue::Bool(true),
            operator: "==",
            right: ExpectedValue::Bool(true),
        },
        InfixTest {
            input: "true != false",
            left: ExpectedValue::Bool(true),
            operator: "!=",
            right: ExpectedValue::Bool(false),
        },
        InfixTest {
            input: "false == false",
            left: ExpectedValue::Bool(false),
            operator: "==",
            right: ExpectedValue::Bool(false),
        },
    ];
    for test in tests {
        let l = Lexer::new(test.input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(
            program.statements.len(),
            1,
            "program.Statements does not contain 1 statements. got={}",
            program.statements.len()
        );
        match &program.statements[0] {
            Statement::ExpressionStatement { expression, .. } => match &**expression {
                Expression::Infix {
                    left,
                    operator,
                    right,
                    ..
                } => {
                    assert_eq!(
                        operator, test.operator,
                        "exp.Operator is not '{}'. got={}",
                        test.operator, operator
                    );
                    match (&test.left, &**left) {
                        (ExpectedValue::Int(i), Expression::IntegerLiteral { value: v, .. }) => {
                            assert_eq!(v, i)
                        }
                        (ExpectedValue::Bool(b), Expression::Boolean { value: v, .. }) => {
                            assert_eq!(v, b)
                        }
                        (ExpectedValue::Ident(s), Expression::Identifier { value: v, .. }) => {
                            assert_eq!(v, s)
                        }
                        _ => panic!(
                            "infix left value type mismatch: expected {:?}, got {:?}",
                            test.left, left
                        ),
                    }
                    match (&test.right, &**right) {
                        (ExpectedValue::Int(i), Expression::IntegerLiteral { value: v, .. }) => {
                            assert_eq!(v, i)
                        }
                        (ExpectedValue::Bool(b), Expression::Boolean { value: v, .. }) => {
                            assert_eq!(v, b)
                        }
                        (ExpectedValue::Ident(s), Expression::Identifier { value: v, .. }) => {
                            assert_eq!(v, s)
                        }
                        _ => panic!(
                            "infix right value type mismatch: expected {:?}, got {:?}",
                            test.right, right
                        ),
                    }
                }
                _ => panic!(
                    "stmt.Expression is not InfixExpression. got={:?}",
                    expression
                ),
            },
            _ => panic!(
                "program.Statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    struct PrecedenceTest<'a> {
        input: &'a str,
        expected: &'a str,
    }
    let tests = vec![
        PrecedenceTest {
            input: "-a * b",
            expected: "((-a) * b)",
        },
        PrecedenceTest {
            input: "!-a",
            expected: "(!(-a))",
        },
        PrecedenceTest {
            input: "a + b + c",
            expected: "((a + b) + c)",
        },
        PrecedenceTest {
            input: "a + b - c",
            expected: "((a + b) - c)",
        },
        PrecedenceTest {
            input: "a * b * c",
            expected: "((a * b) * c)",
        },
        PrecedenceTest {
            input: "a * b / c",
            expected: "((a * b) / c)",
        },
        PrecedenceTest {
            input: "a + b / c",
            expected: "(a + (b / c))",
        },
        PrecedenceTest {
            input: "a + b * c + d / e - f",
            expected: "(((a + (b * c)) + (d / e)) - f)",
        },
        PrecedenceTest {
            input: "3 + 4; -5 * 5",
            expected: "(3 + 4)((-5) * 5)",
        },
        PrecedenceTest {
            input: "5 > 4 == 3 < 4",
            expected: "((5 > 4) == (3 < 4))",
        },
        PrecedenceTest {
            input: "5 < 4 != 3 > 4",
            expected: "((5 < 4) != (3 > 4))",
        },
        PrecedenceTest {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        PrecedenceTest {
            input: "true",
            expected: "true",
        },
        PrecedenceTest {
            input: "false",
            expected: "false",
        },
        PrecedenceTest {
            input: "3 > 5 == false",
            expected: "((3 > 5) == false)",
        },
        PrecedenceTest {
            input: "3 < 5 == true",
            expected: "((3 < 5) == true)",
        },
        PrecedenceTest {
            input: "1 + (2 + 3) + 4",
            expected: "((1 + (2 + 3)) + 4)",
        },
        PrecedenceTest {
            input: "(5 + 5) * 2",
            expected: "((5 + 5) * 2)",
        },
        PrecedenceTest {
            input: "2 / (5 + 5)",
            expected: "(2 / (5 + 5))",
        },
        PrecedenceTest {
            input: "(5 + 5) * 2 * (5 + 5)",
            expected: "(((5 + 5) * 2) * (5 + 5))",
        },
        PrecedenceTest {
            input: "-(5 + 5)",
            expected: "(-(5 + 5))",
        },
        PrecedenceTest {
            input: "!(true == true)",
            expected: "(!(true == true))",
        },
        PrecedenceTest {
            input: "a + add(b * c) + d",
            expected: "((a + add((b * c))) + d)",
        },
        PrecedenceTest {
            input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        PrecedenceTest {
            input: "add(a + b + c * d / f + g)",
            expected: "add((((a + b) + ((c * d) / f)) + g))",
        },
        PrecedenceTest {
            input: "a * [1, 2, 3, 4][b * c] * d",
            expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        },
        PrecedenceTest {
            input: "add(a * b[2], b[1], 2 * [1, 2][1])",
            expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        },
    ];
    for test in tests {
        let l = Lexer::new(test.input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        let actual = format!("{}", program);
        assert_eq!(
            actual, test.expected,
            "expected={}, got={}",
            test.expected, actual
        );
    }
}

#[test]
fn test_boolean_expression() {
    struct BoolTest<'a> {
        input: &'a str,
        expected: bool,
    }
    let tests = vec![
        BoolTest {
            input: "true;",
            expected: true,
        },
        BoolTest {
            input: "false;",
            expected: false,
        },
    ];
    for test in tests {
        let l = Lexer::new(test.input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(
            program.statements.len(),
            1,
            "program has not enough statements. got={}",
            program.statements.len()
        );
        match &program.statements[0] {
            Statement::ExpressionStatement { expression, .. } => match &**expression {
                Expression::Boolean { value, .. } => assert_eq!(
                    value, &test.expected,
                    "boolean.Value not {}. got={}",
                    test.expected, value
                ),
                _ => panic!("exp not Boolean. got={:?}", expression),
            },
            _ => panic!(
                "program.Statements[0] is not ExpressionStatement. got={:?}",
                program.statements[0]
            ),
        }
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(
        program.statements.len(),
        1,
        "program.Statements does not contain 1 statements. got={}",
        program.statements.len()
    );
    match &program.statements[0] {
        Statement::ExpressionStatement { expression, .. } => {
            match &**expression {
                Expression::If {
                    condition,
                    consequence,
                    alternative,
                    ..
                } => {
                    // Test condition: x < y
                    match &**condition {
                        Expression::Infix {
                            left,
                            operator,
                            right,
                            ..
                        } => {
                            match &**left {
                                Expression::Identifier { value, .. } => assert_eq!(value, "x"),
                                _ => panic!("if condition left is not Identifier"),
                            }
                            assert_eq!(operator, "<");
                            match &**right {
                                Expression::Identifier { value, .. } => assert_eq!(value, "y"),
                                _ => panic!("if condition right is not Identifier"),
                            }
                        }
                        _ => panic!("if condition is not InfixExpression"),
                    }
                    // Test consequence: { x }
                    match &**consequence {
                        Statement::Block { statements, .. } => {
                            assert_eq!(
                                statements.len(),
                                1,
                                "consequence is not 1 statements. got={}",
                                statements.len()
                            );
                            match &statements[0] {
                                Statement::ExpressionStatement { expression, .. } => {
                                    match &**expression {
                                        Expression::Identifier { value, .. } => {
                                            assert_eq!(value, "x")
                                        }
                                        _ => panic!("consequence expression is not Identifier"),
                                    }
                                }
                                _ => panic!("consequence statement is not ExpressionStatement"),
                            }
                        }
                        _ => panic!("consequence is not BlockStatement"),
                    }
                    // Test alternative: None
                    assert!(
                        alternative.is_none(),
                        "exp.Alternative.Statements was not nil. got={:?}",
                        alternative
                    );
                }
                _ => panic!("stmt.Expression is not IfExpression. got={:?}", expression),
            }
        }
        _ => panic!(
            "program.Statements[0] is not ExpressionStatement. got={:?}",
            program.statements[0]
        ),
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(
        program.statements.len(),
        1,
        "program.Statements does not contain 1 statements. got={}",
        program.statements.len()
    );
    match &program.statements[0] {
        Statement::ExpressionStatement { expression, .. } => {
            match &**expression {
                Expression::If {
                    condition,
                    consequence,
                    alternative,
                    ..
                } => {
                    // Test condition: x < y
                    match &**condition {
                        Expression::Infix {
                            left,
                            operator,
                            right,
                            ..
                        } => {
                            match &**left {
                                Expression::Identifier { value, .. } => assert_eq!(value, "x"),
                                _ => panic!("if condition left is not Identifier"),
                            }
                            assert_eq!(operator, "<");
                            match &**right {
                                Expression::Identifier { value, .. } => assert_eq!(value, "y"),
                                _ => panic!("if condition right is not Identifier"),
                            }
                        }
                        _ => panic!("if condition is not InfixExpression"),
                    }
                    // Test consequence: { x }
                    match &**consequence {
                        Statement::Block { statements, .. } => {
                            assert_eq!(
                                statements.len(),
                                1,
                                "consequence is not 1 statements. got={}",
                                statements.len()
                            );
                            match &statements[0] {
                                Statement::ExpressionStatement { expression, .. } => {
                                    match &**expression {
                                        Expression::Identifier { value, .. } => {
                                            assert_eq!(value, "x")
                                        }
                                        _ => panic!("consequence expression is not Identifier"),
                                    }
                                }
                                _ => panic!("consequence statement is not ExpressionStatement"),
                            }
                        }
                        _ => panic!("consequence is not BlockStatement"),
                    }
                    // Test alternative: Some(block)
                    match alternative {
                        Some(alt_block) => match &**alt_block {
                            Statement::Block { statements, .. } => {
                                assert_eq!(
                                    statements.len(),
                                    1,
                                    "exp.Alternative.Statements does not contain 1 statements. got={}",
                                    statements.len()
                                );
                                match &statements[0] {
                                    Statement::ExpressionStatement { expression, .. } => {
                                        match &**expression {
                                            Expression::Identifier { value, .. } => {
                                                assert_eq!(value, "y")
                                            }
                                            _ => panic!("alternative expression is not Identifier"),
                                        }
                                    }
                                    _ => panic!("alternative statement is not ExpressionStatement"),
                                }
                            }
                            _ => panic!("alternative is not BlockStatement"),
                        },
                        None => panic!("exp.Alternative.Statements was nil"),
                    }
                }
                _ => panic!("stmt.Expression is not IfExpression. got={:?}", expression),
            }
        }
        _ => panic!(
            "program.Statements[0] is not ExpressionStatement. got={:?}",
            program.statements[0]
        ),
    }
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(
        program.statements.len(),
        1,
        "program.Statements does not contain 1 statements. got={}",
        program.statements.len()
    );
    match &program.statements[0] {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::FunctionLiteral {
                parameters, body, ..
            } => {
                assert_eq!(
                    parameters.len(),
                    2,
                    "function literal parameters wrong. want 2, got={}",
                    parameters.len()
                );
                match &parameters[0] {
                    Expression::Identifier { value, .. } => assert_eq!(value, "x"),
                    _ => panic!("first parameter is not Identifier"),
                }
                match &parameters[1] {
                    Expression::Identifier { value, .. } => assert_eq!(value, "y"),
                    _ => panic!("second parameter is not Identifier"),
                }
                match &**body {
                    Statement::Block { statements, .. } => {
                        assert_eq!(
                            statements.len(),
                            1,
                            "function.Body.Statements has not 1 statements. got={}",
                            statements.len()
                        );
                        match &statements[0] {
                            Statement::ExpressionStatement { expression, .. } => {
                                match &**expression {
                                    Expression::Infix {
                                        left,
                                        operator,
                                        right,
                                        ..
                                    } => {
                                        match &**left {
                                            Expression::Identifier { value, .. } => {
                                                assert_eq!(value, "x")
                                            }
                                            _ => panic!("function body left is not Identifier"),
                                        }
                                        assert_eq!(operator, "+");
                                        match &**right {
                                            Expression::Identifier { value, .. } => {
                                                assert_eq!(value, "y")
                                            }
                                            _ => panic!("function body right is not Identifier"),
                                        }
                                    }
                                    _ => panic!("function body is not InfixExpression"),
                                }
                            }
                            _ => panic!("function body stmt is not ExpressionStatement"),
                        }
                    }
                    _ => panic!("function body is not BlockStatement"),
                }
            }
            _ => panic!(
                "stmt.Expression is not FunctionLiteral. got={:?}",
                expression
            ),
        },
        _ => panic!(
            "program.Statements[0] is not ExpressionStatement. got={:?}",
            program.statements[0]
        ),
    }
}

#[test]
fn test_function_parameter_parsing() {
    struct ParamTest<'a> {
        input: &'a str,
        expected_params: Vec<&'a str>,
    }
    let tests = vec![
        ParamTest {
            input: "fn() {};",
            expected_params: vec![],
        },
        ParamTest {
            input: "fn(x) {};",
            expected_params: vec!["x"],
        },
        ParamTest {
            input: "fn(x, y, z) {};",
            expected_params: vec!["x", "y", "z"],
        },
    ];
    for test in tests {
        let l = Lexer::new(test.input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        let stmt = &program.statements[0];
        match stmt {
            Statement::ExpressionStatement { expression, .. } => match &**expression {
                Expression::FunctionLiteral { parameters, .. } => {
                    assert_eq!(
                        parameters.len(),
                        test.expected_params.len(),
                        "length parameters wrong. want {}, got={}",
                        test.expected_params.len(),
                        parameters.len()
                    );
                    for (i, ident) in test.expected_params.iter().enumerate() {
                        match &parameters[i] {
                            Expression::Identifier { value, .. } => assert_eq!(value, ident),
                            _ => panic!("parameter is not Identifier"),
                        }
                    }
                }
                _ => panic!("stmt.Expression is not FunctionLiteral"),
            },
            _ => panic!("stmt is not ExpressionStatement"),
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 1);
    match &program.statements[0] {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::Call {
                function,
                arguments,
                ..
            } => {
                match &**function {
                    Expression::Identifier { value, .. } => assert_eq!(value, "add"),
                    _ => panic!("function is not Identifier"),
                }
                assert_eq!(
                    arguments.len(),
                    3,
                    "wrong length of arguments. got={}",
                    arguments.len()
                );
                match &arguments[0] {
                    Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 1),
                    _ => panic!("first argument is not IntegerLiteral"),
                }
                match &arguments[1] {
                    Expression::Infix {
                        left,
                        operator,
                        right,
                        ..
                    } => {
                        match &**left {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 2),
                            _ => panic!("left of second argument is not IntegerLiteral"),
                        }
                        assert_eq!(operator, "*");
                        match &**right {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 3),
                            _ => panic!("right of second argument is not IntegerLiteral"),
                        }
                    }
                    _ => panic!("second argument is not InfixExpression"),
                }
                match &arguments[2] {
                    Expression::Infix {
                        left,
                        operator,
                        right,
                        ..
                    } => {
                        match &**left {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 4),
                            _ => panic!("left of third argument is not IntegerLiteral"),
                        }
                        assert_eq!(operator, "+");
                        match &**right {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 5),
                            _ => panic!("right of third argument is not IntegerLiteral"),
                        }
                    }
                    _ => panic!("third argument is not InfixExpression"),
                }
            }
            _ => panic!("stmt.Expression is not CallExpression"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}

#[test]
fn test_call_expression_parameter_parsing() {
    struct CallTest<'a> {
        input: &'a str,
        expected_ident: &'a str,
        expected_args: Vec<&'a str>,
    }
    let tests = vec![
        CallTest {
            input: "add();",
            expected_ident: "add",
            expected_args: vec![],
        },
        CallTest {
            input: "add(1);",
            expected_ident: "add",
            expected_args: vec!["1"],
        },
        CallTest {
            input: "add(1, 2 * 3, 4 + 5);",
            expected_ident: "add",
            expected_args: vec!["1", "(2 * 3)", "(4 + 5)"],
        },
    ];
    for test in tests {
        let l = Lexer::new(test.input.to_string());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        let stmt = &program.statements[0];
        match stmt {
            Statement::ExpressionStatement { expression, .. } => match &**expression {
                Expression::Call {
                    function,
                    arguments,
                    ..
                } => {
                    match &**function {
                        Expression::Identifier { value, .. } => {
                            assert_eq!(value, test.expected_ident)
                        }
                        _ => panic!("function is not Identifier"),
                    }
                    assert_eq!(
                        arguments.len(),
                        test.expected_args.len(),
                        "wrong number of arguments. want={}, got={}",
                        test.expected_args.len(),
                        arguments.len()
                    );
                    for (i, arg) in test.expected_args.iter().enumerate() {
                        assert_eq!(
                            format!("{}", arguments[i]),
                            *arg,
                            "argument {} wrong. want={}, got={}",
                            i,
                            arg,
                            format!("{}", arguments[i])
                        );
                    }
                }
                _ => panic!("stmt.Expression is not CallExpression"),
            },
            _ => panic!("stmt is not ExpressionStatement"),
        }
    }
}

#[test]
fn test_string_literal_expression() {
    let input = "\"hello world\";";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    let stmt = &program.statements[0];
    match stmt {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::StringLiteral { value, .. } => assert_eq!(value, "hello world"),
            _ => panic!("exp not StringLiteral"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}

#[test]
fn test_parsing_empty_array_literals() {
    let input = "[]";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    let stmt = &program.statements[0];
    match stmt {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::ArrayLiteral { elements, .. } => assert_eq!(
                elements.len(),
                0,
                "len(array.Elements) not 0. got={}",
                elements.len()
            ),
            _ => panic!("exp not ArrayLiteral"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}

#[test]
fn test_parsing_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    let stmt = &program.statements[0];
    match stmt {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::ArrayLiteral { elements, .. } => {
                assert_eq!(
                    elements.len(),
                    3,
                    "len(array.Elements) not 3. got={}",
                    elements.len()
                );
                match &elements[0] {
                    Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 1),
                    _ => panic!("first element is not IntegerLiteral"),
                }
                match &elements[1] {
                    Expression::Infix {
                        left,
                        operator,
                        right,
                        ..
                    } => {
                        match &**left {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 2),
                            _ => panic!("left of second element is not IntegerLiteral"),
                        }
                        assert_eq!(operator, "*");
                        match &**right {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 2),
                            _ => panic!("right of second element is not IntegerLiteral"),
                        }
                    }
                    _ => panic!("second element is not InfixExpression"),
                }
                match &elements[2] {
                    Expression::Infix {
                        left,
                        operator,
                        right,
                        ..
                    } => {
                        match &**left {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 3),
                            _ => panic!("left of third element is not IntegerLiteral"),
                        }
                        assert_eq!(operator, "+");
                        match &**right {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 3),
                            _ => panic!("right of third element is not IntegerLiteral"),
                        }
                    }
                    _ => panic!("third element is not InfixExpression"),
                }
            }
            _ => panic!("exp not ArrayLiteral"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}

#[test]
fn test_parsing_index_expressions() {
    let input = "myArray[1 + 1]";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    let stmt = &program.statements[0];
    match stmt {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::IndexExpression { left, index, .. } => {
                match &**left {
                    Expression::Identifier { value, .. } => assert_eq!(value, "myArray"),
                    _ => panic!("left of index expression is not Identifier"),
                }
                match &**index {
                    Expression::Infix {
                        left,
                        operator,
                        right,
                        ..
                    } => {
                        match &**left {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 1),
                            _ => panic!("left of index is not IntegerLiteral"),
                        }
                        assert_eq!(operator, "+");
                        match &**right {
                            Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 1),
                            _ => panic!("right of index is not IntegerLiteral"),
                        }
                    }
                    _ => panic!("index is not InfixExpression"),
                }
            }
            _ => panic!("exp not IndexExpression"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}
#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{}";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    let stmt = &program.statements[0];
    match stmt {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::HashLiteral { pairs, .. } => assert_eq!(
                pairs.len(),
                0,
                "hash.Pairs has wrong length. got={}",
                pairs.len()
            ),
            _ => panic!("exp is not HashLiteral"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}

#[test]
fn test_parsing_hash_literals_string_keys() {
    let input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    let stmt = &program.statements[0];
    match stmt {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::HashLiteral { pairs, .. } => {
                let expected = vec![("one", 1), ("two", 2), ("three", 3)];
                assert_eq!(
                    pairs.len(),
                    expected.len(),
                    "hash.Pairs has wrong length. got={}",
                    pairs.len()
                );
                for (k, v) in expected {
                    let mut found = false;
                    for (key_expr, value_expr) in pairs {
                        if let Expression::StringLiteral { value, .. } = key_expr {
                            if value == k {
                                if let Expression::IntegerLiteral { value: val, .. } = value_expr {
                                    assert_eq!(*val, v);
                                    found = true;
                                    break;
                                } else {
                                    panic!("value for key '{}' is not IntegerLiteral", k);
                                }
                            }
                        }
                    }
                    if !found {
                        panic!("key '{}' not found", k);
                    }
                }
            }
            _ => panic!("exp is not HashLiteral"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}

#[test]
fn test_parsing_hash_literals_boolean_keys() {
    let input = "{true: 1, false: 2}";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    let stmt = &program.statements[0];
    match stmt {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::HashLiteral { pairs, .. } => {
                let expected = vec![(true, 1), (false, 2)];
                assert_eq!(
                    pairs.len(),
                    expected.len(),
                    "hash.Pairs has wrong length. got={}",
                    pairs.len()
                );
                for (k, v) in expected {
                    let mut found = false;
                    for (key_expr, value_expr) in pairs {
                        if let Expression::Boolean { value, .. } = key_expr {
                            if *value == k {
                                if let Expression::IntegerLiteral { value: val, .. } = value_expr {
                                    assert_eq!(*val, v);
                                    found = true;
                                    break;
                                } else {
                                    panic!("value for key '{}' is not IntegerLiteral", k);
                                }
                            }
                        }
                    }
                    if !found {
                        panic!("key '{}' not found", k);
                    }
                }
            }
            _ => panic!("exp is not HashLiteral"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}

#[test]
fn test_parsing_hash_literals_integer_keys() {
    let input = "{1: 1, 2: 2, 3: 3}";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    let stmt = &program.statements[0];
    match stmt {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::HashLiteral { pairs, .. } => {
                let expected = vec![(1, 1), (2, 2), (3, 3)];
                assert_eq!(
                    pairs.len(),
                    expected.len(),
                    "hash.Pairs has wrong length. got={}",
                    pairs.len()
                );
                for (k, v) in expected {
                    let mut found = false;
                    for (key_expr, value_expr) in pairs {
                        if let Expression::IntegerLiteral { value, .. } = key_expr {
                            if *value == k {
                                if let Expression::IntegerLiteral { value: val, .. } = value_expr {
                                    assert_eq!(*val, v);
                                    found = true;
                                    break;
                                } else {
                                    panic!("value for key '{}' is not IntegerLiteral", k);
                                }
                            }
                        }
                    }
                    if !found {
                        panic!("key '{}' not found", k);
                    }
                }
            }
            _ => panic!("exp is not HashLiteral"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}

#[test]
fn test_parsing_hash_literals_with_expressions() {
    let input = "{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);
    let stmt = &program.statements[0];
    match stmt {
        Statement::ExpressionStatement { expression, .. } => match &**expression {
            Expression::HashLiteral { pairs, .. } => {
                assert_eq!(
                    pairs.len(),
                    3,
                    "hash.Pairs has wrong length. got={}",
                    pairs.len()
                );
                let tests: Vec<(&str, fn(&Expression))> = vec![
                    ("one", |e| match e {
                        Expression::Infix {
                            left,
                            operator,
                            right,
                            ..
                        } => {
                            match &**left {
                                Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 0),
                                _ => panic!("left of 'one' is not IntegerLiteral"),
                            }
                            assert_eq!(operator, "+");
                            match &**right {
                                Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 1),
                                _ => panic!("right of 'one' is not IntegerLiteral"),
                            }
                        }
                        _ => panic!("value for 'one' is not InfixExpression"),
                    }),
                    ("two", |e| match e {
                        Expression::Infix {
                            left,
                            operator,
                            right,
                            ..
                        } => {
                            match &**left {
                                Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 10),
                                _ => panic!("left of 'two' is not IntegerLiteral"),
                            }
                            assert_eq!(operator, "-");
                            match &**right {
                                Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 8),
                                _ => panic!("right of 'two' is not IntegerLiteral"),
                            }
                        }
                        _ => panic!("value for 'two' is not InfixExpression"),
                    }),
                    ("three", |e| match e {
                        Expression::Infix {
                            left,
                            operator,
                            right,
                            ..
                        } => {
                            match &**left {
                                Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 15),
                                _ => panic!("left of 'three' is not IntegerLiteral"),
                            }
                            assert_eq!(operator, "/");
                            match &**right {
                                Expression::IntegerLiteral { value, .. } => assert_eq!(*value, 5),
                                _ => panic!("right of 'three' is not IntegerLiteral"),
                            }
                        }
                        _ => panic!("value for 'three' is not InfixExpression"),
                    }),
                ];
                for (k, test_fn) in tests {
                    let mut found = false;
                    for (key_expr, value_expr) in pairs {
                        if let Expression::StringLiteral { value, .. } = key_expr {
                            if value == k {
                                test_fn(value_expr);
                                found = true;
                                break;
                            }
                        }
                    }
                    if !found {
                        panic!("key '{}' not found", k);
                    }
                }
            }
            _ => panic!("exp is not HashLiteral"),
        },
        _ => panic!("stmt is not ExpressionStatement"),
    }
}
fn check_parser_errors(p: &Parser) {
    if !p.errors.is_empty() {
        panic!("parser has errors: {:?}", p.errors);
    }
}
