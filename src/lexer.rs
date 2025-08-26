use crate::token::{Token, TokenType, lookup_ident};

pub struct Lexer {
    input: String, // Lexer owns the input. Parser owns the Lexer. I was worried about lifetimes and ownership at first, but the hierarchy works immediately.
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok = match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    let ch = self.ch.unwrap();
                    self.read_char();
                    let literal = format!("{}{}", ch, self.ch.unwrap());
                    Token {
                        token_type: TokenType::Eq,
                        literal,
                    }
                } else {
                    Token {
                        token_type: TokenType::Assign,
                        literal: "=".to_string(),
                    }
                }
            }
            Some('+') => Token {
                token_type: TokenType::Plus,
                literal: "+".to_string(),
            },
            Some('-') => Token {
                token_type: TokenType::Minus,
                literal: "-".to_string(),
            },
            Some('!') => {
                if self.peek_char() == Some('=') {
                    let ch = self.ch.unwrap();
                    self.read_char();
                    let literal = format!("{}{}", ch, self.ch.unwrap());
                    Token {
                        token_type: TokenType::NotEq,
                        literal,
                    }
                } else {
                    Token {
                        token_type: TokenType::Bang,
                        literal: "!".to_string(),
                    }
                }
            }
            Some('/') => Token {
                token_type: TokenType::Slash,
                literal: "/".to_string(),
            },
            Some('*') => Token {
                token_type: TokenType::Asterisk,
                literal: "*".to_string(),
            },
            Some('<') => Token {
                token_type: TokenType::Lt,
                literal: "<".to_string(),
            },
            Some('>') => Token {
                token_type: TokenType::Gt,
                literal: ">".to_string(),
            },
            Some(';') => Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            Some(':') => Token {
                token_type: TokenType::Colon,
                literal: ":".to_string(),
            },
            Some(',') => Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            Some('{') => Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            Some('}') => Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            Some('(') => Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            Some(')') => Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            Some('[') => Token {
                token_type: TokenType::LBracket,
                literal: "[".to_string(),
            },
            Some(']') => Token {
                token_type: TokenType::RBracket,
                literal: "]".to_string(),
            },
            Some('"') => {
                let literal = self.read_string();
                Token {
                    token_type: TokenType::String,
                    literal,
                }
            }
            None => Token {
                token_type: TokenType::Eof,
                literal: String::new(),
            },
            Some(ch) => {
                if is_letter(ch) {
                    let literal = self.read_identifier();
                    let token_type = lookup_ident(&literal);
                    return Token {
                        token_type,
                        literal,
                    };
                } else if is_digit(ch) {
                    let literal = self.read_number();
                    return Token {
                        token_type: TokenType::Int,
                        literal,
                    };
                } else {
                    Token {
                        token_type: TokenType::Illegal,
                        literal: ch.to_string(),
                    }
                }
            }
        };
        self.read_char();
        tok
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.ch, Some(' ' | '\t' | '\n' | '\r')) {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input[self.read_position..].chars().next();
        }
        self.position = self.read_position;
        if let Some(ch) = self.ch {
            self.read_position += ch.len_utf8();
        } else {
            self.read_position += 1;
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            self.input[self.read_position..].chars().next()
        }
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while let Some(ch) = self.ch {
            if is_letter(ch) {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[start..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let start = self.position;
        while let Some(ch) = self.ch {
            if is_digit(ch) {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[start..self.position].to_string()
    }

    fn read_string(&mut self) -> String {
        let start = self.position + 1;
        loop {
            self.read_char();
            match self.ch {
                Some('"') | None => break,
                _ => {}
            }
        }
        self.input[start..self.position].to_string()
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}
