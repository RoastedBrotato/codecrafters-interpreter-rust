use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum TokenType {
    // Single-character tokens
    LeftParen, RightParen, Plus,
    // Literals
    Number(f64),
    // Keywords
    True, False, Nil,
    Eof
}

#[derive(Debug, Clone)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<String>,
    line: usize,
}

#[derive(Debug)]
enum Expr {
    Literal(LiteralValue),
}

#[derive(Debug)]
enum LiteralValue {
    Number(f64),
    True,
    False,
    Nil,
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn parse(&mut self) -> Option<Expr> {
        self.expression()
    }

    fn expression(&mut self) -> Option<Expr> {
        self.literal()
    }

    fn literal(&mut self) -> Option<Expr> {
        if let Some(token) = self.peek() {
            match token.token_type {
                TokenType::True => {
                    self.advance();
                    Some(Expr::Literal(LiteralValue::True))
                }
                TokenType::False => {
                    self.advance();
                    Some(Expr::Literal(LiteralValue::False))
                }
                TokenType::Nil => {
                    self.advance();
                    Some(Expr::Literal(LiteralValue::Nil))
                }
                TokenType::Number(n) => {
                    self.advance();
                    Some(Expr::Literal(LiteralValue::Number(n)))
                }
                _ => None
            }
        } else {
            None
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) -> Option<Token> {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> Option<Token> {
        self.tokens.get(self.current - 1).cloned()
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek()?.token_type, TokenType::Eof)
    }
}

fn scan_tokens(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = source.chars().peekable();
    let mut line = 1;

    while let Some(c) = chars.next() {
        match c {
            'f' if source[chars.pos()..].starts_with("alse") => {
                tokens.push(Token {
                    token_type: TokenType::False,
                    lexeme: "false".to_string(),
                    literal: None,
                    line,
                });
            },
            't' if source[chars.pos()..].starts_with("rue") => {
                tokens.push(Token {
                    token_type: TokenType::True,
                    lexeme: "true".to_string(),
                    literal: None,
                    line,
                });
            },
            'n' if source[chars.pos()..].starts_with("il") => {
                tokens.push(Token {
                    token_type: TokenType::Nil,
                    lexeme: "nil".to_string(),
                    literal: None,
                    line,
                });
            },
            ' ' | '\r' | '\t' => {},
            '\n' => line += 1,
            _ => {}
        }
    }

    tokens.push(Token {
        token_type: TokenType::Eof,
        lexeme: String::new(),
        literal: None,
        line,
    });

    tokens
}

fn main() {
    let keywords: HashMap<&str, &str> = [
        ("and", "AND"),
        ("class", "CLASS"),
        ("else", "ELSE"),
        ("false", "FALSE"),
        ("for", "FOR"),
        ("fun", "FUN"),
        ("if", "IF"),
        ("nil", "NIL"),
        ("or", "OR"),
        ("print", "PRINT"),
        ("return", "RETURN"),
        ("super", "SUPER"),
        ("this", "THIS"),
        ("true", "TRUE"),
        ("var", "VAR"),
        ("while", "WHILE"),
    ].iter().cloned().collect();

    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} <command> <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
        String::new()
    });

    match command.as_str() {
        "parse" => {
            let tokens = scan_tokens(&file_contents);
            let mut parser = Parser::new(tokens);
            if let Some(expr) = parser.parse() {
                print_ast(&expr);
            }
        }
        "tokenize" => {
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let mut has_error = false;
            let mut line_number = 1;

            if !file_contents.is_empty() {
                let mut file_contents_chars = file_contents.chars().peekable();
                while let Some(char) = file_contents_chars.next() {
                    match char {
                        '(' => println!("LEFT_PAREN ( null"),
                        ')' => println!("RIGHT_PAREN ) null"),
                        '{' => println!("LEFT_BRACE {{ null"),
                        '}' => println!("RIGHT_BRACE }} null"),
                        ',' => println!("COMMA , null"),
                        '.' => println!("DOT . null"),
                        '+' => println!("PLUS + null"),
                        '-' => println!("MINUS - null"),
                        ';' => println!("SEMICOLON ; null"),
                        '*' => println!("STAR * null"),
                        '!' => {
                            if file_contents_chars.peek() == Some(&'=') {
                                file_contents_chars.next();
                                println!("BANG_EQUAL != null");
                            } else {
                                println!("BANG ! null");
                            }
                        }
                        '=' => {
                            if file_contents_chars.peek() == Some(&'=') {
                                file_contents_chars.next();
                                println!("EQUAL_EQUAL == null");
                            } else {
                                println!("EQUAL = null");
                            }
                        }
                        '<' => {
                            if file_contents_chars.peek() == Some(&'=') {
                                file_contents_chars.next();
                                println!("LESS_EQUAL <= null");
                            } else {
                                println!("LESS < null");
                            }
                        }
                        '>' => {
                            if file_contents_chars.peek() == Some(&'=') {
                                file_contents_chars.next();
                                println!("GREATER_EQUAL >= null");
                            } else {
                                println!("GREATER > null");
                            }
                        }
                        '/' => {
                            if file_contents_chars.peek() == Some(&'/') {
                                // Consume the second '/' and skip the rest of the line
                                while let Some(next_char) = file_contents_chars.next() {
                                    if next_char == '\n' {
                                        line_number += 1;
                                        break;
                                    }
                                }
                            } else {
                                println!("SLASH / null");
                            }
                        }
                        ' ' | '\r' | '\t' => {}, // Ignore whitespace
                        '\n' => {
                            line_number += 1;
                        }
                        '"' => {
                            let mut string_literal = String::new();
                            let mut unterminated = true;
                            while let Some(next_char) = file_contents_chars.next() {
                                if next_char == '"' {
                                    println!("STRING \"{}\" {}", string_literal, string_literal);
                                    unterminated = false;
                                    break;
                                } else if next_char == '\n' {
                                    line_number += 1;
                                    writeln!(io::stderr(), "[line {}] Error: Unterminated string.", line_number).unwrap();
                                    has_error = true;
                                    unterminated = false;
                                    break;
                                } else {
                                    string_literal.push(next_char);
                                }
                            }
                            if unterminated {
                                writeln!(io::stderr(), "[line {}] Error: Unterminated string.", line_number).unwrap();
                                has_error = true;
                            }
                        }
                        '0'..='9' => {
                            // Scan number and parse
                            let mut number_literal = char.to_string();
                            while let Some(&next_char) = file_contents_chars.peek() {
                                if next_char.is_digit(10) {
                                    number_literal.push(next_char);
                                    file_contents_chars.next();
                                } else {
                                    break;
                                }
                            }

                            if let Some(&'.') = file_contents_chars.peek() {
                                if let Some(next) = file_contents_chars.clone().nth(1) {
                                    if next.is_digit(10) {
                                        number_literal.push('.');
                                        file_contents_chars.next();
                                        while let Some(&next_char) = file_contents_chars.peek() {
                                            if next_char.is_digit(10) {
                                                number_literal.push(next_char);
                                                file_contents_chars.next();
                                            } else {
                                                break;
                                            }
                                        }
                                    }
                                }
                            }

                            // Parse and format output
                            let number_value: f64 = number_literal.parse().unwrap();
                            let formatted_value = if number_literal.contains('.') {
                                // Check if decimal ends with zeros
                                let trimmed = number_literal.trim_end_matches('0');
                                if trimmed.ends_with('.') {
                                    // It was a whole number like 200.00
                                    format!("{}.0", number_value)
                                } else {
                                    // Regular decimal
                                    number_value.to_string()
                                }
                            } else {
                                // Integer
                                format!("{}.0", number_value)
                            };
                            println!("NUMBER {} {}", number_literal, formatted_value);
                        }
                        'a'..='z' | 'A'..='Z' | '_' => {
                            let mut identifier = char.to_string();
                            while let Some(&next_char) = file_contents_chars.peek() {
                                if next_char.is_alphanumeric() || next_char == '_' {
                                    identifier.push(next_char);
                                    file_contents_chars.next();
                                } else {
                                    break;
                                }
                            }
                            if let Some(keyword_type) = keywords.get(identifier.as_str()) {
                                println!("{} {} null", keyword_type, identifier);
                            } else {
                                println!("IDENTIFIER {} null", identifier);
                            }
                        }
                        _ => {
                            writeln!(io::stderr(), "[line {}] Error: Unexpected character: {}", line_number, char).unwrap();
                            has_error = true;
                        }
                    }
                }
                println!("EOF  null"); 
            } else {
                println!("EOF  null");
            }

            if has_error {
                process::exit(65);
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}

fn print_ast(expr: &Expr) {
    match expr {
        Expr::Literal(value) => match value {
            LiteralValue::True => print!("true"),
            LiteralValue::False => print!("false"),
            LiteralValue::Nil => print!("nil"),
            LiteralValue::Number(n) => print!("{}", n),
        }
    }
    println!();
}
