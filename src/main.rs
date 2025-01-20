use std::env;
use std::fmt::Display;
use std::fs;
use std::iter::Peekable;
use std::str::Chars;
const LEFT_PAREN: char = '(';
const RIGHT_PAREN: char = ')';
const LEFT_BRACE: char = '{';
const RIGHT_BRACE: char = '}';
const COMMA: char = ',';
const DOT: char = '.';
const MINUS: char = '-';
const PLUS: char = '+';
const SEMICOLON: char = ';';
const SLASH: char = '/';
const STAR: char = '*';
const BANG: char = '!';
const BANG_EQUAL: &str = "!=";
const EQUAL: char = '=';
const EQUAL_EQUAL: &str = "==";
const GREATER: char = '>';
const GREATER_EQUAL: &str = ">=";
const LESS: char = '<';
const LESS_EQUAL: &str = "<=";
const AND: &str = "and";
const CLASS: &str = "class";
const ELSE: &str = "else";
const FALSE: &str = "false";
const FUN: &str = "fun";
const FOR: &str = "for";
const IF: &str = "if";
const NIL: &str = "nil";
const OR: &str = "or";
const PRINT: &str = "print";
const RETURN: &str = "return";
const SUPER: &str = "super";
const THIS: &str = "this";
const TRUE: &str = "true";
const VAR: &str = "var";
const WHILE: &str = "while";
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }
    let command = &args[1];
    let filename = &args[2];
    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let scanner = Scanner::new(file_contents.as_str());
            let (tokens, had_error) = scanner.scan_tokens();
            tokens.iter().for_each(|token| println!("{}", token));
            if had_error {
                std::process::exit(65);
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let scanner = Scanner::new(file_contents.as_str());
            let (tokens, _) = scanner.scan_tokens();
            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(expr) => println!("{}", expr),
                Err(error) => {
                    eprintln!(
                        "[line {}] Error at '{}': {}",
                        error.line,
                        error.token.lexeme(),
                        error.message
                    );
                    std::process::exit(65);
                }
            }
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let scanner = Scanner::new(file_contents.as_str());
            let (tokens, _) = scanner.scan_tokens();
            let mut parser = Parser::new(tokens);
            match parser.parse_expression() {
                Ok(expr) => {
                    let interpreter = Interpreter;
                    match interpreter.evaluate(&expr) {
                        Ok(value) => println!("{}", value),
                        Err(error) => {
                            eprintln!("Runtime error: {}", error);
                            std::process::exit(70);
                        }
                    }
                }
                Err(error) => {
                    eprintln!(
                        "[line {}] Error at '{}': {}",
                        error.line,
                        error.token.lexeme(),
                        error.message
                    );
                    std::process::exit(65);
                }
            }
        }
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let scanner = Scanner::new(file_contents.as_str());
            let (tokens, _) = scanner.scan_tokens();
            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(statements) => {
                    let mut interpreter = Interpreter;
                    if let Err(error) = interpreter.interpret(statements) {
                        eprintln!("Runtime error: {}", error);
                        std::process::exit(70);
                    }
                }
                Err(error) => {
                    eprintln!(
                        "[line {}] Error at '{}': {}",
                        error.line,
                        error.token.lexeme(),
                        error.message
                    );
                    std::process::exit(65);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
#[derive(Debug, Eq, PartialEq, Clone)]
enum Token {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals.
    Identifier(String),
    String(String),
    Number(String),
    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}
impl Token {
    fn token_type(&self) -> String {
        match self {
            Token::LeftParen => "LEFT_PAREN".to_string(),
            Token::RightParen => "RIGHT_PAREN".to_string(),
            Token::LeftBrace => "LEFT_BRACE".to_string(),
            Token::RightBrace => "RIGHT_BRACE".to_string(),
            Token::Comma => "COMMA".to_string(),
            Token::Dot => "DOT".to_string(),
            Token::Minus => "MINUS".to_string(),
            Token::Plus => "PLUS".to_string(),
            Token::Semicolon => "SEMICOLON".to_string(),
            Token::Slash => "SLASH".to_string(),
            Token::Star => "STAR".to_string(),
            Token::Bang => "BANG".to_string(),
            Token::BangEqual => "BANG_EQUAL".to_string(),
            Token::Equal => "EQUAL".to_string(),
            Token::EqualEqual => "EQUAL_EQUAL".to_string(),
            Token::Greater => "GREATER".to_string(),
            Token::GreaterEqual => "GREATER_EQUAL".to_string(),
            Token::Less => "LESS".to_string(),
            Token::LessEqual => "LESS_EQUAL".to_string(),
            Token::Identifier(_) => "IDENTIFIER".to_string(),
            Token::String(_) => "STRING".to_string(),
            Token::Number(_) => "NUMBER".to_string(),
            Token::And => "AND".to_string(),
            Token::Class => "CLASS".to_string(),
            Token::Else => "ELSE".to_string(),
            Token::False => "FALSE".to_string(),
            Token::Fun => "FUN".to_string(),
            Token::For => "FOR".to_string(),
            Token::If => "IF".to_string(),
            Token::Nil => "NIL".to_string(),
            Token::Or => "OR".to_string(),
            Token::Print => "PRINT".to_string(),
            Token::Return => "RETURN".to_string(),
            Token::Super => "SUPER".to_string(),
            Token::This => "THIS".to_string(),
            Token::True => "TRUE".to_string(),
            Token::Var => "VAR".to_string(),
            Token::While => "WHILE".to_string(),
            Token::Eof => "EOF".to_string(),
        }
    }
    fn lexeme(&self) -> String {
        match self {
            Token::LeftParen => LEFT_PAREN.to_string(),
            Token::RightParen => RIGHT_PAREN.to_string(),
            Token::LeftBrace => LEFT_BRACE.to_string(),
            Token::RightBrace => RIGHT_BRACE.to_string(),
            Token::Comma => COMMA.to_string(),
            Token::Dot => DOT.to_string(),
            Token::Minus => MINUS.to_string(),
            Token::Plus => PLUS.to_string(),
            Token::Semicolon => SEMICOLON.to_string(),
            Token::Slash => SLASH.to_string(),
            Token::Star => STAR.to_string(),
            Token::Bang => BANG.to_string(),
            Token::BangEqual => BANG_EQUAL.to_string(),
            Token::Equal => EQUAL.to_string(),
            Token::EqualEqual => EQUAL_EQUAL.to_string(),
            Token::Greater => GREATER.to_string(),
            Token::GreaterEqual => GREATER_EQUAL.to_string(),
            Token::Less => LESS.to_string(),
            Token::LessEqual => LESS_EQUAL.to_string(),
            Token::Identifier(identifier) => identifier.to_string(),
            Token::String(string) => format!("\"{}\"", string),
            Token::Number(number) => number.to_string(),
            Token::And => AND.to_string(),
            Token::Class => CLASS.to_string(),
            Token::Else => ELSE.to_string(),
            Token::False => FALSE.to_string(),
            Token::Fun => FUN.to_string(),
            Token::For => FOR.to_string(),
            Token::If => IF.to_string(),
            Token::Nil => NIL.to_string(),
            Token::Or => OR.to_string(),
            Token::Print => PRINT.to_string(),
            Token::Return => RETURN.to_string(),
            Token::Super => SUPER.to_string(),
            Token::This => THIS.to_string(),
            Token::True => TRUE.to_string(),
            Token::Var => VAR.to_string(),
            Token::While => WHILE.to_string(),
            Token::Eof => "".to_string(),
        }
    }
    fn literal(&self) -> String {
        match self {
            Token::String(string) => string.to_string(),
            Token::Number(number) => format!("{:?}", number.parse::<f64>().unwrap()),
            _ => "null".to_string(),
        }
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.token_type(),
            self.lexeme(),
            self.literal()
        )
    }
}
struct Scanner<'a> {
    line: usize,
    current: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
    had_error: bool,
}
impl<'a> Scanner<'a> {
    fn new(source: &'a str) -> Self {
        Scanner {
            line: 1,
            current: source.chars().peekable(),
            tokens: Vec::new(),
            had_error: false,
        }
    }
    fn scan_tokens(mut self) -> (Vec<Token>, bool) {
        self.scan_token();
        self.tokens.push(Token::Eof);
        (self.tokens, self.had_error)
    }
    fn scan_token(&mut self) {
        while let Some(c) = self.advance() {
            let token: Option<Token> = match c {
                LEFT_PAREN => Some(Token::LeftParen),
                RIGHT_PAREN => Some(Token::RightParen),
                LEFT_BRACE => Some(Token::LeftBrace),
                RIGHT_BRACE => Some(Token::RightBrace),
                STAR => Some(Token::Star),
                DOT => Some(Token::Dot),
                COMMA => Some(Token::Comma),
                PLUS => Some(Token::Plus),
                MINUS => Some(Token::Minus),
                SEMICOLON => Some(Token::Semicolon),
                EQUAL => {
                    if self.next_match(EQUAL) {
                        Some(Token::EqualEqual)
                    } else {
                        Some(Token::Equal)
                    }
                }
                BANG => {
                    if self.next_match(EQUAL) {
                        Some(Token::BangEqual)
                    } else {
                        Some(Token::Bang)
                    }
                }
                LESS => {
                    if self.next_match(EQUAL) {
                        Some(Token::LessEqual)
                    } else {
                        Some(Token::Less)
                    }
                }
                GREATER => {
                    if self.next_match(EQUAL) {
                        Some(Token::GreaterEqual)
                    } else {
                        Some(Token::Greater)
                    }
                }
                SLASH => {
                    if self.next_match(SLASH) {
                        while let Some(c) = self.current.peek() {
                            if *c != '\n' {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        None
                    } else {
                        Some(Token::Slash)
                    }
                }
                '"' => {
                    let mut value = String::new();
                    loop {
                        match self.advance() {
                            Some('"') => break Some(Token::String(value)),
                            Some(c) => {
                                if c == '\n' {
                                    self.line += 1;
                                }
                                value.push(c);
                            }
                            None => {
                                self.report_error("Unterminated string.");
                                break None;
                            }
                        }
                    }
                }
                _ if c.is_numeric() => {
                    let mut value = String::new();
                    value.push(c);
                    while let Some(c) = self.current.peek() {
                        if c.is_numeric() {
                            value.push(*c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let mut advanced_iterator = self.current.clone();
                    advanced_iterator.next();
                    if let Some(&DOT) = self.current.peek() {
                        if advanced_iterator.peek().is_some_and(|c| c.is_numeric()) {
                            value.push(DOT);
                            self.advance();
                            while let Some(c) = self.current.peek() {
                                if c.is_numeric() {
                                    value.push(*c);
                                    self.advance();
                                } else {
                                    break;
                                }
                            }
                        }
                    }
                    Some(Token::Number(value))
                }
                _ if c.is_alphabetic() || c == '_' => {
                    let mut value = String::new();
                    value.push(c);
                    while let Some(c) = self.current.peek() {
                        if c.is_alphanumeric() || *c == '_' {
                            value.push(*c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    Some(match value.as_str() {
                        AND => Token::And,
                        CLASS => Token::Class,
                        ELSE => Token::Else,
                        FALSE => Token::False,
                        FOR => Token::For,
                        FUN => Token::Fun,
                        IF => Token::If,
                        NIL => Token::Nil,
                        OR => Token::Or,
                        PRINT => Token::Print,
                        RETURN => Token::Return,
                        SUPER => Token::Super,
                        THIS => Token::This,
                        TRUE => Token::True,
                        VAR => Token::Var,
                        WHILE => Token::While,
                        _ => Token::Identifier(value),
                    })
                }
                '\n' => {
                    self.line += 1;
                    None
                }
                ' ' | '\t' => None,
                _ => {
                    self.report_error(format!("Unexpected character: {c}").as_str());
                    None
                }
            };
            if let Some(token) = token {
                self.tokens.push(token);
            }
        }
    }
    fn report_error(&mut self, error: &str) {
        self.had_error = true;
        eprintln!("[line {}] Error: {error}", self.line);
    }
    fn advance(&mut self) -> Option<char> {
        self.current.next()
    }
    fn next_match(&mut self, expected: char) -> bool {
        if self.current.peek() != Some(&expected) {
            return false;
        }
        self.current.next();
        true
    }
}
#[derive(Debug)]
enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Token),
    Unary(Token, Box<Expr>),
}
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(left, operator, right) => {
                write!(f, "({} {} {})", operator.lexeme(), left, right)
            }
            Expr::Grouping(expr) => write!(f, "(group {expr})"),
            Expr::Literal(t @ (Token::Number(_) | Token::String(_))) => {
                write!(f, "{}", t.literal())
            }
            Expr::Literal(value) => write!(f, "{}", value.lexeme()),
            Expr::Unary(operator, expr) => write!(f, "({} {expr})", operator.lexeme()),
        }
    }
}
struct Parser {
    tokens: Vec<Token>,
    current: usize,
}
struct ParseError {
    line: usize,
    token: Token,
    message: String,
}
impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }
    fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.statement()?);
        }
        Ok(statements)
    }

    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }
    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.addition()
    }
    fn addition(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.multiplication()?;

        while matches!(self.peek(), Token::Plus | Token::Minus) {
            let operator = self.advance();
            let right = self.multiplication()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }
    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while matches!(self.peek(), Token::Star | Token::Slash) {
            let operator = self.advance();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }
    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        while matches!(
            self.peek(),
            Token::Greater
                | Token::GreaterEqual
                | Token::Less
                | Token::LessEqual
                | Token::EqualEqual
                | Token::BangEqual
        ) {
            let operator = self.advance();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expr, ParseError> {
        if matches!(self.peek(), Token::Bang | Token::Minus) {
            let operator = self.advance();
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        self.primary()
    }
    fn primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek() {
            Token::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                match self.peek() {
                    Token::RightParen => {
                        self.advance();
                        Ok(Expr::Grouping(Box::new(expr)))
                    }
                    token => Err(self.error(token.clone(), "Expect ')' after expression.")),
                }
            }
            Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Nil => {
                Ok(Expr::Literal(self.advance()))
            }
            token => Err(self.error(token.clone(), "Expect expression.")),
        }
    }
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    fn is_at_end(&self) -> bool {
        self.peek() == &Token::Eof
    }
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }
    fn error(&self, token: Token, message: &str) -> ParseError {
        ParseError {
            line: 1, // TODO: Track actual line numbers
            token,
            message: message.to_string(),
        }
    }
    fn synchronize(&mut self) {
        while !self.is_at_end() {
            if matches!(
                self.peek(),
                Token::Class
                    | Token::Fun
                    | Token::Var
                    | Token::For
                    | Token::If
                    | Token::While
                    | Token::Print
                    | Token::Return
            ) {
                break;
            }
            self.advance();
        }
    }
    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if matches!(self.peek(), Token::Print) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // consume 'print'
        let value = self.expression()?;
        self.consume(Token::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(Token::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }
    fn consume(&mut self, expected: Token, message: &str) -> Result<Token, ParseError> {
        if self.peek() == &expected {
            Ok(self.advance())
        } else {
            Err(self.error(self.peek().clone(), message))
        }
    }
}
#[derive(Debug)]
enum Stmt {
    Print(Expr),
    Expression(Expr),
}
#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{}", n.trunc())
                } else {
                    write!(f, "{}", n)
                }
            }
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

struct Interpreter;

impl Interpreter {
    fn evaluate(&self, expr: &Expr) -> Result<Value, String> {
        match expr {
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;

                match (left, right) {
                    (Value::Number(l), Value::Number(r)) => match operator {
                        Token::EqualEqual => Ok(Value::Boolean(l == r)),
                        Token::BangEqual => Ok(Value::Boolean(l != r)),
                        Token::Greater => Ok(Value::Boolean(l > r)),
                        Token::GreaterEqual => Ok(Value::Boolean(l >= r)),
                        Token::Less => Ok(Value::Boolean(l < r)),
                        Token::LessEqual => Ok(Value::Boolean(l <= r)),
                        Token::Plus => Ok(Value::Number(l + r)),
                        Token::Minus => Ok(Value::Number(l - r)),
                        Token::Star => Ok(Value::Number(l * r)),
                        Token::Slash => {
                            if r == 0.0 {
                                Err("Division by zero.".to_string())
                            } else {
                                Ok(Value::Number(l / r))
                            }
                        }
                        _ => Err("Invalid binary operator.".to_string()),
                    },
                    (Value::String(l), Value::String(r)) => match operator {
                        Token::EqualEqual => Ok(Value::Boolean(l == r)),
                        Token::BangEqual => Ok(Value::Boolean(l != r)),
                        Token::Plus => Ok(Value::String(l + &r)),
                        _ => Err("Invalid operator for strings.".to_string()),
                    },
                    (Value::Boolean(l), Value::Boolean(r)) => match operator {
                        Token::EqualEqual => Ok(Value::Boolean(l == r)),
                        Token::BangEqual => Ok(Value::Boolean(l != r)),
                        _ => Err("Invalid operator for booleans.".to_string()),
                    },
                    (Value::Nil, Value::Nil) => match operator {
                        Token::EqualEqual => Ok(Value::Boolean(true)),
                        Token::BangEqual => Ok(Value::Boolean(false)),
                        _ => Err("Invalid operator for nil.".to_string()),
                    },
                    _ => match operator {
                        Token::EqualEqual => Ok(Value::Boolean(false)),
                        Token::BangEqual => Ok(Value::Boolean(true)),
                        _ => Err("Invalid operand types.".to_string()),
                    },
                }
            }
            Expr::Unary(operator, expr) => {
                let right = self.evaluate(expr)?;
                match operator {
                    Token::Minus => {
                        if let Value::Number(n) = right {
                            Ok(Value::Number(-n))
                        } else {
                            Err("Operand must be a number.".to_string())
                        }
                    }
                    Token::Bang => Ok(Value::Boolean(!self.is_truthy(&right))),
                    _ => Err("Invalid unary operator.".to_string()),
                }
            }
            Expr::Grouping(expr) => self.evaluate(expr),
            Expr::Literal(token) => match token {
                Token::Number(n) => Ok(Value::Number(n.parse().unwrap())),
                Token::String(s) => Ok(Value::String(s.clone())),
                Token::True => Ok(Value::Boolean(true)),
                Token::False => Ok(Value::Boolean(false)),
                Token::Nil => Ok(Value::Nil),
                _ => Err("Invalid literal".to_string()),
            },
        }
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true,
        }
    }
    fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), String> {
        for stmt in statements {
            match stmt {
                Stmt::Print(expr) => {
                    let value = self.evaluate(&expr)?;
                    println!("{}", value);
                }
                Stmt::Expression(expr) => {
                    self.evaluate(&expr)?;
                }
            }
        }
        Ok(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Print(expr) => {
                let value = self.evaluate(&expr)?;
                println!("{}", value);
                Ok(())
            }
            Stmt::Expression(expr) => {
                self.evaluate(&expr)?;
                Ok(())
            }
        }
    }
}
