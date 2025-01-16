use std::env;
use std::fs;
use std::io::{self, Write};
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });

            let mut has_error = false;

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
                                        break;
                                    }
                                }
                            } else {
                                println!("SLASH / null");
                            }
                        }
                        ' ' | '\r' | '\t' => {}, // Ignore whitespace
                        _ => {
                            writeln!(io::stderr(), "[line 1] Error: Unexpected character: {}", char).unwrap();
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
