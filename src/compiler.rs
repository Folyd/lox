use crate::scanner::{Scanner, TokenType};

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source);
    let mut line = 0;
    loop {
        let token = scanner.scan_token();
        if token.line != line {
            print!("{:4} ", token.line);
            line = token.line;
        } else {
            print!("   | ")
        }

        println!("{:?} {}", token.kind, token.origin);

        if matches!(token.kind, TokenType::Eof | TokenType::Error) {
            break;
        }
    }
}
