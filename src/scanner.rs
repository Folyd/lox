use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
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

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    For,
    Fun,
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

    Error,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub origin: &'a str,
    pub line: usize,
}

impl Default for Token<'_> {
    fn default() -> Self {
        Self {
            kind: TokenType::Eof,
            origin: "",
            line: 1,
        }
    }
}

impl<'a> Token<'a> {
    fn new(kind: TokenType, origin: &'a str, line: usize) -> Self {
        Token { kind, origin, line }
    }
}

pub struct Scanner<'a> {
    pub source: &'a str,
    iter: Peekable<Chars<'a>>,
    pub start: usize,
    pub current: usize,
    pub line: usize,
    is_eof: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            iter: source.chars().peekable(),
            start: 0,
            current: 0,
            line: 1,
            is_eof: false,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        // self.source[self.current - 1]
        self.iter.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    fn skip_white_spaces(&mut self) {
        while let Some(c) = self.peek() {
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                    // break;
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                    // break;
                }
                '/' => {
                    if &self.source[self.current..=self.current + 1] == "//" {
                        while matches!(self.peek(), Some(c) if *c != '\n') {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn make_token(&self, kind: TokenType) -> Token<'a> {
        Token {
            kind,
            origin: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn _advance_digit(&mut self) {
        while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            self.advance();
        }
    }

    fn scan_number(&mut self) -> Token<'a> {
        // TODO: fix this: 0p =>  19 Number 0
        //                         | Identifier p
        self._advance_digit();
        // Look for a fractional part.
        if self.source[self.current..].len() >= 2 {
            let mut next_two_chars = self.source[self.current..self.current + 2].chars();
            let (maybe_dot, maybe_digit) = (next_two_chars.next(), next_two_chars.next());
            if maybe_dot == Some('.') && matches!(maybe_digit, Some(c) if c.is_ascii_digit()) {
                // Consume the "."
                self.advance();

                self._advance_digit();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn scan_identifier(&mut self) -> Token<'a> {
        while matches!(self.peek(), Some(c) if c.is_alphanumeric() || *c == '_') {
            self.advance();
        }

        let kind = match &self.source[self.start..self.current] {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };

        self.make_token(kind)
    }

    fn scan_token(&mut self) -> Token<'a> {
        self.skip_white_spaces();

        self.start = self.current;
        if let Some(c) = self.advance() {
            if c.is_ascii_digit() {
                return self.scan_number();
            }

            if c.is_alphabetic() || c == '_' {
                return self.scan_identifier();
            }

            match c {
                '(' => return self.make_token(TokenType::LeftParen),
                ')' => return self.make_token(TokenType::RightParen),
                '{' => return self.make_token(TokenType::LeftBrace),
                '}' => return self.make_token(TokenType::RightBrace),
                ';' => return self.make_token(TokenType::Semicolon),
                ',' => return self.make_token(TokenType::Comma),
                '.' => return self.make_token(TokenType::Dot),
                '-' => return self.make_token(TokenType::Minus),
                '+' => return self.make_token(TokenType::Plus),
                '/' => return self.make_token(TokenType::Slash),
                '*' => return self.make_token(TokenType::Star),
                '!' => {
                    return if &self.source[self.current - 1..=self.current] == "!=" {
                        self.advance();
                        self.make_token(TokenType::BangEqual)
                    } else {
                        self.make_token(TokenType::Bang)
                    };
                }
                '=' => {
                    return if &self.source[self.current - 1..=self.current] == "==" {
                        self.advance();
                        self.make_token(TokenType::EqualEqual)
                    } else {
                        self.make_token(TokenType::Equal)
                    };
                }
                '<' => {
                    return if &self.source[self.current - 1..=self.current] == "<=" {
                        self.advance();
                        self.make_token(TokenType::LessEqual)
                    } else {
                        self.make_token(TokenType::Less)
                    };
                }
                '>' => {
                    return if &self.source[self.current - 1..=self.current] == ">=" {
                        self.advance();
                        self.make_token(TokenType::GreaterEqual)
                    } else {
                        self.make_token(TokenType::Greater)
                    };
                }
                '"' => {
                    while matches!(self.peek(), Some(c) if *c !='"') {
                        if c == '\n' {
                            self.line += 1;
                        }
                        self.advance();
                    }

                    if self.peek().is_none() {
                        return Token::new(TokenType::Error, "Unterminated string.", self.line);
                    }

                    self.advance();
                    return self.make_token(TokenType::String);
                }
                _ => {}
            }
        }

        Token::new(TokenType::Eof, "", self.line)
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_eof {
            None
        } else {
            let token = self.scan_token();
            self.is_eof = token.kind == TokenType::Eof;
            Some(token)
        }
    }
}
