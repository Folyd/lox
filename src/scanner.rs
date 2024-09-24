use std::{iter::Peekable, str::Chars};

#[allow(unused)]
#[derive(Debug)]
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

#[derive(Debug)]
pub struct Token<'a> {
    pub kind: TokenType,
    pub origin: &'a str,
    pub line: usize,
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
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            iter: source.chars().peekable(),
            start: 0,
            current: 0,
            line: 1,
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

    fn make_token(&self, kind: TokenType) -> Token {
        Token {
            kind,
            origin: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn _advance_digit(&mut self) {
        while matches!(self.peek(), Some(c) if c.is_digit(10)) {
            self.advance();
        }
    }

    fn scan_number(&mut self) -> Token {
        self._advance_digit();
        // Look for a fractional part.
        let mut next_two_chars = self.source[self.current..self.current + 2].chars();
        let (maybe_dot, maybe_digit) = (next_two_chars.next(), next_two_chars.next());
        if maybe_dot == Some('.') && matches!(maybe_digit, Some(c) if c.is_digit(10)) {
            // Consume the "."
            self.advance();

            self._advance_digit();
        }

        self.make_token(TokenType::Number)
    }

    fn check_keyword(
        &mut self,
        start: usize,
        length: usize,
        rest: &str,
        kind: TokenType,
    ) -> Token<'_> {
        if self.current - self.start == start + length
            && &self.source[self.start + start..self.current] == rest
        {
            return self.make_token(kind);
        }
        self.make_token(TokenType::Identifier)
    }

    fn scan_identifier(&mut self) -> Token<'_> {
        while matches!(self.peek(), Some(c) if c.is_alphanumeric()) {
            self.advance();
        }

        let mut chars = self.source[self.start..self.current].chars();
        if let Some(c) = chars.next() {
            match c {
                'a' => return self.check_keyword(1, 2, "nd", TokenType::And),
                'c' => return self.check_keyword(1, 4, "lass", TokenType::Class),
                'e' => return self.check_keyword(1, 3, "lse", TokenType::Else),
                'f' => {
                    if let Some(c) = chars.next() {
                        match c {
                            'a' => return self.check_keyword(2, 3, "lse", TokenType::False),
                            'o' => return self.check_keyword(2, 1, "r", TokenType::For),
                            'u' => return self.check_keyword(2, 1, "n", TokenType::Fun),
                            _ => {}
                        }
                    }
                }
                'i' => return self.check_keyword(1, 1, "f", TokenType::If),
                'n' => return self.check_keyword(1, 2, "il", TokenType::Nil),
                'o' => return self.check_keyword(1, 1, "r", TokenType::Or),
                'p' => return self.check_keyword(1, 4, "rint", TokenType::Print),
                'r' => return self.check_keyword(1, 5, "eturn", TokenType::Return),
                's' => return self.check_keyword(1, 4, "uper", TokenType::Super),
                't' => {
                    if let Some(c) = chars.next() {
                        match c {
                            'h' => return self.check_keyword(2, 2, "is", TokenType::This),
                            'r' => return self.check_keyword(2, 2, "ue", TokenType::True),
                            _ => {}
                        }
                    }
                }
                'v' => return self.check_keyword(1, 2, "ar", TokenType::Var),
                'w' => return self.check_keyword(1, 4, "hile", TokenType::While),
                _ => {}
            }
        }
        self.make_token(TokenType::Identifier)
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_white_spaces();

        self.start = self.current;
        if let Some(c) = self.advance() {
            if c.is_digit(10) {
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
                    return if self.source[self.current - 1..=self.current].starts_with("!=") {
                        self.advance();
                        self.make_token(TokenType::BangEqual)
                    } else {
                        self.make_token(TokenType::Bang)
                    };
                }
                '=' => {
                    return if self.source[self.current - 1..=self.current].starts_with("==") {
                        self.advance();
                        self.make_token(TokenType::EqualEqual)
                    } else {
                        self.make_token(TokenType::Equal)
                    };
                }
                '<' => {
                    return if self.source[self.current - 1..=self.current].starts_with("<=") {
                        self.advance();
                        self.make_token(TokenType::LessEqual)
                    } else {
                        self.make_token(TokenType::Less)
                    };
                }
                '>' => {
                    return if self.source[self.current - 1..=self.current].starts_with(">=") {
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
