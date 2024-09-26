use std::{mem, ops::Add};

use crate::{
    scanner::{Scanner, Token, TokenType},
    Chunk, OpCode, Value,
};
use num_enum::{IntoPrimitive, TryFromPrimitive};

type ParseFn<'a> = fn(&mut Parser<'a>, bool /*can assign*/);

struct Parser<'a> {
    scanner: Scanner<'a>,
    current: Token<'a>,
    previous: Token<'a>,
    chunk: Chunk,
    had_error: bool,
    panic_mode: bool,
}

struct ParseRule<'a> {
    prefix: Option<ParseFn<'a>>,
    infix: Option<ParseFn<'a>>,
    precedence: Precedence,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Add<u8> for Precedence {
    type Output = Self;

    fn add(self, rhs: u8) -> Self::Output {
        Self::try_from(self as u8 + rhs).unwrap()
    }
}

pub struct Compiler<'a> {
    parser: Parser<'a>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Parser {
            scanner: Scanner::new(source),
            current: Token::default(),
            previous: Token::default(),
            chunk: Chunk::new(),
            had_error: false,
            panic_mode: false,
        }
    }

    fn make_constant(&mut self, value: Value) -> usize {
        let constant = self.chunk.add_constant(value);
        if constant > u8::MAX as usize {
            self.error("Too many constants in one chunk.");
            return 0;
        }
        constant
    }

    fn emit_byte<T: Into<u8>>(&mut self, byte: T) {
        self.chunk.write_byte(byte, self.previous.line);
    }

    fn emit_bytes<T1: Into<u8>, T2: Into<u8>>(&mut self, byte1: T1, byte2: T2) {
        self.chunk.write_byte(byte1, self.previous.line);
        self.chunk.write_byte(byte2, self.previous.line);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return);
    }

    fn emity_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant, constant as u8);
    }
}

impl<'a> Parser<'a> {
    fn declaration(&mut self) {
        if self._match(TokenType::Var) {
            self.var_decaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.kind != TokenType::Eof {
            if self.previous.kind == TokenType::Semicolon {
                return;
            }

            match self.current.kind {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => self.advance(),
            }
        }
    }

    fn var_decaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self._match(TokenType::Equal) {
            self.expression();
        } else {
            // desugars a variable declaration like: var a;
            // into: var a = nil;
            self.emit_byte(OpCode::Nil);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );
        self.define_variable(global);
    }

    fn parse_variable(&mut self, error_message: &str) -> usize {
        self.consume(TokenType::Identifier, error_message);
        self.identifier_constant(self.previous.origin)
    }

    fn identifier_constant(&mut self, identifier: &str) -> usize {
        self.make_constant(identifier.into())
    }

    fn define_variable(&mut self, global: usize) {
        self.emit_bytes(OpCode::DefineGlobal, global as u8);
    }

    fn statement(&mut self) {
        if self._match(TokenType::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn _match(&mut self, kind: TokenType) -> bool {
        if self.check(kind) {
            self.advance();
            return true;
        }
        false
    }

    fn check(&mut self, kind: TokenType) -> bool {
        self.current.kind == kind
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop);
    }
}

impl<'a> Parser<'a> {
    fn compile(&mut self) -> Chunk {
        self.advance();
        while !self._match(TokenType::Eof) {
            self.declaration();
        }
        self.emit_return();

        mem::take(&mut self.chunk)
    }

    fn advance(&mut self) {
        self.previous = mem::take(&mut self.current);
        while let Some(token) = self.scanner.next() {
            self.current = token;
            if self.previous.kind == TokenType::Bang
                && !matches!(
                    self.current.kind,
                    TokenType::True
                        | TokenType::False
                        | TokenType::Nil
                        | TokenType::Bang
                        | TokenType::LeftParen
                )
            {
                self.error("! operator can only be used on booleans and nil.");
                break;
            }

            if self.current.kind != TokenType::Error {
                break;
            }

            self.error_at_current(self.current.origin);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let rule = ParseRule::get_rule(self.previous.kind);

        let can_assign = precedence <= Precedence::Assignment;
        if let Some(prefix_fn) = rule.prefix {
            prefix_fn(self, can_assign);
        } else {
            self.error("Parse precedence: expect expression.");
            return;
        }

        while precedence <= ParseRule::get_rule(self.current.kind).precedence {
            self.advance();
            // Do not reuse the previous rule since it may have changed.
            let rule = ParseRule::get_rule(self.previous.kind);
            if let Some(infix_fn) = rule.infix {
                infix_fn(self, can_assign);
            }
        }

        if can_assign && self._match(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }
    }

    fn consume(&mut self, kind: TokenType, message: &str) {
        if self.current.kind == kind {
            self.advance();
            return;
        }
        self.error_at_current(message);
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.previous.kind {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::True => self.emit_byte(OpCode::True),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            _ => (),
        }
    }

    fn string(&mut self, _can_assign: bool) {
        let string = self.previous.origin.trim_matches('"');
        self.emity_constant(string.into());
    }

    fn number(&mut self, _can_assign: bool) {
        self.emity_constant(self.previous.origin.parse::<f64>().unwrap().into());
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self, _can_assign: bool) {
        let operator_kind = self.previous.kind;
        self.parse_precedence(Precedence::Unary);
        match operator_kind {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            TokenType::Bang => self.emit_byte(OpCode::Not),
            _ => (),
        }
    }

    fn binary(&mut self, _can_assign: bool) {
        let operator_kind = self.previous.kind;
        let rule = ParseRule::get_rule(operator_kind);
        self.parse_precedence(rule.precedence + 1);

        match operator_kind {
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Subtract),
            TokenType::Star => self.emit_byte(OpCode::Multiply),
            TokenType::Slash => self.emit_byte(OpCode::Divide),
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal, OpCode::Not),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal),
            TokenType::Greater => self.emit_byte(OpCode::Greater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less, OpCode::Not),
            TokenType::Less => self.emit_byte(OpCode::Less),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater, OpCode::Not),
            _ => (),
        }
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous.origin, can_assign);
    }

    fn named_variable(&mut self, name: &str, can_assign: bool) {
        let pos = self.identifier_constant(name);
        if can_assign && self._match(TokenType::Equal) {
            self.expression();
            self.emit_bytes(OpCode::SetGlobal, pos as u8);
        } else {
            self.emit_bytes(OpCode::GetGlobal, pos as u8);
        }
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(self.current.clone(), message);
    }

    fn error(&mut self, message: &str) {
        self.error_at(self.previous.clone(), message);
    }

    fn error_at(&mut self, token: Token<'a>, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprintln!("[line {}] Error: {}", token.line, message);
        if token.kind == TokenType::Eof {
            eprintln!(" at end");
        } else if token.kind == TokenType::Error {
            // Do nothing.
        } else {
            eprintln!(" at '{}'", token.origin);
        }
        eprintln!("{message}");
        self.had_error = true;
    }
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Compiler {
            parser: Parser::new(source),
        }
    }

    pub fn compile(&mut self) -> Chunk {
        self.parser.compile()
    }
}

impl<'a> ParseRule<'a> {
    fn new(
        prefix: Option<ParseFn<'a>>,
        infix: Option<ParseFn<'a>>,
        precedence: Precedence,
    ) -> Self {
        ParseRule {
            prefix,
            infix,
            precedence,
        }
    }

    fn get_rule(kind: TokenType) -> Self {
        match kind {
            TokenType::LeftParen => Self::new(Some(Parser::grouping), None, Precedence::None),
            TokenType::RightParen => Self::new(None, None, Precedence::None),
            TokenType::LeftBrace => Self::new(None, None, Precedence::None),
            TokenType::RightBrace => Self::new(None, None, Precedence::None),
            TokenType::Comma => Self::new(None, None, Precedence::None),
            TokenType::Dot => Self::new(None, None, Precedence::None),
            TokenType::Minus => {
                Self::new(Some(Parser::unary), Some(Parser::binary), Precedence::Term)
            }
            TokenType::Plus => Self::new(None, Some(Parser::binary), Precedence::Term),
            TokenType::Semicolon => Self::new(None, None, Precedence::None),
            TokenType::Slash => Self::new(None, Some(Parser::binary), Precedence::Factor),
            TokenType::Star => Self::new(None, Some(Parser::binary), Precedence::Factor),
            TokenType::Bang => Self::new(Some(Parser::unary), None, Precedence::None),
            TokenType::BangEqual => Self::new(None, Some(Parser::binary), Precedence::Equality),
            TokenType::Equal => Self::new(None, None, Precedence::None),
            TokenType::EqualEqual => Self::new(None, Some(Parser::binary), Precedence::Equality),
            TokenType::Greater => Self::new(None, Some(Parser::binary), Precedence::Comparison),
            TokenType::GreaterEqual => {
                Self::new(None, Some(Parser::binary), Precedence::Comparison)
            }
            TokenType::Less => Self::new(None, Some(Parser::binary), Precedence::Comparison),
            TokenType::LessEqual => Self::new(None, Some(Parser::binary), Precedence::Comparison),
            TokenType::Identifier => Self::new(Some(Parser::variable), None, Precedence::None),
            TokenType::String => Self::new(Some(Parser::string), None, Precedence::None),
            TokenType::Number => Self::new(Some(Parser::number), None, Precedence::None),
            TokenType::And => Self::new(None, None, Precedence::None),
            TokenType::Class => Self::new(None, None, Precedence::None),
            TokenType::Else => Self::new(None, None, Precedence::None),
            TokenType::False => Self::new(Some(Parser::literal), None, Precedence::None),
            TokenType::For => Self::new(None, None, Precedence::None),
            TokenType::Fun => Self::new(None, None, Precedence::None),
            TokenType::If => Self::new(None, None, Precedence::None),
            TokenType::Nil => Self::new(Some(Parser::literal), None, Precedence::None),
            TokenType::Or => Self::new(None, None, Precedence::None),
            TokenType::Print => Self::new(None, None, Precedence::None),
            TokenType::Return => Self::new(None, None, Precedence::None),
            TokenType::Super => Self::new(None, None, Precedence::None),
            TokenType::This => Self::new(None, None, Precedence::None),
            TokenType::True => Self::new(Some(Parser::literal), None, Precedence::None),
            TokenType::Var => Self::new(None, None, Precedence::None),
            TokenType::While => Self::new(None, None, Precedence::None),
            TokenType::Error => Self::new(None, None, Precedence::None),
            TokenType::Eof => Self::new(None, None, Precedence::None),
        }
    }
}
