use std::{mem, ops::Add};

use crate::{
    scanner::{Scanner, Token, TokenType},
    Chunk, OpCode,
};
use num_enum::{IntoPrimitive, TryFromPrimitive};

type ParseFn<'a> = fn(&mut Parser<'a>);

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

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.write_byte(byte, self.previous.line);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.chunk.write_byte(byte1, self.previous.line);
        self.chunk.write_byte(byte2, self.previous.line);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return.into());
    }

    fn emity_constant(&mut self, value: f64) {
        let constant = self.chunk.add_constant(value);
        if constant > u8::MAX as usize {
            self.error("Too many constants in one chunk.");
            return;
        }
        self.emit_bytes(OpCode::Constant.into(), constant as u8);
    }
}

impl<'a> Parser<'a> {
    fn compile(&mut self) -> Chunk {
        self.advance();
        self.expression();
        self.consume(TokenType::Eof, "Expect end of expression.");

        self.emit_return();

        mem::take(&mut self.chunk)
    }

    fn advance(&mut self) {
        self.previous = mem::take(&mut self.current);
        while let Some(token) = self.scanner.next() {
            self.current = token;
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

        if let Some(prefix_fn) = rule.prefix {
            prefix_fn(self);
        } else {
            self.error("Expect expression.");
            return;
        }

        while precedence <= ParseRule::get_rule(self.current.kind).precedence {
            self.advance();
            if let Some(infix_fn) = rule.infix {
                infix_fn(self);
            }
        }
    }

    fn consume(&mut self, kind: TokenType, message: &str) {
        if self.current.kind == kind {
            self.advance();
            return;
        }
        self.error_at_current(message);
    }

    fn number(&mut self) {
        self.emity_constant(self.previous.origin.parse().unwrap());
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_kind = self.previous.kind;
        self.expression();
        if let TokenType::Minus = operator_kind {
            self.emit_byte(OpCode::Negate.into())
        }
    }

    fn binary(&mut self) {
        let operator_kind = self.previous.kind;
        let rule = ParseRule::get_rule(operator_kind);
        self.parse_precedence(rule.precedence + 1);

        match operator_kind {
            TokenType::Plus => self.emit_byte(OpCode::Add.into()),
            TokenType::Minus => self.emit_byte(OpCode::Subtract.into()),
            TokenType::Star => self.emit_byte(OpCode::Multiply.into()),
            TokenType::Slash => self.emit_byte(OpCode::Divide.into()),
            _ => (),
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
        let chunk = self.parser.compile();
        chunk.disassemble("code");
        chunk
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
            TokenType::Bang => Self::new(None, None, Precedence::None),
            TokenType::BangEqual => Self::new(None, None, Precedence::None),
            TokenType::Equal => Self::new(None, None, Precedence::None),
            TokenType::EqualEqual => Self::new(None, None, Precedence::None),
            TokenType::Greater => Self::new(None, None, Precedence::None),
            TokenType::GreaterEqual => Self::new(None, None, Precedence::None),
            TokenType::Less => Self::new(None, None, Precedence::None),
            TokenType::LessEqual => Self::new(None, None, Precedence::None),
            TokenType::Identifier => Self::new(None, None, Precedence::None),
            TokenType::String => Self::new(None, None, Precedence::None),
            TokenType::Number => Self::new(Some(Parser::number), None, Precedence::None),
            TokenType::And => Self::new(None, None, Precedence::None),
            TokenType::Class => Self::new(None, None, Precedence::None),
            TokenType::Else => Self::new(None, None, Precedence::None),
            TokenType::False => Self::new(None, None, Precedence::None),
            TokenType::For => Self::new(None, None, Precedence::None),
            TokenType::Fun => Self::new(None, None, Precedence::None),
            TokenType::If => Self::new(None, None, Precedence::None),
            TokenType::Nil => Self::new(None, None, Precedence::None),
            TokenType::Or => Self::new(None, None, Precedence::None),
            TokenType::Print => Self::new(None, None, Precedence::None),
            TokenType::Return => Self::new(None, None, Precedence::None),
            TokenType::Super => Self::new(None, None, Precedence::None),
            TokenType::This => Self::new(None, None, Precedence::None),
            TokenType::True => Self::new(None, None, Precedence::None),
            TokenType::Var => Self::new(None, None, Precedence::None),
            TokenType::While => Self::new(None, None, Precedence::None),
            TokenType::Error => Self::new(None, None, Precedence::None),
            TokenType::Eof => Self::new(None, None, Precedence::None),
        }
    }
}
