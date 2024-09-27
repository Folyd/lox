use std::sync::Once;

use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::Value;

#[derive(Copy, Clone, Debug, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Return,
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Nil,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
    Unknown,
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constans: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constans: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write_code(&mut self, code: OpCode, line: usize) {
        self.write_byte(code, line);
    }

    pub fn write_byte<T: Into<u8>>(&mut self, byte: T, line: usize) {
        self.code.push(byte.into());
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constans.push(value);
        // return the index where the constant
        // was appended so that we can locate that same constant later
        self.constans.len() - 1
    }

    pub fn read_constant(&self, byte: u8) -> Value {
        self.constans[byte as usize].clone()
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        static ONCE_TITLE: Once = Once::new();
        ONCE_TITLE.call_once(|| {
            println!(
                "{:4} {:4} {:16} {:04} Constvalue",
                "IP", "Line", "OPCode", "CIndex"
            );
        });

        print!("{:04} ", offset);
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        if let Some(code) = self.code.get(offset) {
            match OpCode::try_from(*code).unwrap_or(OpCode::Unknown) {
                OpCode::Return => return simple_instruction("RETURN", offset),
                OpCode::Constant => return self.constant_instruction("CONSTANT", offset),
                OpCode::Add => return simple_instruction("ADD", offset),
                OpCode::Subtract => return simple_instruction("SUBTRACT", offset),
                OpCode::Multiply => return simple_instruction("MULTIPLY", offset),
                OpCode::Divide => return simple_instruction("DIVIDE", offset),
                OpCode::Negate => return simple_instruction("NEGATE", offset),
                OpCode::Nil => return simple_instruction("NIL", offset),
                OpCode::True => return simple_instruction("TRUE", offset),
                OpCode::False => return simple_instruction("FALSE", offset),
                OpCode::Not => return simple_instruction("NOT", offset),
                OpCode::Equal => return simple_instruction("EQUAL", offset),
                OpCode::Greater => return simple_instruction("GREATER", offset),
                OpCode::Less => return simple_instruction("LESS", offset),
                OpCode::Print => return simple_instruction("PRINT", offset),
                OpCode::Pop => return simple_instruction("POP", offset),
                OpCode::DefineGlobal => return self.constant_instruction("DEFINE_GLOBAL", offset),
                OpCode::GetGlobal => return self.constant_instruction("GET_GLOBAL", offset),
                OpCode::SetGlobal => return self.constant_instruction("SET_GLOBAL", offset),
                OpCode::GetLocal => return self.byte_instruction("GET_LOCAL", offset),
                OpCode::SetLocal => return self.byte_instruction("SET_LOCAL", offset),
                OpCode::JumpIfFalse => return self.jump_instruction("JUMP_IF_FALSE", 1, offset),
                OpCode::Jump => return self.jump_instruction("JUMP", 1, offset),
                OpCode::Loop => return self.jump_instruction("LOOP", -1, offset),
                OpCode::Unknown => {}
            }
        } else {
            println!("Invalid opcode at offset: {offset}");
        }

        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        println!(
            "{:-16} {:04} {}",
            name, constant, self.constans[constant as usize]
        );
        offset + 2
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        println!("{:-16} {:04}", name, slot);
        offset + 2
    }

    fn jump_instruction(&self, name: &str, sign: i8, offset: usize) -> usize {
        let jump = u16::from_be_bytes([self.code[offset + 1], self.code[offset + 2]]);
        let jump = if sign < 0 {
            offset.saturating_add(3).saturating_sub(jump as usize)
        } else {
            offset.saturating_add(3).saturating_add(jump as usize)
        };

        println!("{:-16} {:04} -> {:04}", name, offset, jump);
        offset + 3
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}
