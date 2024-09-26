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
    Unknown,
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    // count: usize,
    // capacity: usize,
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
        let mut offset = 0usize;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);
        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", self.lines[offset]);
        }

        if let Some(code) = self.code.get(offset) {
            match OpCode::try_from(*code).unwrap_or(OpCode::Unknown) {
                OpCode::Return => return simple_instruction("OP_RETURN", offset),
                OpCode::Constant => return self.constant_instruction("OP_CONSTANT", offset),
                OpCode::Add => return simple_instruction("OP_ADD", offset),
                OpCode::Subtract => return simple_instruction("OP_SUBTRACT", offset),
                OpCode::Multiply => return simple_instruction("OP_MULTIPLY", offset),
                OpCode::Divide => return simple_instruction("OP_DIVIDE", offset),
                OpCode::Negate => return simple_instruction("OP_NEGATE", offset),
                OpCode::Nil => return simple_instruction("OP_NIL", offset),
                OpCode::True => return simple_instruction("OP_TRUE", offset),
                OpCode::False => return simple_instruction("OP_FALSE", offset),
                OpCode::Not => return simple_instruction("OP_NOT", offset),
                OpCode::Equal => return simple_instruction("OP_EQUAL", offset),
                OpCode::Greater => return simple_instruction("OP_GREATER", offset),
                OpCode::Less => return simple_instruction("OP_LESS", offset),
                OpCode::Print => return simple_instruction("OP_PRINT", offset),
                OpCode::Pop => return simple_instruction("OP_POP", offset),
                OpCode::DefineGlobal => {
                    return self.constant_instruction("OP_DEFINE_GLOBAL", offset)
                }
                OpCode::GetGlobal => return self.constant_instruction("OP_GET_GLOBAL", offset),
                OpCode::SetGlobal => return self.constant_instruction("OP_SET_GLOBAL", offset),
                OpCode::Unknown => {}
            }
        } else {
            println!("Invalid opcode at offset: {offset}");
        }

        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let constant = self.code[offset + 1];
        print!("{:-16} {:04} ", name, constant);
        print!("{}", self.constans[constant as usize]);
        println!();
        offset + 2
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{name}");
    offset + 1
}
