use std::{array, borrow::Cow};

use ustr::UstrMap;

use crate::{compiler::Compiler, Chunk, OpCode, Value};

const STACK_MAX_SIZE: usize = 256;
pub enum InterpretResult {
    Ok,
    #[allow(unused)]
    CompileError,
    RuntimeError(Cow<'static, str>),
}
pub struct Vm {
    chunk: Chunk,
    ip: usize,
    stack: [Value; STACK_MAX_SIZE],
    stack_top: usize,
    globals: UstrMap<Value>,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            chunk: Chunk::new(),
            ip: 0,
            stack: array::from_fn(|_| Value::Nil),
            stack_top: 0,
            globals: UstrMap::default(),
        }
    }

    fn read_byte(&mut self) -> u8 {
        let byte = self.chunk.code[self.ip];
        self.ip += 1;
        byte
    }

    fn read_short(&mut self) -> u16 {
        let short = u16::from_be_bytes([self.chunk.code[self.ip], self.chunk.code[self.ip + 1]]);
        self.ip += 2;
        short
    }

    pub fn interpret_chunk(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.run()
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let mut compiler = Compiler::new();
        self.chunk = compiler.compile(source);
        self.chunk.disassemble("main");
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            // Debug stack info
            // self.print_stack();
            // // Disassemble instruction for debug
            // self.chunk.disassemble_instruction(self.ip);
            match OpCode::try_from(self.read_byte()).unwrap_or(OpCode::Unknown) {
                OpCode::Constant => {
                    let byte = self.read_byte();
                    let constant = self.chunk.read_constant(byte);
                    self.push_stack(constant);
                    // print_value(&constant);
                    // println!();
                    // break;
                }
                OpCode::Add => match (self.peek(0), self.peek(1)) {
                    (Value::Number(_), Value::Number(_)) => {
                        let a = self.pop_stack().as_number().unwrap();
                        let b = self.pop_stack().as_number().unwrap();
                        self.push_stack((a + b).into());
                    }
                    (Value::String(_), Value::String(_)) => {
                        let b = self.pop_stack().as_string().unwrap();
                        let a = self.pop_stack().as_string().unwrap();
                        self.push_stack(format!("{a}{b}").into());
                    }
                    _ => {
                        return InterpretResult::RuntimeError(
                            "Operands must be two numbers or two strings.".into(),
                        )
                    }
                },
                OpCode::Subtract => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a - b).into());
                }
                OpCode::Multiply => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a * b).into());
                }
                OpCode::Divide => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a / b).into());
                }
                OpCode::Negate => {
                    let v = self.pop_stack().as_number().unwrap();
                    self.push_stack((-v).into());
                }
                OpCode::Return => {
                    return InterpretResult::Ok;
                }
                OpCode::Nil => self.push_stack(Value::Nil),
                OpCode::True => self.push_stack(Value::Boolean(true)),
                OpCode::False => self.push_stack(Value::Boolean(false)),
                OpCode::Not => {
                    let v = self.pop_stack().as_boolean().unwrap();
                    self.push_stack((!v).into())
                }
                OpCode::Equal => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    self.push_stack((a == b).into());
                }
                OpCode::Greater => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a > b).into());
                }
                OpCode::Less => {
                    let b = self.pop_stack().as_number().unwrap();
                    let a = self.pop_stack().as_number().unwrap();
                    self.push_stack((a < b).into());
                }
                OpCode::Print => {
                    let value = self.pop_stack();
                    println!("{value}");
                }
                OpCode::Pop => {
                    self.pop_stack();
                }
                OpCode::DefineGlobal => {
                    let byte = self.read_byte();
                    let varible_name = self.chunk.read_constant(byte).as_string().unwrap();
                    self.globals.insert(varible_name, self.peek(0).clone());
                    self.pop_stack();
                }
                OpCode::GetGlobal => {
                    let byte = self.read_byte();
                    let varible_name = self.chunk.read_constant(byte).as_string().unwrap();
                    if let Some(value) = self.globals.get(&varible_name) {
                        self.push_stack(value.clone());
                    } else {
                        return InterpretResult::RuntimeError(
                            format!("Undefined variable '{}'", varible_name).into(),
                        );
                    }
                }
                OpCode::SetGlobal => {
                    let byte = self.read_byte();
                    let varible_name = self.chunk.read_constant(byte).as_string().unwrap();
                    #[allow(clippy::map_entry)]
                    if self.globals.contains_key(&varible_name) {
                        self.globals.insert(varible_name, self.peek(0).clone());
                    } else {
                        return InterpretResult::RuntimeError(
                            format!("Undefined variable '{}'", varible_name).into(),
                        );
                    }
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte();
                    self.push_stack(self.stack[slot as usize].clone());
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte();
                    self.stack[slot as usize] = self.peek(0).clone();
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.peek(0).is_falsy() {
                        self.ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short();
                    self.ip += offset as usize;
                }
                OpCode::Unknown => return InterpretResult::RuntimeError("Unknown opcode".into()),
            }
        }
    }
}

impl Vm {
    fn push_stack(&mut self, value: Value) {
        if self.stack_top < STACK_MAX_SIZE {
            self.stack[self.stack_top] = value;
            self.stack_top += 1;
        } else {
            panic!("Stack overflow");
        }
    }

    fn pop_stack(&mut self) -> Value {
        if self.stack_top > 0 {
            self.stack_top -= 1;
            self.stack[self.stack_top].clone()
        } else {
            // panic!("Stack underflow")
            Value::Nil
        }
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack_top - 1 - distance]
    }

    #[allow(unused)]
    fn print_stack(&self) {
        print!("          ");
        for value in self.stack.iter().take(self.stack_top) {
            print!("[");
            print!("{value}");
            print!("]")
        }
        println!();
    }
}
