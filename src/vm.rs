use std::array;

use crate::{compiler::Compiler, Chunk, OpCode, Value};

const STACK_MAX_SIZE: usize = 256;

pub enum InterpretResult {
    Ok,
    #[allow(unused)]
    CompileError,
    RuntimeError(&'static str),
}
pub struct Vm {
    chunk: Option<Chunk>,
    ip: usize,
    stack: [Value; STACK_MAX_SIZE],
    stack_top: usize,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            chunk: None,
            ip: 0,
            stack: array::from_fn(|_| Value::Nil),
            stack_top: 0,
        }
    }

    fn read_byte(&mut self) -> u8 {
        match &self.chunk {
            Some(chunk) => {
                let byte = chunk.code[self.ip];
                self.ip += 1;
                byte
            }
            None => 0,
        }
    }

    pub fn interpret_chunk(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = Some(chunk);
        self.run()
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let mut compiler = Compiler::new(source);
        self.chunk = Some(compiler.compile());
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            // Debug stack info
            self.print_stack();
            // Disassemble instruction for debug
            self.chunk
                .as_ref()
                .unwrap()
                .disassemble_instruction(self.ip);
            match OpCode::try_from(self.read_byte()).unwrap_or(OpCode::Unknown) {
                OpCode::Constant => {
                    let byte = self.read_byte();
                    let constant = self.chunk.as_ref().unwrap().read_constant(byte);
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
                        let a = self.pop_stack().as_string().unwrap();
                        let b = self.pop_stack().as_string().unwrap();
                        self.push_stack(Value::from(&*format!("{a}{b}")));
                    }
                    _ => {
                        return InterpretResult::RuntimeError(
                            "Operands must be two numbers or two strings.",
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
                    let value = self.pop_stack();
                    print!("{value}");
                    println!();
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
                OpCode::Unknown => return InterpretResult::RuntimeError("Unknown opcode"),
            }
        }
        // InterpretResult::Ok
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
