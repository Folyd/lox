use crate::{chunk::print_value, Chunk, OpCode, Value};

const STACK_MAX_SIZE: usize = 256;

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
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
            stack: [Value::Nnon; STACK_MAX_SIZE],
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

    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = Some(chunk);
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
                OpCode::Negate => {
                    let mut c = self.pop_stack();
                    if !c.negate() {
                        return InterpretResult::RuntimeError;
                    }
                    self.push_stack(c);
                }
                OpCode::Return => {
                    let value = self.pop_stack();
                    print_value(&value);
                    println!();
                    return InterpretResult::Ok;
                }
                OpCode::Unknown => return InterpretResult::RuntimeError,
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
            self.stack[self.stack_top]
        } else {
            panic!("Stack underflow")
        }
    }

    fn print_stack(&self) {
        print!("          ");
        for value in self.stack.iter().take(self.stack_top) {
            print!("[");
            print_value(value);
            print!("]")
        }
        println!();
    }
}
