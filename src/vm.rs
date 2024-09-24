use crate::{chunk::print_value, Chunk, OpCode};

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
pub struct Vm {
    chunk: Option<Chunk>,
    ip: usize,
}

impl Vm {
    pub fn new() -> Vm {
        Vm { chunk: None, ip: 0 }
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
            // Disassemble instruction for debug
            self.chunk
                .as_ref()
                .unwrap()
                .disassemble_instruction(self.ip);
            match OpCode::try_from(self.read_byte()).unwrap_or(OpCode::Unknown) {
                OpCode::Constant => {
                    let byte = self.read_byte();
                    let constant = self.chunk.as_ref().unwrap().read_constant(byte);
                    print_value(&constant);
                    println!();
                    break;
                }
                OpCode::Return => return InterpretResult::Ok,
                OpCode::Unknown => return InterpretResult::RuntimeError,
            }
        }
        InterpretResult::Ok
    }
}
