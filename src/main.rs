mod chunk;
mod common;
mod value;
mod vm;

pub use chunk::{Chunk, OpCode};
pub use value::Value;
use vm::Vm;

fn main() {
    let mut chunk = Chunk::new();
    let line = 123;
    let pos = chunk.add_constant(1.2);
    chunk.write_code(OpCode::Constant, line);
    chunk.write_byte(pos as u8, line);

    let pos = chunk.add_constant(3.4);
    chunk.write_code(OpCode::Constant, line);
    chunk.write_byte(pos as u8, line);

    chunk.write_code(OpCode::Add, line);

    let pos = chunk.add_constant(5.6);
    chunk.write_code(OpCode::Constant, 123);
    chunk.write_byte(pos as u8, 123);

    chunk.write_code(OpCode::Divide, line);

    chunk.write_code(OpCode::Negate, line);
    chunk.write_code(OpCode::Return, line);

    let mut vm = Vm::new();
    vm.interpret(chunk);
}
