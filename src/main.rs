mod chunk;
mod common;
mod value;
mod vm;

pub use chunk::{Chunk, OpCode};
pub use value::Value;
use vm::Vm;

fn main() {
    let mut chunk = Chunk::new();
    let pos = chunk.add_constant(1.2);
    chunk.write_code(OpCode::Constant, 123);
    chunk.write_byte(pos as u8, 123);
    chunk.write_code(OpCode::Return, 123);
    // chunk.disassemble("test chunk");

    let mut vm = Vm::new();
    vm.interpret(chunk);
}
