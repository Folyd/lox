mod chunk;
mod common;
mod value;

pub use chunk::{Chunk, OpCode};
pub use value::Value;

fn main() {
    let mut chunk = Chunk::new();
    let pos = chunk.add_constant(1.2);
    chunk.write_code(OpCode::Constant, 123);
    chunk.write_byte(pos as u8, 123);
    chunk.write_code(OpCode::Return, 123);
    chunk.disassemble("test chunk");
}
