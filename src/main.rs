use std::{env::args, fs, process::exit};

mod builtins;
mod chunk;
mod common;
mod compiler;
mod object;
mod scanner;
mod value;
mod vm;

pub use chunk::{Chunk, OpCode};
pub use value::Value;
use vm::Vm;

fn main() {
    let mut args = args();
    if args.len() == 1 {
        // repl
    } else if args.len() == 2 {
        let path = args.nth(1).unwrap();
        run_file(&path);
    } else {
        println!("Usage: clox [path]");
        exit(64);
    }
}

fn run_file(path: &str) {
    let source = fs::read_to_string(path).unwrap();
    let mut vm = Vm::new();
    vm.define_builtins();
    match vm.interpret(&source) {
        vm::InterpretResult::CompileError => exit(70),
        vm::InterpretResult::RuntimeError(err) => {
            println!("RuntimeError: {err}");
            exit(70)
        }
        _ => {}
    }
}

#[allow(unused)]
fn test() {
    let mut chunk = Chunk::new();
    let line = 123;
    let pos = chunk.add_constant(1.2.into());
    chunk.write_code(OpCode::Constant, line);
    chunk.write_byte(pos as u8, line);

    let pos = chunk.add_constant(3.4.into());
    chunk.write_code(OpCode::Constant, line);
    chunk.write_byte(pos as u8, line);

    chunk.write_code(OpCode::Add, line);

    let pos = chunk.add_constant(5.6.into());
    chunk.write_code(OpCode::Constant, 123);
    chunk.write_byte(pos as u8, 123);

    chunk.write_code(OpCode::Divide, line);

    chunk.write_code(OpCode::Negate, line);
    chunk.write_code(OpCode::Return, line);

    let mut vm = Vm::new();
    // vm.interpret_chunk(chunk);
}
