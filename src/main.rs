use std::{env::args, fs, process::exit};

mod builtins;
mod chunk;
mod compiler;
mod fuel;
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
    let source: &'static str = Box::leak(source.into_boxed_str());
    let mut vm = Vm::new();
    match vm.interpret(source) {
        vm::InterpretResult::CompileError => exit(70),
        vm::InterpretResult::RuntimeError(err) => {
            println!("RuntimeError: {err}");
            exit(70)
        }
        _ => {}
    }
}
