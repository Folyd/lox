use std::ops::{Deref, DerefMut};

use ustr::Ustr;

use crate::{value::intern_str, Chunk, Value};

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Function,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Ustr,
    pub upvalue_count: u8,
}

pub type NativeFn = fn(Vec<Value>) -> Value;

#[derive(Debug, PartialEq, Eq)]
pub enum FunctionType {
    Function,
    Script,
    // Native,
}

impl Default for Function {
    fn default() -> Self {
        Self::empty()
    }
}

impl Closure {
    pub fn new(function: Function) -> Self {
        Self { function }
    }
}

impl Function {
    pub fn new(name: &str, arity: u8) -> Self {
        Self {
            arity,
            chunk: Chunk::new(),
            name: intern_str(name),
            upvalue_count: 0,
        }
    }

    pub fn empty() -> Self {
        Self::new("", 0)
    }
}

impl Deref for Function {
    type Target = Chunk;
    fn deref(&self) -> &Self::Target {
        &self.chunk
    }
}

impl DerefMut for Function {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.chunk
    }
}
