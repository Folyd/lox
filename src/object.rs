use std::ops::{Deref, DerefMut};

use ustr::Ustr;

use crate::{value::intern_str, Chunk};

#[derive(Debug, Clone)]
pub struct Function {
    pub arity: u16,
    pub chunk: Chunk,
    pub name: Ustr,
}

pub enum FunctionType {
    Function,
    Script,
    Native,
}

impl Default for Function {
    fn default() -> Self {
        Self::empty()
    }
}

impl Function {
    pub fn new(name: &str, arity: u16) -> Self {
        Self {
            arity,
            chunk: Chunk::new(),
            name: intern_str(name),
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
