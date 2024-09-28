use std::ops::{Deref, DerefMut};

use crate::Chunk;

pub struct Function<'a> {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: &'a str,
}

pub enum FunctionType {
    Script,
    Native,
}

impl<'a> Function<'a> {
    pub fn new(name: &'a str, arity: u8) -> Self {
        Self {
            arity,
            chunk: Chunk::new(),
            name,
        }
    }
}

impl Deref for Function<'_> {
    type Target = Chunk;
    fn deref(&self) -> &Self::Target {
        &self.chunk
    }
}

impl DerefMut for Function<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.chunk
    }
}
