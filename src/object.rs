use std::{
    cell::RefCell,
    iter,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use ustr::Ustr;

use crate::{value::intern_str, Chunk, Value};

#[derive(Clone)]
pub struct UpvalueObj {
    pub location: usize,
    pub value: Value,
    pub closed: Option<Value>,
    pub next: Option<Rc<RefCell<UpvalueObj>>>,
}

impl std::fmt::Debug for UpvalueObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("UpvalueObj")
            .field("location", &self.location)
            .field("value", &self.value)
            .field("closed", &self.closed)
            .field("next", &self.next.is_some())
            .finish()
    }
}

impl Default for UpvalueObj {
    fn default() -> Self {
        Self::new(0)
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Function,
    pub upvalues: Vec<Rc<RefCell<UpvalueObj>>>,
}

impl Default for Closure {
    fn default() -> Self {
        Self {
            function: Function::empty(),
            upvalues: Vec::new(),
        }
    }
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

impl UpvalueObj {
    pub fn new(location: usize) -> Self {
        Self {
            location,
            value: Value::Nil,
            closed: None,
            next: None,
        }
    }
}

impl Closure {
    pub fn new(function: Function) -> Self {
        let upvalues = iter::repeat_with(UpvalueObj::default)
            .take(function.upvalue_count as usize)
            .map(|u| Rc::new(RefCell::new(u)))
            .collect::<Vec<_>>();
        Self { function, upvalues }
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

    fn empty() -> Self {
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
