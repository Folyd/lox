use std::{
    iter,
    ops::{Deref, DerefMut},
};

use gc_arena::{
    lock::{GcRefLock, RefLock},
    Collect, Gc, Mutation,
};

use crate::{Chunk, Value};

#[derive(Debug, Copy, Clone, Collect)]
#[collect[no_drop]]
pub struct UpvalueObj<'gc> {
    pub location: usize,
    pub closed: Option<Value<'gc>>,
    pub next: Option<GcRefLock<'gc, UpvalueObj<'gc>>>,
}

impl<'gc> Default for UpvalueObj<'gc> {
    fn default() -> Self {
        Self::new(0)
    }
}

#[derive(Debug, Clone, Collect)]
#[collect[no_drop]]
pub struct Closure<'gc> {
    pub function: Gc<'gc, Function<'gc>>,
    pub upvalues: Box<[GcRefLock<'gc, UpvalueObj<'gc>>]>,
}

#[derive(Debug, Clone, Collect)]
#[collect[no_drop]]
pub struct Function<'gc> {
    pub arity: u8,
    pub chunk: Chunk<'gc>,
    pub name: Gc<'gc, String>,
    pub upvalue_count: u8,
}

pub type NativeFn<'gc> = fn(Vec<Value<'gc>>) -> Value<'gc>;

#[derive(Debug, PartialEq, Eq)]
pub enum FunctionType {
    Function,
    Script,
}

// impl<'gc> Default for Function<'gc> {
//     fn default() -> Self {
//         Self::empty()
//     }
// }

impl<'gc> UpvalueObj<'gc> {
    pub fn new(location: usize) -> Self {
        Self {
            location,
            closed: None,
            next: None,
        }
    }
}

impl<'gc> Closure<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>, function: Gc<'gc, Function<'gc>>) -> Self {
        let upvalues = iter::repeat_with(|| Gc::new(mc, RefLock::new(UpvalueObj::default())))
            .take(function.upvalue_count as usize)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Self { function, upvalues }
    }
}

impl<'gc> Function<'gc> {
    pub fn new(mc: &'gc Mutation<'gc>, name: &str, arity: u8) -> Self {
        Self {
            arity,
            chunk: Chunk::new(),
            name: Gc::new(mc, name.to_owned()),
            upvalue_count: 0,
        }
    }

    // fn empty() -> Self {
    //     Self::new("", 0)
    // }
}

impl<'gc> Deref for Function<'gc> {
    type Target = Chunk<'gc>;
    fn deref(&self) -> &Self::Target {
        &self.chunk
    }
}

impl<'gc> DerefMut for Function<'gc> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.chunk
    }
}
