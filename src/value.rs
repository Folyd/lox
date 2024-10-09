use std::{borrow::Cow, fmt::Display};

use gc_arena::{lock::GcRefLock, Collect, Gc};
use ustr::Ustr;

use crate::object::{BoundMethod, Class, Closure, Function, Instance, NativeFn};

#[derive(Debug, Copy, Clone)]
pub enum Value<'gc> {
    Number(f64),
    Boolean(bool),
    String(Gc<'gc, String>),
    // String(Gc<'gc, Ustr>),
    Function(Gc<'gc, Function<'gc>>),
    Closure(Gc<'gc, Closure<'gc>>),
    NativeFunction(NativeFn<'gc>),
    Class(GcRefLock<'gc, Class<'gc>>),
    Instance(GcRefLock<'gc, Instance<'gc>>),
    BoundMethod(Gc<'gc, BoundMethod<'gc>>),
    Nil,
}

unsafe impl<'gc> Collect for Value<'gc> {
    fn needs_trace() -> bool
    where
        Self: Sized,
    {
        true
    }

    fn trace(&self, cc: &gc_arena::Collection) {
        match self {
            Value::String(s) => s.trace(cc),
            Value::Function(fun) => fun.trace(cc),
            Value::Closure(closure) => closure.trace(cc),
            _ => {}
        }
    }
}

impl<'gc> Display for Value<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(fun) => write!(f, "{}", fun),
            Value::Closure(closure) => {
                if closure.function.name.is_empty() {
                    write!(f, "<script>")
                } else {
                    write!(f, "<fn {}>", closure.function.name)
                }
            }
            Value::NativeFunction(_) => write!(f, "<native fn>"),
            Value::Class(class) => write!(f, "{}", class.borrow().name),
            Value::Instance(instance) => {
                write!(f, "{} instance", instance.borrow().class.borrow().name)
            }
            Value::BoundMethod(bm) => write!(f, "{}", bm.method.function),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl<'gc> Value<'gc> {
    pub fn equals(&self, other: &Value<'gc>) -> bool {
        match (self, other) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }

    pub fn as_number(self) -> Result<f64, Cow<'static, str>> {
        match self {
            Value::Number(value) => Ok(value),
            a => Err(format!("cannot convert to number: {:?}", a).into()),
        }
    }

    pub fn as_boolean(&self) -> Result<bool, &'static str> {
        match self {
            Value::Boolean(value) => Ok(*value),
            Value::Number(value) => Ok(*value != 0.0),
            Value::String(s) => Ok(!s.is_empty()),
            _ => Ok(false),
        }
    }

    pub fn as_string(self) -> Result<Gc<'gc, String>, &'static str> {
        match self {
            Value::String(value) => Ok(value),
            _ => Err("cannot convert to string"),
        }
    }

    pub fn as_closure(self) -> Result<Gc<'gc, Closure<'gc>>, &'static str> {
        match self {
            Value::Closure(closure) => Ok(closure),
            _ => Err("cannot convert to closure"),
        }
    }

    pub fn as_function(self) -> Result<Gc<'gc, Function<'gc>>, &'static str> {
        match self {
            Value::Function(function) => Ok(function),
            _ => Err("cannot convert to function"),
        }
    }

    pub fn as_class(self) -> Result<GcRefLock<'gc, Class<'gc>>, &'static str> {
        match self {
            Value::Class(class) => Ok(class),
            _ => Err("cannot convert to class"),
        }
    }

    pub fn as_instance(self) -> Result<GcRefLock<'gc, Instance<'gc>>, &'static str> {
        match self {
            Value::Instance(instance) => Ok(instance),
            _ => Err("cannot convert to instance"),
        }
    }

    pub fn as_bound_method(self) -> Result<Gc<'gc, BoundMethod<'gc>>, &'static str> {
        match self {
            Value::BoundMethod(bm) => Ok(bm),
            _ => Err("cannot convert to bound method"),
        }
    }

    pub fn is_bound_method(&self) -> bool {
        matches!(self, Value::BoundMethod(_))
    }

    pub fn is_class(&self) -> bool {
        matches!(self, Value::Class(_))
    }

    pub fn is_instance(&self) -> bool {
        matches!(self, Value::Instance(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Value::Function(_))
    }

    pub fn is_closure(&self) -> bool {
        matches!(self, Value::Closure(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Value::Boolean(_) | Value::Nil)
    }

    pub fn is_true(&self) -> bool {
        !self.is_falsy()
    }

    pub fn is_falsy(&self) -> bool {
        self.is_nil() || !self.as_boolean().unwrap_or(false)
    }
}

impl<'gc> From<u64> for Value<'gc> {
    fn from(value: u64) -> Self {
        Value::Number(value as f64)
    }
}

impl<'gc> From<f64> for Value<'gc> {
    fn from(value: f64) -> Self {
        Value::Number(value)
    }
}

impl<'gc> From<bool> for Value<'gc> {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

// impl<'gc> From<Gc<'gc, Ustr>> for Value<'gc> {
//     fn from(value: Gc<'gc, Ustr>) -> Self {
//         Value::String(value)
//     }
// }

impl<'gc> From<Gc<'gc, String>> for Value<'gc> {
    fn from(value: Gc<'gc, String>) -> Self {
        Value::String(value)
    }
}

// impl<'gc> From<&str> for Value<'gc> {
//     fn from(value: &str) -> Self {
//         Value::String(intern_str(value))
//     }
// }

impl<'gc> From<Gc<'gc, Function<'gc>>> for Value<'gc> {
    fn from(value: Gc<'gc, Function<'gc>>) -> Self {
        Value::Function(value)
    }
}

impl<'gc> From<Gc<'gc, Closure<'gc>>> for Value<'gc> {
    fn from(value: Gc<'gc, Closure<'gc>>) -> Self {
        Value::Closure(value)
    }
}

impl<'gc> From<GcRefLock<'gc, Class<'gc>>> for Value<'gc> {
    fn from(value: GcRefLock<'gc, Class<'gc>>) -> Self {
        Value::Class(value)
    }
}

impl<'gc> From<GcRefLock<'gc, Instance<'gc>>> for Value<'gc> {
    fn from(value: GcRefLock<'gc, Instance<'gc>>) -> Self {
        Value::Instance(value)
    }
}

impl<'gc> From<Gc<'gc, BoundMethod<'gc>>> for Value<'gc> {
    fn from(value: Gc<'gc, BoundMethod<'gc>>) -> Self {
        Value::BoundMethod(value)
    }
}

#[allow(unused)]
pub fn intern_str(s: &str) -> Ustr {
    Ustr::from_existing(s).unwrap_or_else(|| Ustr::from(s))
}
