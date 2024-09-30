use std::{borrow::Cow, fmt::Display};

use ustr::Ustr;

use crate::object::Function;

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(Ustr),
    Function(Box<Function>),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(fun) => {
                if fun.name.is_empty() {
                    write!(f, "<script>")
                } else {
                    write!(f, "<fn {}>", fun.name)
                }
            }
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl Value {
    pub fn equals(&self, other: &Value) -> bool {
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

    pub fn as_string(self) -> Result<Ustr, &'static str> {
        match self {
            Value::String(value) => Ok(value),
            _ => Err("cannot convert to string"),
        }
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Value::Function(_))
    }

    pub fn as_function(&self) -> Result<(), &'static str> {
        match self {
            Value::Function(_) => Ok(()),
            _ => Err("cannot convert to function"),
        }
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

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Number(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Boolean(value)
    }
}

impl From<Ustr> for Value {
    fn from(value: Ustr) -> Self {
        Value::String(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::String(intern_str(&value))
    }
}
impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::String(intern_str(value))
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Value::Function(Box::new(value))
    }
}

pub fn intern_str(s: &str) -> Ustr {
    Ustr::from_existing(s).unwrap_or_else(|| Ustr::from(s))
}
