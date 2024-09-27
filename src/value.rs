use std::fmt::Display;

use ustr::Ustr;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    String(Ustr),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Nil => write!(f, ""),
        }
    }
}

impl Value {
    pub fn as_number(self) -> Result<f64, &'static str> {
        match self {
            Value::Number(value) => Ok(value),
            _ => Err("cannot convert to number"),
        }
    }

    pub fn as_boolean(&self) -> Result<bool, &'static str> {
        match self {
            Value::Boolean(value) => Ok(*value),
            Value::Nil => Ok(false),
            Value::Number(value) => Ok(*value != 0.0),
            Value::String(s) => Ok(!s.is_empty()),
        }
    }

    pub fn as_string(self) -> Result<Ustr, &'static str> {
        match self {
            Value::String(value) => Ok(value),
            _ => Err("cannot convert to string"),
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

fn intern_str(s: &str) -> Ustr {
    Ustr::from_existing(s).unwrap_or_else(|| Ustr::from(s))
}
