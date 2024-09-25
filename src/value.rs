use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Const(f64),
    None,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Const(v) => write!(f, "{}", v),
            Value::None => write!(f, ""),
        }
    }
}

impl Value {
    pub fn as_number(self) -> Result<f64, &'static str> {
        match self {
            Value::Const(value) => Ok(value),
            Value::None => Err("cannot convert to number"),
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Const(value)
    }
}
