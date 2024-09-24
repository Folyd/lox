use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Const(f64),
    Nnon,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Const(v) => write!(f, "{}", v),
            Value::Nnon => write!(f, ""),
        }
    }
}

impl Value {
    pub fn negate(&mut self) -> bool {
        if let Value::Const(v) = self {
            *v = -*v;
            true
        } else {
            false
        }
    }
}
