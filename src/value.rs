use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Const(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(v) => write!(f, "{}", v),
        }
    }
}
