use std::fmt;

use super::primitive::Primitive;

#[derive(Debug, PartialEq)]
pub enum Value<'tok> {
    Primitive(Primitive),
    Ident(&'tok str),
    String(&'tok str),
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Primitive(p) => write!(f, "{}", p),
            Value::Ident(i) => write!(f, "{i}"),
            Value::String(s) => write!(f, "{s}"),
        }
    }
}
