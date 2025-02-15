use core::fmt;
use std::fmt::Display;

use super::kind::Kind;
use super::value::Value;
use super::{IntoToken, Token};

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Numeral {
    Signed(i64),
    Unsigned(u64),
}

impl Display for Numeral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Numeral::Signed(val) => write!(f, "{val}"),
            Numeral::Unsigned(val) => write!(f, "{val}"),
        }
    }
}

pub trait IntoNumeral {
    fn parse_unsigned(self) -> Result<Numeral, std::num::ParseIntError>;
    fn parse_signed(self) -> Result<Numeral, std::num::ParseIntError>;
}

impl<S: AsRef<str>> IntoNumeral for S {
    fn parse_unsigned(self) -> Result<Numeral, std::num::ParseIntError> {
        Ok(Numeral::Unsigned(self.as_ref().parse()?))
    }

    fn parse_signed(self) -> Result<Numeral, std::num::ParseIntError> {
        Ok(Numeral::Signed(self.as_ref().parse()?))
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum FloatSizes {
    F32,
    F64,
}

impl fmt::Display for FloatSizes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatSizes::F32 => write!(f, "f32"),
            FloatSizes::F64 => write!(f, "f64"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum IntSizes {
    U8,
    U16,
    U32,
    U64,
    Usize,
    I8,
    I16,
    I32,
    I64,
    Isize,
}

impl IntSizes {
    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            IntSizes::U8 | IntSizes::U16 | IntSizes::U32 | IntSizes::U64 | IntSizes::Usize
        )
    }

    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            IntSizes::I8 | IntSizes::I16 | IntSizes::I32 | IntSizes::I64 | IntSizes::Isize
        )
    }
}

impl fmt::Display for IntSizes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntSizes::I8 => write!(f, "i8"),
            IntSizes::I16 => write!(f, "i16"),
            IntSizes::I32 => write!(f, "i32"),
            IntSizes::I64 => write!(f, "i64"),
            IntSizes::Isize => write!(f, "isize"),
            IntSizes::U8 => write!(f, "u8"),
            IntSizes::U16 => write!(f, "u16"),
            IntSizes::U32 => write!(f, "u32"),
            IntSizes::U64 => write!(f, "u64"),
            IntSizes::Usize => write!(f, "usize"),
        }
    }
}

impl TryFrom<&str> for IntSizes {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i8" => Ok(IntSizes::I8),
            "i16" => Ok(IntSizes::I16),
            "i32" => Ok(IntSizes::I32),
            "i64" => Ok(IntSizes::I64),
            "isize" => Ok(IntSizes::Isize),
            "u8" => Ok(IntSizes::U8),
            "u16" => Ok(IntSizes::U16),
            "u32" => Ok(IntSizes::U32),
            "u64" => Ok(IntSizes::U64),
            "usize" => Ok(IntSizes::Usize),
            _ => Err(()),
        }
    }
}

impl TryFrom<&str> for FloatSizes {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "f32" => Ok(FloatSizes::F32),
            "f64" => Ok(FloatSizes::F64),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Primitive {
    Bool(bool),
    Int { value: Numeral, size: Option<IntSizes> },
    Float { value: f64, size: Option<FloatSizes> },
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Bool(b) => write!(f, "{b}"),
            Primitive::Float { value, size } => write!(
                f,
                "{}{}",
                value,
                size.as_ref().map(|s| s.to_string()).unwrap_or_default()
            ),
            Primitive::Int { value, size } => write!(
                f,
                "{}{}",
                value,
                size.as_ref().map(|s| s.to_string()).unwrap_or_default()
            ),
        }
    }
}

impl<'tok> IntoToken<'tok> for Primitive {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token<'tok> {
        Token::new(Kind::Value(Value::Primitive(self)), (start_byte, end_byte).into())
    }
}
