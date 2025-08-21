use core::fmt;
use std::fmt::Display;

use super::kind::Kind;
use super::value::Value;
use super::{IntoToken, Token};

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Signedness {
    Signed,
    Unsigned,
}

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

impl Numeral {
    pub fn signed_inner(&self) -> i64 {
        match self {
            Numeral::Signed(val) => *val,
            Numeral::Unsigned(_) => unreachable!(),
        }
    }

    pub fn signedness(&self) -> Signedness {
        match self {
            Numeral::Signed(_) => Signedness::Signed,
            Numeral::Unsigned(_) => Signedness::Unsigned,
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
pub enum IntSize {
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

impl IntSize {
    pub fn is_unsigned(&self) -> bool {
        self.signedness() == Signedness::Unsigned
    }

    pub fn is_signed(&self) -> bool {
        self.signedness() == Signedness::Signed
    }

    pub fn signedness(&self) -> Signedness {
        match self {
            IntSize::U8 | IntSize::U16 | IntSize::U32 | IntSize::U64 | IntSize::Usize => Signedness::Unsigned,
            IntSize::I8 | IntSize::I16 | IntSize::I32 | IntSize::I64 | IntSize::Isize => Signedness::Signed,
        }
    }
}

impl fmt::Display for IntSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntSize::I8 => write!(f, "i8"),
            IntSize::I16 => write!(f, "i16"),
            IntSize::I32 => write!(f, "i32"),
            IntSize::I64 => write!(f, "i64"),
            IntSize::Isize => write!(f, "isize"),
            IntSize::U8 => write!(f, "u8"),
            IntSize::U16 => write!(f, "u16"),
            IntSize::U32 => write!(f, "u32"),
            IntSize::U64 => write!(f, "u64"),
            IntSize::Usize => write!(f, "usize"),
        }
    }
}

impl TryFrom<&str> for IntSize {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i8" => Ok(IntSize::I8),
            "i16" => Ok(IntSize::I16),
            "i32" => Ok(IntSize::I32),
            "i64" => Ok(IntSize::I64),
            "isize" => Ok(IntSize::Isize),
            "u8" => Ok(IntSize::U8),
            "u16" => Ok(IntSize::U16),
            "u32" => Ok(IntSize::U32),
            "u64" => Ok(IntSize::U64),
            "usize" => Ok(IntSize::Usize),
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Primitive {
    Bool(bool),
    Int { value: Numeral, size: Option<IntSize> },
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
