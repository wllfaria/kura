use core::fmt;

use super::{
    kind::Kind,
    token::{IntoToken, Token},
    value::Value,
};

#[derive(Debug)]
pub enum FloatSizes {
    F8,
    F16,
    F32,
    F64,
}

impl fmt::Display for FloatSizes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatSizes::F8 => write!(f, "f8"),
            FloatSizes::F16 => write!(f, "f16"),
            FloatSizes::F32 => write!(f, "f32"),
            FloatSizes::F64 => write!(f, "f64"),
        }
    }
}

#[derive(Debug)]
pub enum IntSizes {
    I8,
    I16,
    I32,
    I64,
    Isize,
}

impl fmt::Display for IntSizes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntSizes::I8 => write!(f, "i8"),
            IntSizes::I16 => write!(f, "i16"),
            IntSizes::I32 => write!(f, "i32"),
            IntSizes::I64 => write!(f, "i64"),
            IntSizes::Isize => write!(f, "isize"),
        }
    }
}

#[derive(Debug)]
pub enum UIntSizes {
    U8,
    U16,
    U32,
    U64,
    Usize,
}

impl fmt::Display for UIntSizes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UIntSizes::U8 => write!(f, "u8"),
            UIntSizes::U16 => write!(f, "u16"),
            UIntSizes::U32 => write!(f, "u32"),
            UIntSizes::U64 => write!(f, "u64"),
            UIntSizes::Usize => write!(f, "usize"),
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
            _ => Err(()),
        }
    }
}

impl TryFrom<&str> for FloatSizes {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "f8" => Ok(FloatSizes::F8),
            "f16" => Ok(FloatSizes::F16),
            "f32" => Ok(FloatSizes::F32),
            "f64" => Ok(FloatSizes::F64),
            _ => Err(()),
        }
    }
}

impl TryFrom<&str> for UIntSizes {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "u8" => Ok(UIntSizes::U8),
            "u16" => Ok(UIntSizes::U16),
            "u32" => Ok(UIntSizes::U32),
            "u64" => Ok(UIntSizes::U64),
            "usize" => Ok(UIntSizes::Usize),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum Primitive {
    Bool(bool),
    UInt {
        value: u64,
        size: Option<UIntSizes>,
    },
    Int {
        value: i64,
        size: Option<IntSizes>,
    },
    Float {
        value: f64,
        size: Option<FloatSizes>,
    },
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Bool(b) => write!(f, "{b}"),
            Primitive::UInt { value, size } => write!(
                f,
                "{}{}",
                value,
                size.as_ref().map(|s| s.to_string()).unwrap_or_default()
            ),
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
        Token::new(
            Kind::Value(Value::Primitive(self)),
            (start_byte, end_byte).into(),
        )
    }
}
