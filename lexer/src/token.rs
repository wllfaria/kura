#[derive(Debug)]
pub enum NumSize {
    F8,
    F16,
    F32,
    F64,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Usize,
    Isize,
}

impl std::fmt::Display for NumSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumSize::F8 => write!(f, "f8"),
            NumSize::F16 => write!(f, "f16"),
            NumSize::F32 => write!(f, "f32"),
            NumSize::F64 => write!(f, "f64"),
            NumSize::I8 => write!(f, "i8"),
            NumSize::I16 => write!(f, "i16"),
            NumSize::I32 => write!(f, "i32"),
            NumSize::I64 => write!(f, "i64"),
            NumSize::U8 => write!(f, "u8"),
            NumSize::U16 => write!(f, "u16"),
            NumSize::U32 => write!(f, "u32"),
            NumSize::U64 => write!(f, "u64"),
            NumSize::Usize => write!(f, "usize"),
            NumSize::Isize => write!(f, "isize"),
        }
    }
}

impl TryFrom<&str> for NumSize {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i8" => Ok(NumSize::I8),
            "i16" => Ok(NumSize::I16),
            "i32" => Ok(NumSize::I32),
            "i64" => Ok(NumSize::I64),
            "u8" => Ok(NumSize::U8),
            "u16" => Ok(NumSize::U16),
            "u32" => Ok(NumSize::U32),
            "u64" => Ok(NumSize::U64),
            "f8" => Ok(NumSize::F8),
            "f16" => Ok(NumSize::F16),
            "f32" => Ok(NumSize::F32),
            "f64" => Ok(NumSize::F64),
            "usize" => Ok(NumSize::Usize),
            "isize" => Ok(NumSize::Isize),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum Token<'tok> {
    Integer { value: u64, size: Option<NumSize> },
    SignedInteger { value: i64, size: Option<NumSize> },
    Float { value: f64, size: Option<NumSize> },
    Minus,
    String(&'tok str),
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Integer { value, size } => write!(
                f,
                "{}{}",
                value,
                size.as_ref().map(|s| s.to_string()).unwrap_or_default()
            ),
            Token::Float { value, size } => write!(
                f,
                "{}{}",
                value,
                size.as_ref().map(|s| s.to_string()).unwrap_or_default()
            ),
            Token::SignedInteger { value, size } => write!(
                f,
                "{}{}",
                value,
                size.as_ref().map(|s| s.to_string()).unwrap_or_default()
            ),
            Token::Minus => write!(f, "-"),
            Token::String(s) => write!(f, "{s}"),
        }
    }
}
