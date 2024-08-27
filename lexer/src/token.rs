use core::fmt;
use miette::Error;

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
pub enum SignedIntSizes {
    I8,
    I16,
    I32,
    I64,
    Isize,
}

impl fmt::Display for SignedIntSizes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SignedIntSizes::I8 => write!(f, "i8"),
            SignedIntSizes::I16 => write!(f, "i16"),
            SignedIntSizes::I32 => write!(f, "i32"),
            SignedIntSizes::I64 => write!(f, "i64"),
            SignedIntSizes::Isize => write!(f, "isize"),
        }
    }
}

#[derive(Debug)]
pub enum UnsignedIntSizes {
    U8,
    U16,
    U32,
    U64,
    Usize,
}

impl fmt::Display for UnsignedIntSizes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnsignedIntSizes::U8 => write!(f, "u8"),
            UnsignedIntSizes::U16 => write!(f, "u16"),
            UnsignedIntSizes::U32 => write!(f, "u32"),
            UnsignedIntSizes::U64 => write!(f, "u64"),
            UnsignedIntSizes::Usize => write!(f, "usize"),
        }
    }
}

#[derive(Debug)]
pub enum NumSize {
    Float(FloatSizes),
    SignedInt(SignedIntSizes),
    UnsignedInt(UnsignedIntSizes),
}

impl std::fmt::Display for NumSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NumSize::Float(size) => size.to_string(),
                NumSize::SignedInt(size) => size.to_string(),
                NumSize::UnsignedInt(size) => size.to_string(),
            }
        )
    }
}

impl TryFrom<&str> for NumSize {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "i8" => Ok(NumSize::SignedInt(SignedIntSizes::I8)),
            "i16" => Ok(NumSize::SignedInt(SignedIntSizes::I16)),
            "i32" => Ok(NumSize::SignedInt(SignedIntSizes::I32)),
            "i64" => Ok(NumSize::SignedInt(SignedIntSizes::I64)),
            "isize" => Ok(NumSize::SignedInt(SignedIntSizes::Isize)),
            "u8" => Ok(NumSize::UnsignedInt(UnsignedIntSizes::U8)),
            "u16" => Ok(NumSize::UnsignedInt(UnsignedIntSizes::U16)),
            "u32" => Ok(NumSize::UnsignedInt(UnsignedIntSizes::U32)),
            "u64" => Ok(NumSize::UnsignedInt(UnsignedIntSizes::U64)),
            "usize" => Ok(NumSize::UnsignedInt(UnsignedIntSizes::Usize)),
            "f8" => Ok(NumSize::Float(FloatSizes::F8)),
            "f16" => Ok(NumSize::Float(FloatSizes::F16)),
            "f32" => Ok(NumSize::Float(FloatSizes::F32)),
            "f64" => Ok(NumSize::Float(FloatSizes::F64)),
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
    MinusAssign,
    String(&'tok str),
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Plus,
    AddAssign,
    Equal,
    ThickArrow,
    EqualEqual,
    Star,
    MultiplyAssign,
    Ampersand,
    Slash,
    DivideAssign,
    Colon,
    SemiColon,
    Bang,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    NotEqual,
    Var,
    Const,
    Match,
    If,
    Else,
    Fun,
    Struct,
    Enum,
    Return,
    Ident(&'tok str),
    Eof,
}

impl Token<'_> {
    pub fn is_binary_op(&self) -> bool {
        matches!(
            self,
            Token::Star
                | Token::Slash
                | Token::Plus
                | Token::Minus
                | Token::Greater
                | Token::Less
                | Token::LessEqual
                | Token::GreaterEqual
        )
    }

    pub fn infix_precedence(&self) -> Result<(u8, u8), Error> {
        match self {
            Token::Plus | Token::Minus => Ok((1, 2)),
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => Ok((3, 4)),
            Token::Star | Token::Slash => Ok((5, 6)),
            _ => miette::bail!("invalid operator with no precedence"),
        }
    }
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
            Token::MinusAssign => write!(f, "-="),
            Token::Colon => write!(f, ":"),
            Token::SemiColon => write!(f, ";"),
            Token::String(s) => write!(f, "{s}"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Plus => write!(f, "+"),
            Token::AddAssign => write!(f, "+="),
            Token::Equal => write!(f, "="),
            Token::ThickArrow => write!(f, "=>"),
            Token::EqualEqual => write!(f, "=="),
            Token::Star => write!(f, "*"),
            Token::MultiplyAssign => write!(f, "*="),
            Token::Ampersand => write!(f, "&"),
            Token::Slash => write!(f, "/"),
            Token::Bang => write!(f, "!"),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::NotEqual => write!(f, "!="),
            Token::DivideAssign => write!(f, "/="),
            Token::Var => write!(f, "var"),
            Token::Const => write!(f, "const"),
            Token::Match => write!(f, "match"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::Fun => write!(f, "fun"),
            Token::Struct => write!(f, "struct"),
            Token::Enum => write!(f, "enum"),
            Token::Ident(str) => write!(f, "{str}"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

impl<'tok> Token<'tok> {
    pub fn identifier_from(value: &'tok str) -> Token<'tok> {
        match value {
            "var" => Token::Var,
            "const" => Token::Const,
            "match" => Token::Match,
            "if" => Token::If,
            "else" => Token::Else,
            "fun" => Token::Fun,
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "return" => Token::Return,
            _ => Token::Ident(value),
        }
    }
}
