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
