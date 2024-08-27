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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Operator {
    Minus,
    MinusAssign,
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
    And,
    Or,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operator::Minus => write!(f, "-"),
            Operator::MinusAssign => write!(f, "-="),
            Operator::Colon => write!(f, ":"),
            Operator::SemiColon => write!(f, ";"),
            Operator::LeftParen => write!(f, "("),
            Operator::RightParen => write!(f, ")"),
            Operator::LeftBracket => write!(f, "["),
            Operator::RightBracket => write!(f, "]"),
            Operator::LeftBrace => write!(f, "{{"),
            Operator::RightBrace => write!(f, "}}"),
            Operator::Comma => write!(f, ","),
            Operator::Dot => write!(f, "."),
            Operator::Plus => write!(f, "+"),
            Operator::AddAssign => write!(f, "+="),
            Operator::Equal => write!(f, "="),
            Operator::ThickArrow => write!(f, "=>"),
            Operator::EqualEqual => write!(f, "=="),
            Operator::Star => write!(f, "*"),
            Operator::MultiplyAssign => write!(f, "*="),
            Operator::Ampersand => write!(f, "&"),
            Operator::Slash => write!(f, "/"),
            Operator::Bang => write!(f, "!"),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
            Operator::NotEqual => write!(f, "!="),
            Operator::DivideAssign => write!(f, "/="),
            Operator::And => write!(f, "&&"),
            Operator::Or => write!(f, "||"),
        }
    }
}

pub trait IntoToken<'tok> {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token<'tok>;
}

impl<'tok> IntoToken<'tok> for Operator {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token<'tok> {
        Token::new(Kind::Op(self), (start_byte, end_byte).into())
    }
}

#[derive(Debug)]
pub enum Kind<'tok> {
    Value(Value<'tok>),
    Op(Operator),

    Var,
    Const,
    Match,
    If,
    Else,
    Fun,
    Struct,
    Enum,
    Return,
    Eof,
}

impl<'tok> Kind<'tok> {
    pub fn infix_precedence(&self) -> Result<(u8, u8), Error> {
        match self {
            Kind::Op(Operator::Plus) | Kind::Op(Operator::Minus) => Ok((1, 2)),
            Kind::Op(Operator::Less)
            | Kind::Op(Operator::LessEqual)
            | Kind::Op(Operator::Greater)
            | Kind::Op(Operator::GreaterEqual) => Ok((3, 4)),
            Kind::Op(Operator::Star) | Kind::Op(Operator::Slash) => Ok((5, 6)),
            _ => miette::bail!("invalid operator with no precedence"),
        }
    }

    pub fn is_binary_op(&self) -> bool {
        matches!(
            self,
            Kind::Op(Operator::Star)
                | Kind::Op(Operator::Slash)
                | Kind::Op(Operator::Plus)
                | Kind::Op(Operator::Minus)
                | Kind::Op(Operator::Greater)
                | Kind::Op(Operator::Less)
                | Kind::Op(Operator::LessEqual)
                | Kind::Op(Operator::GreaterEqual)
        )
    }

    pub fn identifier_from(value: &'tok str) -> Kind<'tok> {
        match value {
            "var" => Kind::Var,
            "const" => Kind::Const,
            "match" => Kind::Match,
            "if" => Kind::If,
            "else" => Kind::Else,
            "fun" => Kind::Fun,
            "struct" => Kind::Struct,
            "enum" => Kind::Enum,
            "return" => Kind::Return,
            _ => Kind::Value(Value::Ident(value)),
        }
    }
}

impl<'tok> IntoToken<'tok> for Kind<'tok> {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token<'tok> {
        Token::new(self, (start_byte, end_byte).into())
    }
}

#[derive(Debug)]
pub struct Location {
    start_byte: usize,
    end_byte: usize,
}

impl From<(usize, usize)> for Location {
    fn from((start_byte, end_byte): (usize, usize)) -> Self {
        Self {
            start_byte,
            end_byte,
        }
    }
}

#[derive(Debug)]
pub struct Token<'tok> {
    pub kind: Kind<'tok>,
    pub location: Location,
}

impl<'tok> Token<'tok> {
    pub fn new(kind: Kind<'tok>, location: Location) -> Self {
        Self { kind, location }
    }
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            Kind::Value(val) => write!(f, "{}", val),
            Kind::Op(op) => write!(f, "{}", op),
            Kind::Var => write!(f, ""),
            Kind::Const => write!(f, ""),
            Kind::Match => write!(f, ""),
            Kind::If => write!(f, ""),
            Kind::Else => write!(f, ""),
            Kind::Fun => write!(f, ""),
            Kind::Struct => write!(f, ""),
            Kind::Enum => write!(f, ""),
            Kind::Return => write!(f, ""),
            Kind::Eof => write!(f, ""),
        }

        //match self {
        //    Token_::Integer { value, size } => write!(
        //        f,
        //        "{}{}",
        //        value,
        //        size.as_ref().map(|s| s.to_string()).unwrap_or_default()
        //    ),
        //    Token_::Float { value, size } => write!(
        //        f,
        //        "{}{}",
        //        value,
        //        size.as_ref().map(|s| s.to_string()).unwrap_or_default()
        //    ),
        //    Token_::SignedInteger { value, size } => write!(
        //        f,
        //        "{}{}",
        //        value,
        //        size.as_ref().map(|s| s.to_string()).unwrap_or_default()
        //    ),
        //    Token_::Return => write!(f, "return"),
        //    Token_::Fun => write!(f, "fun"),
        //    Token_::Struct => write!(f, "struct"),
        //    Token_::Enum => write!(f, "enum"),
        //    Token_::Ident(str) => write!(f, "{str}"),
        //}
    }
}
