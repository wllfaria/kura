use miette::Error;

use super::{
    operator::Operator,
    token::{IntoToken, Token},
    value::Value,
};

#[derive(Debug, PartialEq)]
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

impl std::fmt::Display for Kind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Value(val) => write!(f, "{}", val),
            Kind::Op(op) => write!(f, "{}", op),
            Kind::Var => write!(f, "var"),
            Kind::Const => write!(f, "const"),
            Kind::Match => write!(f, "match"),
            Kind::If => write!(f, "if"),
            Kind::Else => write!(f, "else"),
            Kind::Fun => write!(f, "fun"),
            Kind::Struct => write!(f, "struct"),
            Kind::Enum => write!(f, "enum"),
            Kind::Return => write!(f, "return"),
            Kind::Eof => write!(f, "eof"),
        }
    }
}

impl<'tok> Kind<'tok> {
    pub fn infix_precedence(&self) -> Result<(u8, u8), Error> {
        match self {
            Kind::Op(Operator::Plus) | Kind::Op(Operator::Minus) => Ok((1, 2)),
            Kind::Op(Operator::Less)
            | Kind::Op(Operator::LessEqual)
            | Kind::Op(Operator::EqualEqual)
            | Kind::Op(Operator::Or)
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
                | Kind::Op(Operator::And)
                | Kind::Op(Operator::Or)
                | Kind::Op(Operator::Plus)
                | Kind::Op(Operator::Minus)
                | Kind::Op(Operator::Greater)
                | Kind::Op(Operator::EqualEqual)
                | Kind::Op(Operator::Less)
                | Kind::Op(Operator::LessEqual)
                | Kind::Op(Operator::NotEqual)
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
