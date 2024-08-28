use miette::Error;

use super::{
    operator::Operator,
    token::{IntoToken, Token},
    value::Value,
};

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
