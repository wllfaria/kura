use std::fmt;

use super::{
    kind::Kind,
    token::{IntoToken, Token},
};

#[derive(Debug)]
pub enum Operator {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    MinusEqual,
    Plus,
    PlusEqual,
    Equal,
    ThickArrow,
    EqualEqual,
    Star,
    StarEqual,
    Ampersand,
    Slash,
    SlashEqual,
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
            Operator::MinusEqual => write!(f, "-="),
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
            Operator::PlusEqual => write!(f, "+="),
            Operator::Equal => write!(f, "="),
            Operator::ThickArrow => write!(f, "=>"),
            Operator::EqualEqual => write!(f, "=="),
            Operator::Star => write!(f, "*"),
            Operator::StarEqual => write!(f, "*="),
            Operator::Ampersand => write!(f, "&"),
            Operator::Slash => write!(f, "/"),
            Operator::Bang => write!(f, "!"),
            Operator::Less => write!(f, "<"),
            Operator::LessEqual => write!(f, "<="),
            Operator::Greater => write!(f, ">"),
            Operator::GreaterEqual => write!(f, ">="),
            Operator::NotEqual => write!(f, "!="),
            Operator::SlashEqual => write!(f, "/="),
            Operator::And => write!(f, "&&"),
            Operator::Or => write!(f, "||"),
        }
    }
}

impl<'tok> IntoToken<'tok> for Operator {
    fn into_token(self, start_byte: usize, end_byte: usize) -> Token<'tok> {
        Token::new(Kind::Op(self), (start_byte, end_byte).into())
    }
}
