use lexer::{FloatSizes, IntSizes, Kind, Lexer, Operator, Primitive, Token, UIntSizes, Value};

use miette::{Error, LabeledSpan};

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

impl TryFrom<&Token<'_>> for BinaryOperator {
    type Error = Error;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind {
            Kind::Op(Operator::Plus) => Ok(BinaryOperator::Add),
            Kind::Op(Operator::Minus) => Ok(BinaryOperator::Sub),
            Kind::Op(Operator::Star) => Ok(BinaryOperator::Mul),
            Kind::Op(Operator::Slash) => Ok(BinaryOperator::Div),
            _ => miette::bail!("invalid binary operator"),
        }
    }
}

#[derive(Debug)]
pub enum Expression<'ast> {
    VarNode {
        name: &'ast str,
        value: Box<Expression<'ast>>,
    },
    UintLiteral {
        value: u64,
        size: Option<UIntSizes>,
    },
    FloatLiteral {
        value: f64,
        size: Option<FloatSizes>,
    },
    IntLiteral {
        value: i64,
        size: Option<IntSizes>,
    },
    BinaryOp {
        operator: BinaryOperator,
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
    },
}

pub struct Parser<'par> {
    source: &'par str,
    lexer: Lexer<'par>,
}

impl<'par> Parser<'par> {
    pub fn new(input: &'par str) -> Self {
        Self {
            source: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Expression<'par>>, Error> {
        let mut statements = vec![];

        loop {
            if self.lexer.is_empty() {
                break;
            }

            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(statements)
    }

    pub fn parse_expression(&mut self) -> Result<Expression<'par>, Error> {
        self.parse_expression_with_precedence(0)
    }

    pub fn peek_for_binary_operator(&mut self) -> Result<Option<&Token<'par>>, Error> {
        match self.lexer.peek() {
            Some(result) => match result {
                Ok(token) if token.kind.is_binary_op() => Ok(Some(token)),
                Ok(_) => Ok(None),
                Err(_) => Ok(None),
            },

            None => Ok(None),
        }
    }

    pub fn parse_expression_with_precedence(
        &mut self,
        min_precedence: u8,
    ) -> Result<Expression<'par>, Error> {
        let mut lhs = self.parse_base_expression()?;

        while let Some(op) = self.peek_for_binary_operator()? {
            let (lhs_precedence, rhs_precedence) = op.kind.infix_precedence()?;
            if lhs_precedence < min_precedence {
                break;
            }

            let op = op.try_into()?;
            self.lexer.next().transpose()?;

            let rhs = self.parse_expression_with_precedence(rhs_precedence)?;

            lhs = Expression::BinaryOp {
                operator: op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    pub fn parse_base_expression(&mut self) -> Result<Expression<'par>, Error> {
        match self.lexer.next().transpose()? {
            Some(Token {
                kind: Kind::Value(Value::Primitive(Primitive::Float { value, .. })),
                ..
            }) => Ok(Expression::FloatLiteral { value, size: None }),
            Some(Token {
                kind: Kind::Value(Value::Primitive(Primitive::Int { value, .. })),
                ..
            }) => Ok(Expression::IntLiteral { value, size: None }),
            Some(Token {
                kind: Kind::Value(Value::Primitive(Primitive::UInt { value, .. })),
                ..
            }) => Ok(Expression::UintLiteral { value, size: None }),
            _ => todo!(),
        }
    }

    pub fn parse_statement(&mut self) -> Result<Expression<'par>, Error> {
        match self.lexer.next().transpose()? {
            Some(Token {
                kind: Kind::Value(Value::Primitive(Primitive::Float { value, .. })),
                ..
            }) => Ok(Expression::FloatLiteral { value, size: None }),
            Some(Token {
                kind: Kind::Value(Value::Primitive(Primitive::Int { value, .. })),
                ..
            }) => Ok(Expression::IntLiteral { value, size: None }),
            Some(Token {
                kind: Kind::Value(Value::Primitive(Primitive::UInt { value, .. })),
                ..
            }) => Ok(Expression::UintLiteral { value, size: None }),
            Some(Token {
                kind: Kind::Var, ..
            }) => self.parse_variable(),
            t => todo!("{t:?}"),
        }
    }

    pub fn parse_variable(&mut self) -> Result<Expression<'par>, Error> {
        let Some(name) = self.lexer.next().transpose()? else {
            return Err(
                miette::miette!("temporary error").with_source_code(self.source.to_string())
            );
        };

        let name = match name {
            Token {
                kind: Kind::Value(Value::Ident(name)),
                ..
            } => name,
            _ => panic!("invalid token where name of var should be"),
        };

        let Some(assign) = self.lexer.next().transpose()? else {
            return Err(
                miette::miette!("temporary error").with_source_code(self.source.to_string())
            );
        };

        if !matches!(
            assign,
            Token {
                kind: Kind::Op(Operator::Equal),
                ..
            }
        ) {
            return Err(
                miette::miette!("temporary error").with_source_code(self.source.to_string())
            );
        }

        let expression = Expression::VarNode {
            name,
            value: Box::new(self.parse_expression()?),
        };

        if self.lexer.next().transpose()?.is_some_and(|token| {
            !matches!(
                token,
                Token {
                    kind: Kind::Op(Operator::SemiColon),
                    ..
                }
            )
        }) {
            return Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(0..3, "this expression"),
                ],
                help = "you might have forgotten a `;` semicolon",
                "Unterminated expression",
            }
            .with_source_code(self.source.to_string()));
        }

        Ok(expression)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_sut(source: &str) -> Parser<'_> {
        Parser::new(source)
    }

    #[test]
    fn tests() {
        let source = [
            "var some_name = 1 + 2 * 3 - 4;",
            "var another_name = 123.4;",
        ];
        let source = source.join("\n");

        let result = make_sut(&source).parse().unwrap();

        insta::assert_debug_snapshot!(result);
    }
}
