use lexer::{
    token::{FloatSizes, IntSizes, Kind, Location, Operator, Primitive, Token, UIntSizes, Value},
    Lexer,
};

use miette::{Error, LabeledSpan, SourceSpan};

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    Or,
    And,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

impl TryFrom<&Token<'_>> for BinaryOperator {
    type Error = Error;

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.kind {
            Kind::Op(Operator::Plus) => Ok(BinaryOperator::Add),
            Kind::Op(Operator::Minus) => Ok(BinaryOperator::Sub),
            Kind::Op(Operator::Star) => Ok(BinaryOperator::Mul),
            Kind::Op(Operator::Slash) => Ok(BinaryOperator::Div),
            Kind::Op(Operator::EqualEqual) => Ok(BinaryOperator::EqualEqual),
            Kind::Op(Operator::And) => Ok(BinaryOperator::And),
            Kind::Op(Operator::Or) => Ok(BinaryOperator::Or),
            Kind::Op(Operator::Less) => Ok(BinaryOperator::Less),
            Kind::Op(Operator::LessEqual) => Ok(BinaryOperator::LessEqual),
            Kind::Op(Operator::Greater) => Ok(BinaryOperator::Greater),
            Kind::Op(Operator::GreaterEqual) => Ok(BinaryOperator::GreaterEqual),
            _ => miette::bail!("invalid binary operator"),
        }
    }
}

#[derive(Debug)]
pub enum Expression<'ast> {
    Var {
        name: &'ast str,
        value: Box<Expression<'ast>>,
        location: Location,
    },
    If {
        condition: Box<Expression<'ast>>,
        location: Location,
        body: Vec<Expression<'ast>>,
    },
    Ident {
        name: &'ast str,
        location: Location,
    },
    Const {
        name: &'ast str,
        value: Box<Expression<'ast>>,
        location: Location,
    },
    UintLiteral {
        value: u64,
        size: Option<UIntSizes>,
        location: Location,
    },
    FloatLiteral {
        value: f64,
        size: Option<FloatSizes>,
        location: Location,
    },
    IntLiteral {
        value: i64,
        size: Option<IntSizes>,
        location: Location,
    },
    BinaryOp {
        operator: BinaryOperator,
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
        location: Location,
    },
}

impl Expression<'_> {
    fn location(&self) -> Location {
        match self {
            Expression::Var { location, .. } => *location,
            Expression::If { location, .. } => *location,
            Expression::Ident { location, .. } => *location,
            Expression::Const { location, .. } => *location,
            Expression::UintLiteral { location, .. } => *location,
            Expression::FloatLiteral { location, .. } => *location,
            Expression::IntLiteral { location, .. } => *location,
            Expression::BinaryOp { location, .. } => *location,
        }
    }
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

    fn parse_value(&mut self) -> Result<Expression<'par>, Error> {
        match self.lexer.peek() {
            Some(result) => match result {
                Ok(token) => match token {
                    Token {
                        kind: Kind::Value(Value::Primitive(Primitive::Int { .. })),
                        ..
                    } => self.parse_expression_with_precedence(0),
                    Token {
                        kind: Kind::Value(Value::Primitive(Primitive::Float { .. })),
                        ..
                    } => self.parse_expression_with_precedence(0),
                    Token {
                        kind: Kind::Value(Value::Primitive(Primitive::UInt { .. })),
                        ..
                    } => self.parse_expression_with_precedence(0),
                    _ => todo!(),
                },
                Err(_) => todo!(),
            },
            None => todo!(),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression<'par>, Error> {
        match self.lexer.peek() {
            Some(result) => match result {
                Ok(token) => match token {
                    Token {
                        kind: Kind::Value(_),
                        ..
                    } => self.parse_value(),
                    t => todo!("{t:?}"),
                },
                Err(_) => todo!(),
            },
            None => todo!(),
        }
    }

    fn peek_for_binary_operator(&mut self) -> Result<Option<&Token<'par>>, Error> {
        match self.lexer.peek() {
            Some(result) => match result {
                Ok(token) if token.kind.is_binary_op() => Ok(Some(token)),
                Ok(_) => Ok(None),
                Err(_) => Ok(None),
            },

            None => Ok(None),
        }
    }

    fn parse_expression_with_precedence(
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
                location: Location::new(lhs.location().start_byte, rhs.location().end_byte),
                operator: op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn parse_base_expression(&mut self) -> Result<Expression<'par>, Error> {
        match self.lexer.next().transpose()? {
            Some(Token {
                kind: Kind::Value(Value::Primitive(Primitive::Float { value, .. })),
                location,
                ..
            }) => Ok(Expression::FloatLiteral {
                value,
                size: None,
                location,
            }),
            Some(Token {
                kind: Kind::Value(Value::Primitive(Primitive::Int { value, .. })),
                location,
                ..
            }) => Ok(Expression::IntLiteral {
                value,
                size: None,
                location,
            }),
            Some(Token {
                kind: Kind::Value(Value::Primitive(Primitive::UInt { value, .. })),
                location,
                ..
            }) => Ok(Expression::UintLiteral {
                value,
                size: None,
                location,
            }),
            Some(Token {
                kind: Kind::Value(Value::Ident(name)),
                location,
                ..
            }) => Ok(Expression::Ident { name, location }),
            _ => todo!(),
        }
    }

    fn parse_statement(&mut self) -> Result<Expression<'par>, Error> {
        match self.lexer.peek() {
            Some(Ok(token)) => match token {
                Token {
                    kind: Kind::Var, ..
                } => self.parse_variable(),
                Token {
                    kind: Kind::Const, ..
                } => self.parse_constant(),
                Token { kind: Kind::If, .. } => self.parse_if(),
                t => todo!("{t:?}"),
            },
            _ => todo!(),
        }
    }

    fn assert_keyword(&mut self, assert_kind: Kind) -> Result<Token<'par>, Error> {
        let keyword = match self.lexer.next().transpose()? {
            Some(token) => match token {
                Token { ref kind, .. } if kind == &assert_kind => token,
                t => {
                    return Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(SourceSpan::from(t.location), "this keyword"),
                        ],
                        help = format!("this should be a `{assert_kind}`"),
                        "called parse_constant with an invalid token, expected `{assert_kind}`, found {t}",
                    }
                    .with_source_code(self.source.to_string()));
                }
            },
            _ => unreachable!(),
        };

        Ok(keyword)
    }

    fn parse_if(&mut self) -> Result<Expression<'par>, Error> {
        let keyword = self.assert_keyword(Kind::If)?;
        self.assert_keyword(Kind::Op(Operator::LeftParen))?;

        let condition = self.parse_expression_with_precedence(0)?;

        self.assert_keyword(Kind::Op(Operator::RightParen))?;
        self.assert_keyword(Kind::Op(Operator::LeftBrace))?;

        let mut body = vec![];

        let mut open_braces = 1;

        loop {
            match self.lexer.peek() {
                Some(Ok(Token {
                    kind: Kind::Op(Operator::RightBrace),
                    ..
                })) => open_braces -= 1,
                Some(Ok(Token {
                    kind: Kind::Op(Operator::LeftBrace),
                    ..
                })) => open_braces += 1,
                _ => (),
            }

            if open_braces == 0 {
                break;
            }

            let statement = self.parse_statement()?;
            body.push(statement);
        }

        self.assert_keyword(Kind::Op(Operator::RightBrace))?;

        let location = keyword.location.start_byte..condition.location().end_byte;
        Ok(Expression::If {
            condition: Box::new(condition),
            location: location.into(),
            body,
        })
    }

    fn parse_identifier(&mut self) -> Result<Expression<'par>, Error> {
        let Some(identifier) = self.lexer.next().transpose()? else {
            return Err(
                miette::miette!("temporary error").with_source_code(self.source.to_string())
            );
        };

        match identifier {
            Token {
                kind: Kind::Value(Value::Ident(name)),
                ..
            } => Ok(Expression::Ident { name, location: identifier.location } ),
            _ => Err(miette::miette! {
                labels = vec![LabeledSpan::at(SourceSpan::from(identifier.location), "this expression")],
                help = format!("We expected an identifier, but found `{identifier}`"),
                "invalid identifier",
            }
            .with_source_code(self.source.to_string())),
        }
    }

    fn parse_assign(&mut self) -> Result<(), Error> {
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
        };

        Ok(())
    }

    fn assert_semicolon(&mut self, start: usize, end: usize) -> Result<(), Error> {
        let location = start..end;
        let next = self.lexer.next().transpose()?;
        if next.is_none()
            || next.is_some_and(|token| {
                !matches!(
                    token,
                    Token {
                        kind: Kind::Op(Operator::SemiColon),
                        ..
                    }
                )
            })
        {
            return Err(miette::miette! {
                labels = vec![
                    LabeledSpan::at(location, "this expression"),
                ],
                help = "you might have forgotten a `;` semicolon",
                "Unterminated expression",
            }
            .with_source_code(self.source.to_string()));
        }

        Ok(())
    }

    fn parse_constant(&mut self) -> Result<Expression<'par>, Error> {
        let keyword = self.assert_keyword(Kind::Const)?;

        let name = match self.parse_identifier()? {
            Expression::Ident { name, .. } => name,
            _ => unreachable!(),
        };

        self.parse_assign()?;

        let expression = self.parse_expression()?;
        let expression = Expression::Const {
            location: expression.location(),
            name,
            value: Box::new(expression),
        };

        let (start, end) = (keyword.location.start_byte, expression.location().end_byte);
        self.assert_semicolon(start, end)?;

        Ok(expression)
    }

    fn parse_variable(&mut self) -> Result<Expression<'par>, Error> {
        let keyword = self.lexer.next().transpose()?.expect("");

        let name = match self.parse_identifier()? {
            Expression::Ident { name, .. } => name,
            _ => unreachable!(),
        };

        self.parse_assign()?;

        let expression = self.parse_expression()?;
        let expression = Expression::Var {
            location: expression.location(),
            name,
            value: Box::new(expression),
        };

        let (start, end) = (keyword.location.start_byte, expression.location().end_byte);
        self.assert_semicolon(start, end)?;

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
            "var var_name = 1 + 2 * 3 - 4;",
            "var another_name = 123.4;",
            "const const_name = 1.23 + 4.56 * 7.89;",
            "if (const_name == another_name || var_name == const_name) {",
            "   const my_new_const = 123.0;",
            "   if (my_new_const == another_name) {",
            "       const yet_another = 123.0;",
            "   }",
            "}",
        ];
        let source = source.join("\n");

        let result = match make_sut(&source).parse() {
            Ok(result) => result,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(result);
    }
}
