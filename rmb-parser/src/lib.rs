mod expression;

use rmb_lexer::{
    token::{FloatSizes, IntSizes, Kind, Location, Operator, Token, UIntSizes},
    Lexer, TransposeRef,
};

use crate::expression::{parse_expression, parse_identifier};
use miette::{Error, LabeledSpan, SourceSpan};

#[derive(Debug)]
pub enum Statement<'ast> {
    FunArgument {
        name: &'ast str,
        arg_type: Box<Expression<'ast>>,
        location: Location,
    },
    Fun {
        name: &'ast str,
        arguments: Vec<Statement<'ast>>,
        body: Vec<Expression<'ast>>,
        return_type: Option<Box<Expression<'ast>>>,
        location: Location,
    },
}

#[derive(Debug)]
pub enum Expression<'ast> {
    Var {
        mutable: bool,
        name: &'ast str,
        value: Box<Expression<'ast>>,
        location: Location,
    },
    Bool {
        value: bool,
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
    Block {
        expressions: Vec<Expression<'ast>>,
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
        operator: Operator,
        lhs: Box<Expression<'ast>>,
        rhs: Box<Expression<'ast>>,
        location: Location,
    },
    Return {
        value: Box<Expression<'ast>>,
        location: Location,
    },
}

impl Expression<'_> {
    fn location(&self) -> Location {
        match self {
            Expression::Var { location, .. } => *location,
            Expression::If { location, .. } => *location,
            Expression::Ident { location, .. } => *location,
            Expression::Bool { location, .. } => *location,
            Expression::Block { location, .. } => *location,
            Expression::UintLiteral { location, .. } => *location,
            Expression::Return { location, .. } => *location,
            Expression::FloatLiteral { location, .. } => *location,
            Expression::IntLiteral { location, .. } => *location,
            Expression::BinaryOp { location, .. } => *location,
        }
    }
}

pub struct Parser<'par> {
    pub source: &'par str,
    pub lexer: Lexer<'par>,
}

impl<'par> Parser<'par> {
    pub fn new(input: &'par str) -> Self {
        Self {
            source: input,
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Statement<'par>>, Error> {
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

    fn parse_statement(&mut self) -> Result<Statement<'par>, Error> {
        match self.lexer.peek().transpose()? {
            Some(token) => match &token.kind {
                Kind::Fun => self.parse_function(),
                t => todo!("{t:?}"),
            },
            _ => todo!(),
        }
    }

    fn parse_function_args(&mut self, start_kw: &Token<'_>) -> Result<Vec<Statement<'par>>, Error> {
        let mut arguments = vec![];

        loop {
            let (arg_name_expr, arg_name) = parse_identifier(&mut self.lexer)?;
            self.lexer.expect(Kind::Op(Operator::Colon))?;
            let (arg_type, _) = parse_identifier(&mut self.lexer)?;

            arguments.push(Statement::FunArgument {
                name: arg_name,
                location: Location::new(
                    arg_name_expr.location().start_byte,
                    arg_type.location().end_byte,
                ),
                arg_type: Box::new(arg_type),
            });

            match self.lexer.peek().transpose()? {
                Some(token) => match token.kind {
                    Kind::Op(Operator::RightParen) => break,
                    Kind::Op(Operator::Comma) => (),
                    _ => {
                        let location = start_kw.location.start_byte..token.location.end_byte;
                        return Err(miette::miette! {
                            labels = vec![
                                LabeledSpan::at(SourceSpan::from(location), "this function declaration")
                            ],
                            help = "did you forget to add a comma (,) here?",
                            "syntax error: unexpected identifier after argument type"
                        }
                        .with_source_code(self.source.to_string()));
                    }
                },
                None => break,
            }
        }

        Ok(arguments)
    }

    fn parse_function(&mut self) -> Result<Statement<'par>, Error> {
        let keyword = self.lexer.expect(Kind::Fun)?;
        let (_, fun_name) = parse_identifier(&mut self.lexer)?;
        self.lexer.expect(Kind::Op(Operator::LeftParen))?;

        let arguments = self.parse_function_args(&keyword)?;

        // after parsing argument list we need to consume the closing parenthesis
        self.lexer.expect(Kind::Op(Operator::RightParen))?;

        // after the argument list of a function, there can be an optional return type annotation
        // => <TYPE> {
        // before the left brace, but its fine to be ommited
        let has_return = match self.lexer.peek().transpose()? {
            Some(token) => matches!(token.kind, Kind::Op(Operator::ThickArrow)),
            _ => false,
        };

        let return_type = if has_return {
            self.lexer.next().transpose()?;
            let (identifier, _) = parse_identifier(&mut self.lexer)?;
            Some(Box::new(identifier))
        } else {
            None
        };

        self.lexer.expect(Kind::Op(Operator::LeftBrace))?;

        let mut body = vec![];

        loop {
            match self.lexer.peek().transpose()? {
                Some(token) => {
                    if let Kind::Op(Operator::RightBrace) = token.kind {
                        break;
                    }
                }
                None => break,
            }
            body.push(parse_expression(&mut self.lexer)?);
        }

        // consume the closing brace of the function
        let closing_brace = self.lexer.expect(Kind::Op(Operator::RightBrace))?;

        let location = keyword.location.start_byte..closing_brace.location.end_byte;
        Ok(Statement::Fun {
            name: fun_name,
            arguments,
            body,
            return_type,
            location: location.into(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_sut(source: &str) -> Parser<'_> {
        Parser::new(source)
    }

    #[test]
    fn function_declaration() {
        let source = r#"
            fun calculate_circumference(diameter: f64) => f64 {
                const pi = 3.14159265358979323846264338327950288_f32;
                const radius = diameter / 2.0;
                const circumference = 2.0 * pi * radius;

                const nesting = {
                    const something = 10;
                    var nesting_more = {
                        return 10 + 3 * 4;
                    };
                    return 10 + something;
                };
            
                return circumference;
            }"#;

        let ast = match make_sut(source).parse() {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(ast);
    }
}
