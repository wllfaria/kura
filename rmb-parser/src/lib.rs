mod expression;

use rmb_lexer::{
    token::{FloatSizes, IntSizes, Kind, Location, Operator, Token, UIntSizes, Value},
    Lexer, TransposeRef,
};
use std::fmt;

use crate::expression::{parse_expression, parse_identifier};
use miette::{Error, LabeledSpan, SourceSpan};

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
    Fun {
        name: &'ast str,
        arguments: Vec<Expression<'ast>>,
        body: Vec<Expression<'ast>>,
        return_type: Option<Box<Expression<'ast>>>,
        location: Location,
    },
    FunArgument {
        name: &'ast str,
        arg_type: Box<Expression<'ast>>,
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

impl<'ast> Expression<'ast> {
    fn fmt_with_indentation(&self, f: &mut fmt::Formatter<'_>, indent_level: usize) -> fmt::Result {
        let indent = " ".repeat(indent_level * 4);

        match self {
            Expression::Var {
                mutable,
                name,
                value,
                ..
            } => {
                let kind = if *mutable { "var" } else { "const" };
                write!(f, "{indent}{kind} {name} = {value};")
            }
            Expression::Bool { value, .. } => write!(f, "{value}"),
            Expression::If { .. } => todo!(),
            Expression::Ident { name, .. } => write!(f, "{name}"),
            Expression::UintLiteral { value, size, .. } => {
                let size = size.as_ref().map(|s| s.to_string()).unwrap_or_default();
                write!(f, "{value}{size}")
            }
            Expression::Fun {
                name,
                arguments,
                body,
                return_type,
                ..
            } => {
                write!(f, "{indent}fun {name}(")?;
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")?;
                if let Some(ret_type) = return_type {
                    write!(f, " => {}", ret_type)?;
                }
                writeln!(f, " {{")?;
                for (i, expr) in body.iter().enumerate() {
                    expr.fmt_with_indentation(f, indent_level + 1)?;

                    if i < body.len() {
                        writeln!(f)?;
                    }
                }
                writeln!(f, "{}}}", indent)
            }
            Expression::Return { value, .. } => write!(f, "{indent}return {value};"),
            Expression::FunArgument { name, arg_type, .. } => write!(f, "{name}: {arg_type}"),
            Expression::FloatLiteral { value, size, .. } => {
                let size = size.as_ref().map(|s| s.to_string()).unwrap_or_default();
                write!(f, "{value}{size}")
            }
            Expression::IntLiteral { value, size, .. } => {
                let size = size.as_ref().map(|s| s.to_string()).unwrap_or_default();
                write!(f, "{value}{size}")
            }
            Expression::BinaryOp {
                operator, lhs, rhs, ..
            } => {
                write!(f, "({operator} ")?;
                write!(f, "{lhs} ")?;
                write!(f, "{rhs})")
            }
        }
    }
}

impl fmt::Display for Expression<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indentation(f, 0)
    }
}

impl Expression<'_> {
    fn location(&self) -> Location {
        match self {
            Expression::Var { location, .. } => *location,
            Expression::If { location, .. } => *location,
            Expression::Ident { location, .. } => *location,
            Expression::Fun { location, .. } => *location,
            Expression::FunArgument { location, .. } => *location,
            Expression::Bool { location, .. } => *location,
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

    fn parse_statement(&mut self) -> Result<Expression<'par>, Error> {
        match self.lexer.peek().transpose()? {
            Some(token) => match &token.kind {
                Kind::Fun => self.parse_function(),
                t => todo!("{t:?}"),
            },
            _ => todo!(),
        }
    }

    fn parse_function(&mut self) -> Result<Expression<'par>, Error> {
        let keyword = self.lexer.expect(Kind::Fun)?;
        let (_, fun_name) = parse_identifier(&mut self.lexer)?;
        self.lexer.expect(Kind::Op(Operator::LeftParen))?;

        let mut arguments = vec![];

        loop {
            let (arg_name_expr, arg_name) = parse_identifier(&mut self.lexer)?;
            self.lexer.expect(Kind::Op(Operator::Colon))?;
            let (arg_type, _) = parse_identifier(&mut self.lexer)?;

            arguments.push(Expression::FunArgument {
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
                        let location = keyword.location.start_byte..token.location.end_byte;
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

        // and also the opening brace
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
        Ok(Expression::Fun {
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
        let source = [
            "fun calculate_circumference(diameter: f64) => f64 {",
            "    const pi = 3.14159265358979323846264338327950288_f32;",
            "    const radius = diameter / 2.0;",
            "    const circumference = 2.0 * pi * radius;",
            "",
            "    return circumference;",
            "}",
        ];

        let expected = [
            "fun calculate_circumference(diameter: f64) => f64 {",
            "    const pi = 3.141592653589793f32;",
            "    const radius = (/ diameter 2);",
            "    const circumference = (* (* 2 pi) radius);",
            "    return circumference;",
            "}\n",
        ];
        let source = source.join("\n");
        let expected = expected.join("\n");

        let ast = match make_sut(&source).parse() {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };
        let fun_expr = &ast[0];

        insta::assert_debug_snapshot!(ast);
        insta::assert_snapshot!(fun_expr);

        assert_eq!(fun_expr.to_string(), expected);
    }
}
