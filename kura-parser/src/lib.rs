pub mod ast;
mod error;
mod expression;

use ast::{FunArgument, Statement, StructField};
use expression::{parse_expr_block, parse_type_annotation};
use kura_lexer::token::{Kind, Location, Operator, Token};
use kura_lexer::Lexer;
use miette::NamedSource;

use crate::ast::FunStatement;
use crate::error::Error;
use crate::expression::parse_identifier;

/// Consumes the next token from the lexer, returning an error if there is no next token.
#[macro_export]
macro_rules! consume {
    ($lexer:expr) => {
        $lexer.next().transpose()
    };
}

#[macro_export]
macro_rules! expect {
    ($lexer:expr, $kind:expr) => {{
        let Some(token) = $lexer.next().transpose()? else {
            let location = $lexer.eof_location();
            return Err(Error::new(
                location,
                format!("Expected {} but found `EOF`", $kind),
                None,
                NamedSource::new("file.rs", $lexer.source_arc.clone()),
            ).into());
        };

        if token.kind == $kind {
            Ok(token)
        }
        else {
            Err(Error::new(
                token.location,
                format!("expected `{}` but got `{}`", $kind, token.kind),
                None,
                NamedSource::new("file.rs", $lexer.source_arc.clone()),
            ))
        }
    }};

    ($lexer:expr, $first:expr, $($rest:expr),+) => {{
        let expected_list = [$first, $($rest),+];
        let token = $lexer.next().transpose()?;
        let location = token.as_ref().map(|token| token.location);
        let kind = token.as_ref().map(|token| &token.kind);
        let kind = kind.unwrap_or(&Kind::Eof);

        if expected_list.contains(kind) {
            Ok(token.unwrap())
        } else {
            let kinds = expected_list
                .iter()
                .map(|k| k.to_string())
                .collect::<Vec<_>>()
                .join(" ");

            Err(Error::new(
                location.unwrap_or(($lexer.pos, $lexer.pos).into()),
                format!("expected one of {kinds} but got {kind:?}"),
                None,
                NamedSource::new("file.rs", $lexer.source_arc.clone()),
            ))
        }
    }};
}

#[macro_export]
macro_rules! expect_peek {
    ($lexer:expr, $kind:expr) => {{
        let token = peek!($lexer)?;

        if token.is_none() {
            let location = $lexer.source_arc.len() - 1..$lexer.source_arc.len();
            return Err(Error::new(
                location.into(),
                format!("expected `{}` but got `EOF`", $kind),
                None,
                NamedSource::new("file.rs", $lexer.source_arc.clone()),
            )
            .into());
        }

        if token.unwrap().kind != $kind {
            return Err(Error::new(
                token.unwrap().location,
                format!("expected {} but got {}", $kind, token.unwrap().kind),
                None,
                NamedSource::new("file.rs", $lexer.source_arc.clone()),
            )
            .into());
        }
    }};
}

#[macro_export]
macro_rules! peek {
    ($lexer:expr) => {
        $lexer.peek().transpose()
    };
}

#[macro_export]
macro_rules! peek_matches {
    ($lexer:expr, $kind:pat) => {
        {
            if let Some(token) = peek!($lexer)? {
                matches!(token.kind, $kind)
            } else {
                false
            }
        }
    };

    ($lexer:expr, $kind:ident, $($rest:expr),+) => {
        matches!(peek!($lexer).map(|t| t.kind), $kind $(|| peek_matches!($lexer, $($rest),+))?)
    }
}

pub struct Parser<'par> {
    pub source: &'par str,
    pub lexer: Lexer<'par>,
}

impl<'par> Parser<'par> {
    pub fn new(source: &'par str, lexer: Lexer<'par>) -> Self {
        Self { source, lexer }
    }

    pub fn parse(mut self) -> miette::Result<Vec<Statement<'par>>> {
        let mut statements = vec![];

        while !self.lexer.is_empty() {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> miette::Result<Statement<'par>> {
        match peek!(self.lexer)? {
            Some(token) if matches!(token.kind, Kind::Fun) => self.parse_function(),
            Some(token) if matches!(token.kind, Kind::Struct) => self.parse_struct(),
            Some(token) => todo!("{token:?}"),
            None => todo!(),
        }
    }

    fn parse_function(&mut self) -> miette::Result<Statement<'par>> {
        let keyword = expect!(self.lexer, Kind::Fun)?;

        let (_, name) = parse_identifier(&mut self.lexer)?;
        let arguments = self.parse_function_args(&keyword)?;

        let has_return = peek_matches!(self.lexer, Kind::Op(Operator::ThickArrow));
        let return_type = if has_return { Some(parse_type_annotation(&mut self.lexer)?) } else { None };

        let body = parse_expr_block(&mut self.lexer)?;
        let location = keyword.location.start_byte..body.location().end_byte;

        Ok(Statement::Fun(FunStatement {
            name,
            arguments,
            body,
            return_type,
            location: location.into(),
        }))
    }

    fn parse_struct(&mut self) -> miette::Result<Statement<'par>> {
        let keyword = expect!(self.lexer, Kind::Struct)?;

        let (_, name) = parse_identifier(&mut self.lexer)?;
        let fields = self.parse_struct_fields(&keyword)?;

        let end = expect!(self.lexer, Kind::Op(Operator::RightBrace))?;

        let location = keyword.location.start_byte..end.location.end_byte;
        Ok(Statement::Struct {
            name,
            fields,
            location: location.into(),
        })
    }

    fn parse_struct_fields(&mut self, _: &Token<'_>) -> miette::Result<Vec<StructField<'par>>> {
        expect!(self.lexer, Kind::Op(Operator::LeftBrace))?;

        let mut fields = vec![];

        loop {
            if peek_matches!(self.lexer, Kind::Op(Operator::RightBrace)) {
                break;
            }

            let ident = parse_identifier(&mut self.lexer)?;

            expect_peek!(self.lexer, Kind::Op(Operator::Colon));
            let ty = parse_type_annotation(&mut self.lexer)?;

            let location = (ident.0.location().start_byte..ty.location().end_byte).into();
            fields.push(StructField {
                name: ident.1,
                ty,
                location,
            });

            match peek!(self.lexer)? {
                Some(token) if matches!(token.kind, Kind::Op(Operator::Comma)) => _ = consume!(self.lexer),
                Some(token) if matches!(token.kind, Kind::Op(Operator::RightBrace)) => break,
                Some(token) => {
                    return Err(Error::new(
                        token.location,
                        "Invalid token".into(),
                        Some("You may have forgotten a comma".to_string()),
                        NamedSource::new("file.rs", self.lexer.source_arc.clone()),
                    )
                    .into())
                }
                None => todo!(),
                // return Err("TODO".into()),
            }
        }

        Ok(fields)
    }

    fn parse_function_args(&mut self, _: &Token<'_>) -> miette::Result<Vec<FunArgument<'par>>> {
        expect!(self.lexer, Kind::Op(Operator::LeftParen))?;

        if peek_matches!(self.lexer, Kind::Op(Operator::RightParen)) {
            consume!(self.lexer)?;
            return Ok(vec![]);
        }

        let mut arguments = vec![];

        loop {
            let (arg_name_expr, name) = parse_identifier(&mut self.lexer)?;
            let ty = parse_type_annotation(&mut self.lexer)?;

            let location = Location::new(arg_name_expr.location().start_byte, ty.location().end_byte);
            arguments.push(FunArgument::new(name, ty, location));

            match peek!(self.lexer)? {
                Some(token) if matches!(token.kind, Kind::Op(Operator::Comma)) => (),
                Some(token) if matches!(token.kind, Kind::Op(Operator::RightParen)) => break,
                Some(_) => todo!(),
                None => break,
            }
        }

        expect!(self.lexer, Kind::Op(Operator::RightParen))?;

        Ok(arguments)
    }
}

#[cfg(test)]
mod tests {

    use std::sync::Arc;

    use super::*;

    fn make_sut(source: &str, source_arc: Arc<String>) -> Parser<'_> {
        let lexer = Lexer::new(source, source_arc);
        Parser::new(source, lexer)
    }

    #[test]
    fn function_declaration() {
        let source = [
            "fun calculate_circumference(diameter: f64) => f64 {",
            "    const pi = 3.14159265358979323846264338327950288_f32;",
            "    const radius = diameter / 2.0;",
            "    const circumference = 2.0 * pi * radius;",
            "",
            "    const nesting = {",
            "        const something = 10;",
            "        var nesting_more = {",
            "            return 10 + 3 * 4;",
            "        };",
            "",
            "        // returning on the last expresison",
            "        10 + something",
            "    };",
            "",
            "    circumference",
            "}",
        ]
        .join("\n");
        let source = Arc::new(source);

        let ast = match make_sut(source.as_ref(), source.clone()).parse() {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn language_features() {
        let source = r#"
            fun some_function_name(argument: TypeOfArg) => ReturnType {
                const immutable_var = if truthy_val {
                    10
                } else if another_truthy == 10 {
                    20 + 3 * 2
                } else {
                    const my_inner_var: f64 = 100; // comments don't matter
                    // this would be invalid when we do type checking
                    // as we never return!
                };

                /*
                 * we also have multiline comments!
                    /* Although both regular
                     * and multiline comments will never appear on the parser
                     * ast, as they are ignored on the lexer
                     **/
                 **/

                var mutable_value = function_call(immutable_var + 10, immutable_var);
                mutable_value = 10;

                {
                    const something = println(10 + 3);
                    println(10);
                    something_else();
                    var something = func_call();
                }

                mutable_value
            }
        "#;
        let source = Arc::new(source.to_string());

        let ast = match make_sut(source.as_ref(), source.clone()).parse() {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn if_chain() {
        let source = r#"
            fun main() {
                const x = 10;
                if x > 10 {
                    println("x is greater than 10");
                } else if x > 5 {
                    println("x is greater than 5");
                } else {
                    println("x is less than 5");
                }
            }
        "#;
        let source = Arc::new(source.to_string());

        let ast = match make_sut(source.as_ref(), source.clone()).parse() {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(ast);
    }

    #[test]
    fn struct_declaration() {
        let source = r#"
            struct SomeStruct {
                member: i32,
                another_member: f64,
                yet_another_member: bool,
            }
        "#;
        let source = Arc::new(source.to_string());

        let ast = match make_sut(source.as_ref(), source.clone()).parse() {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(ast);
    }
}
