pub mod ast;
mod expression;

use ast::{FunArgument, Statement, StructField};
use expression::{parse_expr_block, parse_type_annotation};
use kura_lexer::token::{Kind, Location, Operator, Token};
use kura_lexer::{Lexer, TransposeRef};

use crate::expression::parse_identifier;

/// Consumes the next token from the lexer, returning an error if there is no next token.
#[macro_export]
macro_rules! consume {
    ($lexer:expr) => {
        $lexer.next().transpose().map_err(|e| e.to_string())?
    };
}

#[macro_export]
macro_rules! expect {
    ($lexer:expr, $kind:expr) => {
        $lexer.expect($kind).map_err(|e| e.to_string())?
    };

    ($lexer:expr, $first:expr, $($rest:expr),+) => {
        $lexer.expect_one_of(&[$first, $($rest),+]).map_err(|e| e.to_string())?
    };
}

#[macro_export]
macro_rules! expect_peek {
    ($lexer:expr, $kind:expr) => {{
        let token = peek!($lexer);
        if token.is_none() || token.unwrap().kind != $kind {
            return Err(format!("expected {:?} but got {:?}", $kind, token.unwrap().kind));
        }
    }};
}

#[macro_export]
macro_rules! peek {
    ($lexer:expr) => {
        $lexer.peek().transpose().map_err(|e| e.to_string())?
    };
}

#[macro_export]
macro_rules! peek_matches {
    ($lexer:expr, $kind:pat) => {
        {
            if let Some(token) = peek!($lexer) {
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

    pub fn parse(mut self) -> Result<Vec<Statement<'par>>, String> {
        let mut statements = vec![];

        while !self.lexer.is_empty() {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }

        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Statement<'par>, String> {
        match peek!(self.lexer) {
            Some(token) if matches!(token.kind, Kind::Fun) => self.parse_function(),
            Some(token) if matches!(token.kind, Kind::Struct) => self.parse_struct(),
            Some(token) => todo!("{token:?}"),
            None => todo!(),
        }
    }

    fn parse_function(&mut self) -> Result<Statement<'par>, String> {
        let keyword = expect!(self.lexer, Kind::Fun);

        let (_, name) = parse_identifier(&mut self.lexer)?;
        let arguments = self.parse_function_args(&keyword)?;

        let has_return = peek_matches!(self.lexer, Kind::Op(Operator::ThickArrow));
        let return_type = if has_return { Some(parse_type_annotation(&mut self.lexer)?) } else { None };

        let body = parse_expr_block(&mut self.lexer)?;
        let location = keyword.location.start_byte..body.location().end_byte;

        Ok(Statement::Fun {
            name,
            arguments,
            body,
            return_type,
            location: location.into(),
        })
    }

    fn parse_struct(&mut self) -> Result<Statement<'par>, String> {
        let keyword = expect!(self.lexer, Kind::Struct);

        let (_, name) = parse_identifier(&mut self.lexer)?;
        let fields = self.parse_struct_fields(&keyword)?;

        let end = expect!(self.lexer, Kind::Op(Operator::RightBrace));

        let location = keyword.location.start_byte..end.location.end_byte;
        Ok(Statement::Struct {
            name,
            fields,
            location: location.into(),
        })
    }

    fn parse_struct_fields(&mut self, _: &Token<'_>) -> Result<Vec<StructField<'par>>, String> {
        expect!(self.lexer, Kind::Op(Operator::LeftBrace));

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

            match peek!(self.lexer) {
                Some(token) if matches!(token.kind, Kind::Op(Operator::Comma)) => _ = consume!(self.lexer),
                Some(token) if matches!(token.kind, Kind::Op(Operator::RightBrace)) => break,
                Some(token) => return Err(token.location.to_string())?,
                None => return Err("TODO".into()),
            }
        }

        Ok(fields)
    }

    fn parse_function_args(&mut self, _: &Token<'_>) -> Result<Vec<FunArgument<'par>>, String> {
        expect!(self.lexer, Kind::Op(Operator::LeftParen));

        if peek_matches!(self.lexer, Kind::Op(Operator::RightParen)) {
            consume!(self.lexer);
            return Ok(vec![]);
        }

        let mut arguments = vec![];

        loop {
            let (arg_name_expr, name) = parse_identifier(&mut self.lexer)?;
            let ty = parse_type_annotation(&mut self.lexer)?;

            let location = Location::new(arg_name_expr.location().start_byte, ty.location().end_byte);
            arguments.push(FunArgument::new(name, ty, location));

            match peek!(self.lexer) {
                Some(token) if matches!(token.kind, Kind::Op(Operator::Comma)) => (),
                Some(token) if matches!(token.kind, Kind::Op(Operator::RightParen)) => break,
                Some(token) => return Err(token.location.to_string())?,
                None => break,
            }
        }

        expect!(self.lexer, Kind::Op(Operator::RightParen));

        Ok(arguments)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_sut(source: &str) -> Parser<'_> {
        let lexer = Lexer::new(source);
        Parser::new(source, lexer)
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

                    // returning on the last expresison
                    10 + something
                };

                circumference
            }"#;

        let ast = match make_sut(source).parse() {
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

        let ast = match make_sut(source).parse() {
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

        let ast = match make_sut(source).parse() {
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

        let ast = match make_sut(source).parse() {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(ast);
    }
}
