use kura_lexer::token::primitive::Primitive;
use kura_lexer::token::{Kind, Location, Operator, Token, Value};
use kura_lexer::Lexer;
use miette::NamedSource;

use crate::ast::*;
use crate::error::Error;
use crate::{consume, expect, peek, peek_matches};

mod precedences {
    pub const BASE: u8 = 0;
    pub const SUM: u8 = 3;
    pub const MUL: u8 = 4;
    pub const ASSOC: u8 = 5;
    pub const APPLY: u8 = 6;
}

fn get_precedence(operator: Operator) -> u8 {
    match operator {
        Operator::Plus | Operator::Minus => precedences::SUM,
        Operator::Star | Operator::Slash => precedences::MUL,
        Operator::And => precedences::ASSOC,
        Operator::LeftParen | Operator::Greater | Operator::Less | Operator::EqualEqual | Operator::NotEqual => {
            precedences::APPLY
        }
        _ => precedences::BASE,
    }
}

pub fn parse_expression<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Expression<'parser>> {
    match peek!(lexer)? {
        Some(token) if matches!(token.kind, Kind::Var | Kind::Const) => parse_variable(lexer),
        Some(_) => parse_with_precedence(lexer, precedences::BASE),
        None => unreachable!(),
    }
}

pub fn parse_identifier<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<(Expression<'parser>, &'parser str)> {
    let name_and_loc = consume!(lexer)?.map(|token| match token.kind {
        Kind::Value(Value::Ident(name)) => (name, token.location),
        _ => ("", token.location),
    });

    match name_and_loc {
        Some(("", location)) => Err(Error::new(
            location,
            format!(r#"expected identifier but got "" {location}"#),
            None,
            NamedSource::new("file.rs", lexer.source_arc.clone()),
        )
        .into()),
        Some((name, location)) => Ok((Expression::Ident(IdentExpr { name, location }), name)),
        None => todo!(),
    }
}

pub fn parse_expr_block<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Expression<'parser>> {
    let mut expressions = vec![];
    let mut trailing_expr = None;

    let block_start = expect!(lexer, Kind::Op(Operator::LeftBrace))?;

    loop {
        match peek!(lexer)? {
            Some(token) if matches!(token.kind, Kind::Op(Operator::RightBrace)) => break,
            None => break,
            _ => (),
        }

        let expr = parse_expression(lexer)?;

        // after an expression, we must have a semicolon, unless we are at the end of a block, in which case
        // we can have an optional trailing expression without a semicolon that returns itself.
        //
        // if there is a expression without a semicolon that is not at the end of the block, that
        // is a syntax error.
        match peek!(lexer)? {
            Some(token) if matches!(token.kind, Kind::Op(Operator::SemiColon)) => {
                consume!(lexer)?;
                expressions.push(expr);
            }
            Some(token) if matches!(token.kind, Kind::Op(Operator::RightBrace)) => {
                trailing_expr = Some(Box::new(expr));
                break;
            }
            _ => (),
        }
    }

    let block_end = expect!(lexer, Kind::Op(Operator::RightBrace))?;
    let location = block_start.location.start_byte..block_end.location.end_byte;
    Ok(Expression::Block(BlockExpr {
        body: expressions,
        trailing_expr,
        location: location.into(),
    }))
}

fn parse_variable<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Expression<'parser>> {
    let keyword = expect!(lexer, Kind::Var, Kind::Const)?;
    let mutable = matches!(keyword.kind, Kind::Var);

    let (_, name) = parse_identifier(lexer)?;

    let ty = match peek_matches!(lexer, Kind::Op(Operator::Colon)) {
        true => Some(parse_type_annotation(lexer)?),
        false => None,
    };

    expect!(lexer, Kind::Op(Operator::Equal))?;

    let value = match peek!(lexer)? {
        Some(token) if matches!(token.kind, Kind::Op(Operator::LeftBrace)) => parse_expr_block(lexer)?,
        _ => parse_expression(lexer)?,
    };

    let location = Location::new(keyword.location.start_byte, value.location().end_byte);
    Ok(Expression::Var(VarExpr {
        mutable,
        ty,
        name,
        value: Box::new(value),
        location,
    }))
}

fn parse_if_expression<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Expression<'parser>> {
    let keyword = expect!(lexer, Kind::If)?;

    let condition = parse_expression(lexer)?;
    let body = parse_expr_block(lexer)?;

    let mut falsy_branches = vec![];

    while let Some(keyword) = peek!(lexer)? {
        if keyword.kind == Kind::Else {
            consume!(lexer)?;

            if peek_matches!(lexer, Kind::If) {
                let else_if = parse_if_expression(lexer)?;
                falsy_branches.push(else_if);
            } else {
                let else_block = parse_expr_block(lexer)?;
                falsy_branches.push(else_block);
                break;
            }
        } else {
            break;
        }
    }

    let end = match falsy_branches.last() {
        Some(branch) => branch.location().end_byte,
        None => body.location().end_byte,
    };

    let location = keyword.location.start_byte..end;
    Ok(Expression::If(IfExpr {
        condition: Box::new(condition),
        truthy: Box::new(body),
        falsy: falsy_branches,
        location: location.into(),
    }))
}

fn parse_value<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Expression<'parser>> {
    let Some(Token { kind, .. }) = peek!(lexer)? else {
        unreachable!();
    };

    let Kind::Value(value) = kind else {
        unreachable!();
    };

    match value {
        Value::Primitive(_) => parse_primitive(lexer),
        Value::Ident(_) => Ok(parse_identifier(lexer)?.0),
        Value::String(_) => Ok(parse_string(lexer)?),
    }
}

fn parse_string<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Expression<'parser>> {
    let (value, location) = match consume!(lexer)? {
        Some(Token {
            kind: Kind::Value(Value::String(string)),
            location,
            ..
        }) => (string, location),
        _ => unreachable!(),
    };

    Ok(Expression::String(StringExpr { value, location }))
}

fn parse_operation<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Expression<'parser>> {
    let Some(Token { kind, .. }) = peek!(lexer)? else {
        unreachable!();
    };

    let Kind::Op(op) = kind else {
        unreachable!();
    };

    match op {
        Operator::LeftParen => {
            consume!(lexer)?;
            let left = parse_with_precedence(lexer, precedences::BASE)?;
            expect!(lexer, Kind::Op(Operator::RightParen))?;

            Ok(left)
        }
        Operator::LeftBrace => parse_expr_block(lexer),
        t => todo!("{t:?}"),
    }
}

fn parse_fun_call<'parser>(
    lexer: &mut Lexer<'parser>,
    ident: Expression<'parser>,
) -> miette::Result<Expression<'parser>> {
    consume!(lexer)?;

    let mut arguments = vec![];

    loop {
        match peek!(lexer)? {
            Some(token) if matches!(token.kind, Kind::Op(Operator::RightParen)) => {
                break;
            }
            Some(token) if matches!(token.kind, Kind::Op(Operator::Comma)) => {
                lexer.next().transpose()?;
                continue;
            }
            None => break,
            _ => (),
        }

        let arg = parse_expression(lexer)?;

        arguments.push(arg);
    }

    let close_paren = expect!(lexer, Kind::Op(Operator::RightParen))?;

    let Expression::Ident(IdentExpr { name, .. }) = ident else { unreachable!() };

    let location = ident.location().start_byte..close_paren.location.end_byte;
    let expr = Expression::FunCall(FunCallExpr {
        ident: name,
        location: location.into(),
        arguments,
    });
    Ok(expr)
}

pub fn parse_type_annotation<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Type<'parser>> {
    match peek!(lexer)? {
        Some(token) if matches!(token.kind, Kind::Op(Operator::Colon)) => consume!(lexer)?,
        Some(token) if matches!(token.kind, Kind::Op(Operator::ThickArrow)) => consume!(lexer)?,
        _ => unreachable!(),
    };

    let (type_exp, type_name) = parse_identifier(lexer)?;
    let location = type_exp.location();
    let arg_type = Type::from_identifier(type_name, location);
    Ok(arg_type)
}

fn parse_assign<'parser>(lexer: &mut Lexer<'parser>, left: Expression<'parser>) -> miette::Result<Expression<'parser>> {
    expect!(lexer, Kind::Op(Operator::Equal))?;

    let value = match peek!(lexer)? {
        Some(token) if matches!(token.kind, Kind::Op(Operator::LeftBrace)) => parse_expr_block(lexer)?,
        Some(_) => parse_expression(lexer)?,
        _ => unreachable!(),
    };

    let location = left.location().start_byte..value.location().end_byte;

    Ok(Expression::Assign(AssignExpr {
        ident: Box::new(left),
        location: location.into(),
        value: Box::new(value),
    }))
}

fn parse_with_precedence<'parser>(
    lexer: &mut Lexer<'parser>,
    min_precedence: u8,
) -> miette::Result<Expression<'parser>> {
    let mut left = match peek!(lexer)? {
        Some(token) if matches!(token.kind, Kind::Value(_)) => parse_value(lexer)?,
        Some(token) if matches!(token.kind, Kind::Op(_)) => parse_operation(lexer)?,
        Some(token) if matches!(token.kind, Kind::Return) => parse_return_expression(lexer)?,
        Some(token) if matches!(token.kind, Kind::If) => parse_if_expression(lexer)?,
        Some(token) => todo!("{token:?}"),
        None => todo!(),
    };

    if let Expression::Ident { .. } = left {
        match peek!(lexer)? {
            Some(token) if matches!(token.kind, Kind::Op(Operator::LeftParen)) => {
                return parse_fun_call(lexer, left);
            }
            Some(token) if matches!(token.kind, Kind::Op(Operator::Equal)) => return parse_assign(lexer, left),
            Some(token) if token.kind.is_binary_op() => {}
            _ => unreachable!("this should not be allowed"),
        }
    }

    loop {
        let Some(next) = peek!(lexer)? else {
            return Ok(left);
        };

        let Kind::Op(operator) = next.kind else {
            return Ok(left);
        };

        if !next.kind.is_binary_op() {
            return Ok(left);
        }

        let precedence = get_precedence(operator);

        if precedence <= min_precedence {
            break;
        }

        let Some(_) = consume!(lexer)? else {
            unreachable!();
        };

        let right = parse_with_precedence(lexer, precedence)?;

        let location = Location::new(left.location().start_byte, right.location().end_byte);
        left = Expression::BinaryOp(BinaryOpExpr {
            lhs: Box::new(left),
            operator,
            rhs: Box::new(right),
            location,
        });
    }

    Ok(left)
}

fn parse_return_expression<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Expression<'parser>> {
    let keyword = expect!(lexer, Kind::Return)?;

    let value = parse_expression(lexer)?;

    let ending_semi = expect!(lexer, Kind::Op(Operator::SemiColon))?;

    let location = keyword.location.start_byte..ending_semi.location.end_byte;
    Ok(Expression::Return(ReturnExpr {
        value: Box::new(value),
        location: location.into(),
    }))
}

fn parse_primitive<'parser>(lexer: &mut Lexer<'parser>) -> miette::Result<Expression<'parser>> {
    let (primitive, location) = match consume!(lexer)? {
        Some(Token {
            kind: Kind::Value(Value::Primitive(primitive)),
            location,
            ..
        }) => (primitive, location),
        _ => unreachable!(),
    };

    match primitive {
        Primitive::Int { value, size } => Ok(Expression::IntLiteral(IntLiteralExpr { value, size, location })),
        Primitive::Float { value, size } => Ok(Expression::FloatLiteral(FloatLiteralExpr { value, size, location })),
        Primitive::Bool(value) => Ok(Expression::Bool(BoolExpr { value, location })),
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::Parser;

    fn make_sut(source: &str, source_arc: Arc<String>) -> Parser<'_> {
        let lexer = Lexer::new(source, source_arc);
        Parser::new(source, lexer)
    }

    #[test]
    fn simple_math() {
        let simple_math_expr = Arc::new("1 + 2 * 3 - 4".to_string());
        let mut parser = make_sut(simple_math_expr.as_ref(), simple_math_expr.clone());

        let math_expr_ast = match parse_expression(&mut parser.lexer) {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(math_expr_ast);
    }

    #[test]
    fn mutable_variable() {
        let variables = Arc::new("var hello = 1 + 2 * 3;".to_string());
        let mut parser = make_sut(variables.as_ref(), variables.clone());

        let variables_ast = match parse_expression(&mut parser.lexer) {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(variables_ast);
    }

    #[test]
    fn immutable_constant() {
        let variables = Arc::new("const hello = 1 + 2 * 3;".to_string());
        let mut parser = make_sut(variables.as_ref(), variables.clone());

        let variables_ast = match parse_expression(&mut parser.lexer) {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(variables_ast);
    }

    #[test]
    fn if_statement() {
        let source = r#"
            if something == another_thing && 1 != 2 {
                const this_is_a_var = 10 + 3;
            } else if something == 10 {
                const this_is_another = 10 + 10;
            } else {
                const omg = 1 + 1;
            }
        "#;
        let source = Arc::new(source.to_string());
        let mut parser = make_sut(source.as_ref(), source.clone());

        let if_ast = match parse_expression(&mut parser.lexer) {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(if_ast);
    }

    #[test]
    fn if_as_variable_value() {
        let source = r#"
            const my_var = if something == another_thing {
                10 + 10
            } else {
                20 + 20
            };
        "#;
        let source = Arc::new(source.to_string());

        let mut parser = make_sut(source.as_ref(), source.clone());
        let let_if_ast = match parse_expression(&mut parser.lexer) {
            Ok(expr) => expr,
            Err(e) => panic!("{e:?}"),
        };

        insta::assert_debug_snapshot!(let_if_ast);
    }
}
