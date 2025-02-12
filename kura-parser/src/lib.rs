mod expression;

use expression::parse_type_annotation;
use kura_lexer::token::primitive::{FloatSizes, IntSizes, Numeral};
use kura_lexer::token::{Kind, Location, Operator, Token};
use kura_lexer::{Lexer, TransposeRef};

use crate::expression::{parse_expression, parse_identifier};

static PRIMITIVE_MAP: &[(&str, PrimitiveType)] = &[
    ("u8", PrimitiveType::U8),
    ("u16", PrimitiveType::U16),
    ("u32", PrimitiveType::U32),
    ("u64", PrimitiveType::U64),
    ("i8", PrimitiveType::I8),
    ("i16", PrimitiveType::I16),
    ("i32", PrimitiveType::I32),
    ("i64", PrimitiveType::I64),
    ("bool", PrimitiveType::Bool),
    ("f32", PrimitiveType::F32),
    ("f64", PrimitiveType::F64),
];

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveType {
    #[default]
    Unit,
    U8,
    U16,
    U32,
    U64,
    Usize,
    I8,
    I16,
    I32,
    I64,
    Isize,
    Bool,
    F32,
    F64,
}

impl From<IntSizes> for PrimitiveType {
    fn from(size: IntSizes) -> Self {
        match size {
            IntSizes::I8 => PrimitiveType::I8,
            IntSizes::I16 => PrimitiveType::I16,
            IntSizes::I32 => PrimitiveType::I32,
            IntSizes::I64 => PrimitiveType::I64,
            IntSizes::Isize => PrimitiveType::Isize,
            IntSizes::U8 => PrimitiveType::U8,
            IntSizes::U16 => PrimitiveType::U16,
            IntSizes::U32 => PrimitiveType::U32,
            IntSizes::U64 => PrimitiveType::U64,
            IntSizes::Usize => PrimitiveType::Usize,
        }
    }
}

impl From<FloatSizes> for PrimitiveType {
    fn from(size: FloatSizes) -> Self {
        match size {
            FloatSizes::F32 => PrimitiveType::F32,
            FloatSizes::F64 => PrimitiveType::F64,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Type<'ast> {
    Primitive { ty: PrimitiveType, location: Location },
    Defined { name: &'ast str, location: Location },
}

impl<'ast> Type<'ast> {
    fn from_identifier(name: &'ast str, location: Location) -> Self {
        if let Some((_, ty)) = PRIMITIVE_MAP.iter().find(|(n, _)| name == *n) {
            return Type::Primitive { ty: *ty, location };
        };

        Type::Defined { name, location }
    }

    fn location(&self) -> Location {
        match self {
            Self::Primitive { location, .. } => *location,
            Self::Defined { location, .. } => *location,
        }
    }
}

#[derive(Debug)]
pub struct FunArgument<'ast> {
    pub name: &'ast str,
    pub ty: Type<'ast>,
    pub location: Location,
}

#[derive(Debug)]
pub enum Statement<'ast> {
    Fun {
        name: &'ast str,
        arguments: Vec<FunArgument<'ast>>,
        body: Vec<Expression<'ast>>,
        return_type: Option<Type<'ast>>,
        location: Location,
    },
}

#[derive(Debug)]
pub enum Expression<'ast> {
    Var {
        mutable: bool,
        name: &'ast str,
        ty: Option<Type<'ast>>,
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
        truthy: Box<Expression<'ast>>,
        // each `else` or `else if` are added here, if nothing, then
        // this will be an empty vec
        //
        // TODO: maybe use Options so we dont allocate a vector
        falsy: Vec<Expression<'ast>>,
    },
    FunCall {
        ident: &'ast str,
        location: Location,
        arguments: Vec<Expression<'ast>>,
    },
    Assign {
        ident: Box<Expression<'ast>>,
        location: Location,
        value: Box<Expression<'ast>>,
    },
    Ident {
        name: &'ast str,
        location: Location,
    },
    Block {
        expressions: Vec<Expression<'ast>>,
        location: Location,
    },
    FloatLiteral {
        value: f64,
        size: Option<FloatSizes>,
        location: Location,
    },
    IntLiteral {
        value: Numeral,
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
            Expression::FunCall { location, .. } => *location,
            Expression::Assign { location, .. } => *location,
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
        match self.lexer.peek().transpose().map_err(|e| e.to_string())? {
            Some(token) => match &token.kind {
                Kind::Fun => self.parse_function(),
                t => todo!("{t:?}"),
            },
            _ => todo!(),
        }
    }

    fn parse_function_args(&mut self, _: &Token<'_>) -> Result<Vec<FunArgument<'par>>, String> {
        let mut arguments = vec![];

        loop {
            let (arg_name_expr, arg_name) = parse_identifier(&mut self.lexer)?;
            self.lexer
                .expect(Kind::Op(Operator::Colon))
                .map_err(|e| e.to_string())?;

            let arg_type = parse_type_annotation(&mut self.lexer)?;

            arguments.push(FunArgument {
                name: arg_name,
                location: Location::new(arg_name_expr.location().start_byte, arg_type.location().end_byte),
                ty: arg_type,
            });

            match self.lexer.peek().transpose().map_err(|e| e.to_string())? {
                Some(token) => match token.kind {
                    Kind::Op(Operator::RightParen) => break,
                    Kind::Op(Operator::Comma) => (),
                    _ => return Err(token.location.to_string())?,
                },
                None => break,
            }
        }

        Ok(arguments)
    }

    fn parse_function(&mut self) -> Result<Statement<'par>, String> {
        let keyword = self.lexer.expect(Kind::Fun).map_err(|e| e.to_string())?;
        let (_, fun_name) = parse_identifier(&mut self.lexer)?;
        self.lexer
            .expect(Kind::Op(Operator::LeftParen))
            .map_err(|e| e.to_string())?;

        let mut arguments = vec![];
        if let Some(next) = self.lexer.peek().transpose().map_err(|e| e.to_string())? {
            if !matches!(next.kind, Kind::Op(Operator::RightParen)) {
                arguments = self.parse_function_args(&keyword)?;
            }
        }

        // after parsing argument list we need to consume the closing parenthesis
        self.lexer
            .expect(Kind::Op(Operator::RightParen))
            .map_err(|e| e.to_string())?;

        // after the argument list of a function, there can be an optional return type annotation
        // => <TYPE> {
        // before the left brace, but its fine to be ommited
        let has_return = match self.lexer.peek().transpose().map_err(|e| e.to_string())? {
            Some(token) => matches!(token.kind, Kind::Op(Operator::ThickArrow)),
            _ => false,
        };

        let return_type = if has_return {
            self.lexer.next().transpose().map_err(|e| e.to_string())?;
            let return_type = parse_type_annotation(&mut self.lexer)?;
            Some(return_type)
        } else {
            None
        };

        self.lexer
            .expect(Kind::Op(Operator::LeftBrace))
            .map_err(|e| e.to_string())?;

        let mut body = vec![];

        while let Some(token) = self.lexer.peek().transpose().map_err(|e| e.to_string())? {
            if let Kind::Op(Operator::RightBrace) = token.kind {
                break;
            }
            body.push(parse_expression(&mut self.lexer, true)?);
        }

        // consume the closing brace of the function
        let closing_brace = self
            .lexer
            .expect(Kind::Op(Operator::RightBrace))
            .map_err(|e| e.to_string())?;

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
}
