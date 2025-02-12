use std::collections::HashMap;

use kura_lexer::token::{IntSizes, Location};
use kura_parser::{Expression, FunArgument, PrimitiveType, Statement, Type as ParserType};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Type<'ast> {
    Primitive(PrimitiveType),
    Defined(&'ast str),
}

impl From<PrimitiveType> for Type<'_> {
    fn from(ty: PrimitiveType) -> Self {
        Self::Primitive(ty)
    }
}

impl<'ast> From<&'ast str> for Type<'ast> {
    fn from(name: &'ast str) -> Self {
        Self::Defined(name)
    }
}

impl<'ast> From<ParserType<'ast>> for Type<'ast> {
    fn from(ty: ParserType<'ast>) -> Self {
        match ty {
            ParserType::Primitive { ty, .. } => ty.into(),
            ParserType::Defined { name, .. } => name.into(),
        }
    }
}

#[derive(Debug)]
enum TypedStatement<'ast> {
    Fun {
        name: &'ast str,
        body: Vec<TypedExpression<'ast>>,
        return_type: Type<'ast>,
        location: Location,
    },
}

#[derive(Debug)]
enum TypedExpression<'ast> {
    Var {
        mutable: bool,
        name: &'ast str,
        ty: Type<'ast>,
        value: Box<TypedExpression<'ast>>,
        location: Location,
    },
}

#[derive(Debug)]
struct FunctionSignature<'ast> {
    arguments: Vec<Type<'ast>>,
    return_type: Type<'ast>,
}

#[derive(Debug)]
struct Functions<'ast> {
    inner: HashMap<&'ast str, FunctionSignature<'ast>>,
}

impl Functions<'_> {
    fn get(&self, name: &str) -> Option<&FunctionSignature<'_>> {
        self.inner.get(name)
    }
}

impl<'ast> From<(&Vec<FunArgument<'ast>>, &Option<ParserType<'ast>>)> for FunctionSignature<'ast> {
    fn from((arguments, return_type): (&Vec<FunArgument<'ast>>, &Option<ParserType<'ast>>)) -> Self {
        Self {
            arguments: arguments.iter().map(|arg| arg.ty.into()).collect(),
            return_type: return_type.map(Into::into).unwrap_or(PrimitiveType::Unit.into()),
        }
    }
}

impl<'ast> From<&[Statement<'ast>]> for Functions<'ast> {
    fn from(program: &[Statement<'ast>]) -> Self {
        Self {
            inner: program.iter().fold(HashMap::new(), |mut acc, stat| {
                match stat {
                    Statement::Fun {
                        name,
                        arguments,
                        return_type,
                        ..
                    } => acc.insert(name, (arguments, return_type).into()),
                };

                acc
            }),
        }
    }
}

fn typecheck_program(program: &[Statement<'_>]) {
    let functions = Functions::from(program);
    let mut ctxs = HashMap::new();

    for statement in program {
        match statement {
            Statement::Fun { name, .. } => {
                ctxs.insert(name, Context { scopes: vec![] });
                let ctx = ctxs.get_mut(name).unwrap();
                ctx.enter_scope();
                typecheck_function(ctx, &functions, statement);
            }
        }
    }

    println!("{ctxs:#?}");
}

#[derive(Debug)]
struct Context<'a> {
    scopes: Vec<HashMap<&'a str, Type<'a>>>,
}

impl<'a> Context<'a> {
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn get_scope(&mut self) -> &mut HashMap<&'a str, Type<'a>> {
        self.scopes.last_mut().unwrap()
    }
}

fn typecheck_function<'ast>(ctx: &mut Context<'ast>, functions: &'ast Functions<'ast>, function: &Statement<'ast>) {
    let Statement::Fun { arguments, body, .. } = function;

    for arg in arguments {
        let ty = arg.ty.into();
        ctx.get_scope().insert(arg.name, ty);
    }

    for expr in body {
        typecheck_expression(ctx, functions, expr, None);
    }
}

fn typecheck_expression<'ast>(
    ctx: &mut Context<'ast>,
    functions: &'ast Functions<'ast>,
    expr: &Expression<'ast>,
    expected_ty: Option<Type<'ast>>,
) -> Type<'ast> {
    match expr {
        Expression::Var { .. } => typecheck_var(ctx, functions, expr),
        Expression::FunCall { .. } => typecheck_fun_call(ctx, functions, expr),

        Expression::UintLiteral { .. } => typecheck_uint(expr, expected_ty),
        Expression::Ident { .. } => typecheck_ident(ctx, expr),
        _ => todo!(),
    }
}

fn typecheck_var<'ast>(
    ctx: &mut Context<'ast>,
    functions: &'ast Functions<'ast>,
    expr: &Expression<'ast>,
) -> Type<'ast> {
    let Expression::Var { value, name, ty, .. } = expr else { unreachable!() };
    let expected_ty = ty.map(Into::into);
    let ty = typecheck_expression(ctx, functions, value, expected_ty);

    if let Some(expected_ty) = expected_ty {
        if expected_ty != ty {
            panic!("expected type {expected_ty:?} but got {ty:?}")
        }
    }

    let scope = ctx.get_scope();
    scope.insert(name, ty);
    ty
}

fn typecheck_fun_call<'ast>(
    ctx: &mut Context<'ast>,
    functions: &'ast Functions<'ast>,
    expr: &Expression<'ast>,
) -> Type<'ast> {
    let Expression::FunCall { ident, arguments, .. } = expr else { unreachable!() };
    let fun = functions.get(ident).expect("function not found");

    for (arg, expected_ty) in arguments.iter().zip(fun.arguments.iter()) {
        let arg_ty = typecheck_expression(ctx, functions, arg, Some(*expected_ty));
        if arg_ty != *expected_ty {
            panic!("expected type {expected_ty:?} but got {arg_ty:?}")
        }
    }

    fun.return_type
}

fn typecheck_ident<'ast>(ctx: &mut Context<'ast>, expr: &Expression<'ast>) -> Type<'ast> {
    let Expression::Ident { name, .. } = expr else { unreachable!() };

    let ty = ctx.get_scope().get(name).expect("variable not found");
    *ty
}

fn typecheck_uint<'ast>(expr: &Expression<'ast>, expected_ty: Option<Type<'ast>>) -> Type<'ast> {
    let Expression::UintLiteral { value, size, .. } = expr else { unreachable!() };

    let expected_ty = match expected_ty {
        Some(Type::Primitive(ty)) => match ty {
            PrimitiveType::U8 => IntSizes::U8,
            PrimitiveType::U16 => IntSizes::U16,
            PrimitiveType::U32 => IntSizes::U32,
            PrimitiveType::U64 => IntSizes::U64,
            PrimitiveType::Usize => IntSizes::Usize,
            PrimitiveType::I8 => IntSizes::I8,
            PrimitiveType::I16 => IntSizes::I16,
            PrimitiveType::I32 => IntSizes::I32,
            PrimitiveType::I64 => IntSizes::I64,
            PrimitiveType::Isize => IntSizes::Isize,
            _ => IntSizes::I32,
        },
        Some(Type::Defined(_)) => IntSizes::I32,
        None => IntSizes::I32,
    };

    let size = size.unwrap_or(expected_ty);
    match size {
        IntSizes::U8 if *value > u8::MAX.into() => panic!("value is too big for u8"),
        IntSizes::U16 if *value > u16::MAX.into() => panic!("value is too big for u16"),
        IntSizes::U32 if *value > u32::MAX.into() => panic!("value is too big for u32"),
        IntSizes::Usize if *value > usize::MAX as u64 => panic!("value is too big for usize"),

        IntSizes::I8 if *value > i8::MAX as u64 => panic!("value is too big for i8"),
        IntSizes::I16 if *value > i16::MAX as u64 => panic!("value is too big for i16"),
        IntSizes::I32 if *value > i32::MAX as u64 => panic!("value is too big for i32"),
        IntSizes::I64 if *value > i64::MAX as u64 => panic!("value is too big for i64"),
        IntSizes::Isize if *value > isize::MAX as u64 => panic!("value is too big for isize"),
        _ => PrimitiveType::from(size).into(),
    }
}

#[cfg(test)]
mod tests {
    use kura_lexer::Lexer;
    use kura_parser::Parser;

    use super::*;

    #[test]
    fn m_test() {
        let code = r#"
fun main() {
    const x = 10;
    const x: u32 = 100;
    const x: u64 = 10;
}
        "#;

        let lexer = Lexer::new(code);
        let parser = Parser::new(code, lexer);
        let program = parser.parse().unwrap();
        typecheck_program(&program);

        panic!();
    }
}
