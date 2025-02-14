use std::collections::HashMap;

use kura_lexer::token::primitive::{IntSizes, Numeral};
use kura_parser::ast::{Expression, FunArgument, PrimitiveType, Statement, Type as ParserType};

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
struct TypedFunArgument<'ast> {
    name: &'ast str,
    ty: Type<'ast>,
}

impl<'ast> From<&FunArgument<'ast>> for TypedFunArgument<'ast> {
    fn from(arg: &FunArgument<'ast>) -> Self {
        Self {
            name: arg.name,
            ty: arg.ty.into(),
        }
    }
}

#[derive(Debug)]
enum TypedStatement<'ast> {
    Fun {
        name: &'ast str,
        body: TypedExpression<'ast>,
        return_type: Type<'ast>,
        arguments: Vec<TypedFunArgument<'ast>>,
    },
}

#[derive(Debug)]
enum TypedExpression<'ast> {
    Var {
        mutable: bool,
        name: &'ast str,
        ty: Type<'ast>,
        value: Box<TypedExpression<'ast>>,
    },
    IntLiteral {
        value: Numeral,
        ty: Type<'ast>,
    },
    FunCall {
        ident: &'ast str,
        arguments: Vec<TypedExpression<'ast>>,
        ty: Type<'ast>,
    },
    Ident {
        name: &'ast str,
        ty: Type<'ast>,
    },
    Block {
        body: Vec<TypedExpression<'ast>>,
        trailing_expr: Option<Box<TypedExpression<'ast>>>,
        ty: Type<'ast>,
    },
    Bool {
        value: bool,
        ty: Type<'ast>,
    },
}

impl<'ast> TypedExpression<'ast> {
    fn ty(&self) -> Type<'ast> {
        match self {
            Self::Var { ty, .. } => *ty,
            Self::IntLiteral { ty, .. } => *ty,
            Self::FunCall { ty, .. } => *ty,
            Self::Ident { ty, .. } => *ty,
            Self::Block { ty, .. } => *ty,
            Self::Bool { ty, .. } => *ty,
        }
    }
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

pub fn typecheck_program(program: &[Statement<'_>]) {
    let functions = Functions::from(program);
    let mut ctxs = HashMap::new();
    let mut ast = vec![];

    for statement in program {
        match statement {
            Statement::Fun { name, .. } => {
                ctxs.insert(name, Context { scopes: vec![] });
                let ctx = ctxs.get_mut(name).unwrap();
                ctx.enter_scope();

                let stat = typecheck_function(ctx, &functions, statement);
                ast.push(stat);
            }
        }
    }

    println!("{ast:#?}");
}

fn typecheck_function<'ast>(
    ctx: &mut Context<'ast>,
    functions: &'ast Functions<'ast>,
    function: &Statement<'ast>,
) -> TypedStatement<'ast> {
    let Statement::Fun {
        name,
        arguments,
        body,
        return_type,
        ..
    } = function;

    for arg in arguments {
        let ty = arg.ty.into();
        ctx.get_scope().insert(arg.name, ty);
    }

    let return_type = return_type.map(Into::into).unwrap_or(PrimitiveType::Unit.into());
    let body = typecheck_expression(ctx, functions, body, None);

    if return_type != body.ty() {
        panic!("expected return type {return_type:?} but got {:?}", body.ty())
    }

    TypedStatement::Fun {
        name,
        body,
        arguments: arguments.iter().map(Into::into).collect(),
        return_type,
    }
}

fn typecheck_expression<'ast>(
    ctx: &mut Context<'ast>,
    functions: &'ast Functions<'ast>,
    expr: &Expression<'ast>,
    expected_ty: Option<Type<'ast>>,
) -> TypedExpression<'ast> {
    match expr {
        Expression::Var { .. } => typecheck_var(ctx, functions, expr),
        Expression::FunCall { .. } => typecheck_fun_call(ctx, functions, expr),
        Expression::Block { .. } => typecheck_block(ctx, functions, expr),

        Expression::IntLiteral { .. } => typecheck_uint(expr, expected_ty),
        Expression::Ident { .. } => typecheck_ident(ctx, expr),

        Expression::Bool { .. } => typecheck_bool(expr),

        Expression::If { .. } => todo!(),
        Expression::Assign { .. } => todo!(),
        Expression::FloatLiteral { .. } => todo!(),
        Expression::BinaryOp { .. } => todo!(),
        Expression::Return { .. } => todo!(),
    }
}

fn typecheck_bool<'ast>(expr: &Expression<'ast>) -> TypedExpression<'ast> {
    let Expression::Bool { value, .. } = expr else { unreachable!() };

    TypedExpression::Bool {
        value: *value,
        ty: PrimitiveType::Bool.into(),
    }
}

fn typecheck_block<'ast>(
    ctx: &mut Context<'ast>,
    functions: &'ast Functions<'ast>,
    expr: &Expression<'ast>,
) -> TypedExpression<'ast> {
    let Expression::Block {
        body, trailing_expr, ..
    } = expr
    else {
        unreachable!()
    };

    let mut typed_body = vec![];

    for expr in body {
        let expr = typecheck_expression(ctx, functions, expr, None);
        typed_body.push(expr);
    }

    let trailing_expr = trailing_expr
        .as_ref()
        .map(|trailing_expr| Box::new(typecheck_expression(ctx, functions, trailing_expr, None)));

    TypedExpression::Block {
        body: typed_body,
        ty: trailing_expr
            .as_ref()
            .map(|expr| expr.ty())
            .unwrap_or(PrimitiveType::Unit.into()),
        trailing_expr,
    }
}

fn typecheck_var<'ast>(
    ctx: &mut Context<'ast>,
    functions: &'ast Functions<'ast>,
    expr: &Expression<'ast>,
) -> TypedExpression<'ast> {
    let Expression::Var { value, name, ty, .. } = expr else { unreachable!() };
    let expected_ty = ty.map(Into::into);
    let expr = typecheck_expression(ctx, functions, value, expected_ty);

    if let Some(expected_ty) = expected_ty {
        if expected_ty != expr.ty() {
            panic!("expected type {expected_ty:?} but got {ty:?}")
        }
    }

    let scope = ctx.get_scope();
    scope.insert(name, expr.ty());

    expr
}

fn typecheck_fun_call<'ast>(
    ctx: &mut Context<'ast>,
    functions: &'ast Functions<'ast>,
    expr: &Expression<'ast>,
) -> TypedExpression<'ast> {
    let Expression::FunCall { ident, arguments, .. } = expr else { unreachable!() };
    let fun = functions.get(ident).expect("function not found");

    if fun.arguments.len() != arguments.len() {
        panic!(
            "expected {} argument(s) but got {}",
            fun.arguments.len(),
            arguments.len()
        );
    }

    let arguments = arguments
        .iter()
        .zip(fun.arguments.iter())
        .map(|(arg, expected_ty)| {
            let expr = typecheck_expression(ctx, functions, arg, Some(*expected_ty));
            if expr.ty() != *expected_ty {
                panic!("expected type {expected_ty:?} but got {:?}", expr.ty())
            }
            expr
        })
        .collect();

    TypedExpression::FunCall {
        ty: fun.return_type,
        ident,
        arguments,
    }
}

fn typecheck_ident<'ast>(ctx: &mut Context<'ast>, expr: &Expression<'ast>) -> TypedExpression<'ast> {
    let Expression::Ident { name, .. } = expr else { unreachable!() };

    let ty = *ctx.get_scope().get(name).expect("variable not found");
    TypedExpression::Ident { ty, name }
}

fn typecheck_uint<'ast>(expr: &Expression<'ast>, expected_ty: Option<Type<'ast>>) -> TypedExpression<'ast> {
    let Expression::IntLiteral { value, size, .. } = expr else { unreachable!() };

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
    let ty = match (size, value) {
        (IntSizes::U8, Numeral::Unsigned(val)) if *val > u8::MAX.into() => panic!("value is too big for u8"),
        (IntSizes::U16, Numeral::Unsigned(val)) if *val > u16::MAX.into() => panic!("value is too big for u16"),
        (IntSizes::U32, Numeral::Unsigned(val)) if *val > u32::MAX.into() => panic!("value is too big for u32"),
        (IntSizes::Usize, Numeral::Unsigned(val)) if *val > usize::MAX as u64 => panic!("value is too big for usize"),

        (IntSizes::I8, Numeral::Unsigned(val)) if *val > i8::MAX as u64 => panic!("value is too big for i8"),
        (IntSizes::I16, Numeral::Unsigned(val)) if *val > i16::MAX as u64 => panic!("value is too big for i16"),
        (IntSizes::I32, Numeral::Unsigned(val)) if *val > i32::MAX as u64 => panic!("value is too big for i32"),
        (IntSizes::Isize, Numeral::Unsigned(val)) if *val > isize::MAX as u64 => panic!("value is too big for isize"),

        (IntSizes::I8, Numeral::Signed(val)) if *val > i8::MAX as i64 => panic!("value is too big for i8"),
        (IntSizes::I16, Numeral::Signed(val)) if *val > i16::MAX as i64 => panic!("value is too big for i16"),
        (IntSizes::I32, Numeral::Signed(val)) if *val > i32::MAX as i64 => panic!("value is too big for i32"),
        (IntSizes::Isize, Numeral::Signed(val)) if *val > isize::MAX as i64 => panic!("value is too big for isize"),

        (IntSizes::I8, Numeral::Signed(val)) if *val < i8::MIN as i64 => panic!("value is too small for i8"),
        (IntSizes::I16, Numeral::Signed(val)) if *val < i16::MIN as i64 => panic!("value is too small for i16"),
        (IntSizes::I32, Numeral::Signed(val)) if *val < i32::MIN as i64 => panic!("value is too small for i32"),
        (IntSizes::Isize, Numeral::Signed(val)) if *val < isize::MIN as i64 => panic!("value is too small for isize"),

        (_, Numeral::Signed(_)) if size.is_unsigned() => panic!("cannot assign signed value to unsigned type"),
        _ => PrimitiveType::from(size).into(),
    };

    TypedExpression::IntLiteral { ty, value: *value }
}

#[cfg(test)]
mod tests {
    use kura_lexer::Lexer;
    use kura_parser::Parser;

    use super::*;

    #[test]
    fn m_test() {
        let code = r#"
fun do_something_with_x(x: i32) => i32 {
    true
}

fun main() {
    const x = 10;
    const x: i8 = 10;
    const x: i16 = 10;
    const x: i32 = 10;
    do_something_with_x(x);
}

        "#;

        let lexer = Lexer::new(code);
        let parser = Parser::new(code, lexer);
        let program = parser.parse().unwrap();
        typecheck_program(&program);

        panic!();
    }
}
