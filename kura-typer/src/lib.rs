use std::collections::HashMap;

use kura_lexer::token::Location;
use kura_lexer::token::primitive::FloatSizes;
use kura_parser::ast::*;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct FunctionId(usize);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct TypeId(usize);

impl TypeId {
    pub fn next(&self) -> Self {
        Self(self.0 + 1)
    }
}

#[derive(Debug)]
struct ResolvedFunDecl<'src> {
    name: &'src str,
    return_type: ResolvedType,
    location: Location,
    args: Vec<ResolvedFunArgDecl<'src>>,
}

#[derive(Debug)]
struct ResolvedFunArgDecl<'src> {
    name: &'src str,
    ty: ResolvedType,
    location: Location,
}

#[derive(Debug)]
struct ResolvedStruct<'src> {
    name: &'src str,
    type_id: TypeId,
    location: Location,
    fields: Vec<ResolvedStructField<'src>>,
}

#[derive(Debug)]
enum TypeValue<'ast> {
    Struct(ResolvedStruct<'ast>),
    Primitive(PrimitiveType),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum ResolvedType {
    Primitive(PrimitiveType),
    Struct(TypeId),
}

#[derive(Debug)]
enum TypeStatus<'ast> {
    Resolved(ResolvedType),
    Pending(NamedType<'ast>),
}

impl<'ast> From<Type<'ast>> for TypeStatus<'ast> {
    fn from(value: Type<'ast>) -> Self {
        match value {
            Type::Primitive(primitive) => TypeStatus::Resolved(ResolvedType::Primitive(primitive)),
            Type::Named(named_type) => TypeStatus::Pending(named_type),
        }
    }
}

#[derive(Debug)]
struct ResolvedStructField<'ast> {
    name: &'ast str,
    ty: TypeStatus<'ast>,
    location: Location,
}

impl<'ast> From<&StructField<'ast>> for ResolvedStructField<'ast> {
    fn from(value: &StructField<'ast>) -> Self {
        Self {
            name: value.name,
            ty: value.ty.into(),
            location: value.location,
        }
    }
}

#[derive(Debug)]
struct TypedBoolExpr {
    value: bool,
    location: Location,
    ty: ResolvedType,
}

#[derive(Debug)]
struct TypedVarExpr<'src> {
    mutable: bool,
    name: &'src str,
    ty: ResolvedType,
    value: Box<TypedExpression<'src>>,
    location: Location,
}

#[derive(Debug)]
struct TypedFloatExpr {
    value: f64,
    location: Location,
    size: Option<FloatSizes>,
    ty: ResolvedType,
}

#[derive(Debug)]
struct TypedBlockExpr<'src> {
    body: Vec<TypedExpression<'src>>,
    trailing_expr: Option<Box<TypedExpression<'src>>>,
    location: Location,
    ty: ResolvedType,
}

#[derive(Debug)]
struct TypedFunExpr<'src> {
    pub name: &'src str,
    pub arguments: Vec<TypedFunArgument<'src>>,
    pub body: Box<TypedExpression<'src>>,
    pub return_type: ResolvedType,
    pub location: Location,
}

#[derive(Debug)]
struct TypedFunArgument<'src> {
    pub name: &'src str,
    pub ty: ResolvedType,
    pub location: Location,
}

#[derive(Debug)]
struct TypedIdentExpr<'src> {
    name: &'src str,
    ty: ResolvedType,
    location: Location,
}

#[derive(Debug)]
enum TypedExpression<'src> {
    Bool(TypedBoolExpr),
    Var(TypedVarExpr<'src>),
    Float(TypedFloatExpr),
    Block(TypedBlockExpr<'src>),
    Fun(TypedFunExpr<'src>),
    Ident(TypedIdentExpr<'src>),
}

impl<'src> TypedExpression<'src> {
    fn ty(&self) -> &ResolvedType {
        match self {
            TypedExpression::Bool(expr) => &expr.ty,
            TypedExpression::Var(expr) => &expr.ty,
            TypedExpression::Float(expr) => &expr.ty,
            TypedExpression::Block(expr) => &expr.ty,
            TypedExpression::Ident(expr) => &expr.ty,
            TypedExpression::Fun(expr) => &expr.return_type,
        }
    }
}

#[derive(Debug)]
pub struct Typer<'src> {
    types: Vec<TypeValue<'src>>,
    type_ids: HashMap<&'src str, TypeId>,
    functions: HashMap<&'src str, ResolvedFunDecl<'src>>,
    symbol_table: HashMap<&'src str, ResolvedType>,
    next_type_id: TypeId,
}

impl<'src> Typer<'src> {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            type_ids: HashMap::new(),
            next_type_id: TypeId(0),
            functions: HashMap::new(),
            symbol_table: HashMap::new(),
        }
    }

    pub fn typecheck_ast(&mut self, ast: Vec<Statement<'src>>) {
        self.collect_type_declarations(&ast);
        self.resolve_type_declarations();
        self.collect_function_declarations(&ast);

        let mut typed_ast = vec![];

        for statement in ast {
            let Statement::Fun(fun) = statement else { continue };
            typed_ast.push(self.typecheck_function(fun));
        }

        println!("{typed_ast:#?}");
    }

    fn collect_type_declarations(&mut self, ast: &[Statement<'src>]) {
        for statement in ast {
            match statement {
                Statement::Struct { name, fields, location } => {
                    let fields = fields.iter().map(Into::into).collect();
                    let type_id = self.next_type_id;

                    let st = ResolvedStruct {
                        name,
                        fields,
                        type_id,
                        location: *location,
                    };

                    self.types.push(TypeValue::Struct(st));
                    self.type_ids.insert(name, type_id);
                    self.next_type_id = self.next_type_id.next();
                }
                Statement::Fun { .. } => {}
            }
        }
    }

    fn resolve_type_declarations(&mut self) {
        for ty in self.types.iter_mut() {
            let TypeValue::Struct(st) = ty else { continue };

            for field in st.fields.iter_mut() {
                let TypeStatus::Pending(named) = field.ty else { continue };
                let Some(type_id) = self.type_ids.get(named.name) else { unreachable!() };
                field.ty = TypeStatus::Resolved(ResolvedType::Struct(*type_id));
            }
        }
    }

    fn collect_function_declarations(&mut self, ast: &[Statement<'src>]) {
        for statement in ast {
            let Statement::Fun(fun) = statement else { continue };

            let mut typed_args = vec![];
            for arg in fun.arguments.iter() {
                let arg_ty = match arg.ty {
                    Type::Primitive(primitive) => ResolvedType::Primitive(primitive),
                    Type::Named(named_type) => {
                        let Some(type_id) = self.type_ids.get(named_type.name) else { unreachable!() };
                        ResolvedType::Struct(*type_id)
                    }
                };

                typed_args.push(ResolvedFunArgDecl {
                    name: arg.name,
                    ty: arg_ty,
                    location: arg.location,
                });
            }

            let return_type = match fun.return_type {
                Some(ty) => match ty {
                    Type::Primitive(primitive) => ResolvedType::Primitive(primitive),
                    Type::Named(named_type) => {
                        let Some(type_id) = self.type_ids.get(named_type.name) else { unreachable!() };
                        ResolvedType::Struct(*type_id)
                    }
                },
                None => ResolvedType::Primitive(PrimitiveType {
                    kind: PrimitiveTypeKind::Unit,
                    // FIXME: what to do here? cuz the function has no return on the code itself,
                    // so the location is not really right
                    location: Default::default(),
                }),
            };

            let declaration = ResolvedFunDecl {
                name: fun.name,
                location: fun.location,
                args: typed_args,
                return_type,
            };

            self.functions.insert(fun.name, declaration);
        }
    }

    fn typecheck_function(&mut self, fun: FunStatement<'src>) -> miette::Result<TypedExpression<'src>> {
        let signature = self.functions.get(fun.name).expect("typechecking undeclared function");

        let typed_args = signature
            .args
            .iter()
            .map(|arg| TypedFunArgument {
                name: arg.name,
                ty: arg.ty.clone(),
                location: arg.location,
            })
            .collect();

        let return_type = signature.return_type.clone();
        let typed_body = self.typecheck_expression(&fun.body, Some(&return_type))?;

        Ok(TypedExpression::Fun(TypedFunExpr {
            name: fun.name,
            arguments: typed_args,
            return_type: typed_body.ty().clone(),
            location: fun.location,
            body: Box::new(typed_body),
        }))
    }

    fn typecheck_expression(
        &mut self,
        expr: &Expression<'src>,
        expected_type: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        match expr {
            Expression::Bool(expr) => Ok(TypedExpression::Bool(TypedBoolExpr {
                value: expr.value,
                location: expr.location,
                ty: ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::Bool, expr.location)),
            })),

            Expression::Var(expr) => self.typecheck_binding(expr, expected_type),
            Expression::FloatLiteral(expr) => self.typecheck_float_lit(expr, expected_type),

            Expression::IntLiteral(expr) => todo!(),

            Expression::If(expr) => todo!(),
            Expression::FunCall(expr) => todo!(),
            Expression::Assign(expr) => todo!(),
            Expression::Ident(expr) => self.typecheck_ident(expr, expected_type),
            Expression::Block(expr) => self.typecheck_block(expr, expected_type),
            Expression::BinaryOp(expr) => self.typecheck_binary_op(expr, expected_type),
            Expression::Return(expr) => todo!(),
            Expression::String(expr) => todo!(),
        }
    }

    fn typecheck_block(
        &mut self,
        expr: &BlockExpr<'src>,
        expected_type: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let mut typed_block = vec![];

        for expr in expr.body.iter() {
            let typed_expr = self.typecheck_expression(expr, expected_type)?;
            typed_block.push(typed_expr);
        }

        let trailing_expr = match expr.trailing_expr.as_ref() {
            Some(expr) => Some(Box::new(self.typecheck_expression(expr.as_ref(), expected_type)?)),
            None => None,
        };

        let ty = match trailing_expr {
            Some(ref expr) => expr.ty().clone(),
            None => ResolvedType::Primitive(PrimitiveType {
                kind: PrimitiveTypeKind::Unit,
                location: Location::default(),
            }),
        };

        Ok(TypedExpression::Block(TypedBlockExpr {
            body: typed_block,
            trailing_expr,
            location: expr.location,
            ty,
        }))
    }

    fn typecheck_ident(
        &mut self,
        expr: &IdentExpr<'src>,
        _: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let Some(ident_type) = self.symbol_table.get(expr.name) else { unreachable!() };

        Ok(TypedExpression::Ident(TypedIdentExpr {
            name: expr.name,
            ty: ident_type.clone(),
            location: expr.location,
        }))
    }

    fn typecheck_binary_op(
        &mut self,
        expr: &BinaryOpExpr<'src>,
        _: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let typed_lhs = self.typecheck_expression(expr.lhs.as_ref(), None);
        let typed_rhs = self.typecheck_expression(expr.rhs.as_ref(), None);

        println!("{typed_lhs:#?}");
        println!("{typed_rhs:#?}");

        todo!()
    }

    fn typecheck_binding(
        &mut self,
        expr: &VarExpr<'src>,
        _: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let variable_ty = match expr.ty {
            None => None,
            Some(ty) => match ty {
                Type::Primitive(primitive) => Some(ResolvedType::Primitive(primitive)),
                Type::Named(named) => {
                    let Some(type_id) = self.type_ids.get(named.name) else { unreachable!() };
                    Some(ResolvedType::Struct(*type_id))
                }
            },
        };

        let value = self.typecheck_expression(expr.value.as_ref(), variable_ty.as_ref())?;
        self.symbol_table.insert(expr.name, value.ty().clone());

        Ok(TypedExpression::Var(TypedVarExpr {
            mutable: expr.mutable,
            name: expr.name,
            ty: value.ty().clone(),
            location: expr.location,
            value: Box::new(value),
        }))
    }

    fn typecheck_float_lit(
        &self,
        expr: &FloatLiteralExpr,
        expected_type: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        println!("{expected_type:#?}");
        let ty = match expected_type {
            // TODO: typecheck that it expects a float, or type error
            Some(_) => todo!(),
            None => ResolvedType::Primitive(PrimitiveType {
                kind: PrimitiveTypeKind::F32,
                location: Location::default(),
            }),
        };

        if let Some(size) = expr.size
            && matches!(size, FloatSizes::F32)
            && expr.value > f32::MAX as f64
        {
            // TODO: error here, float don't fit "announced" type
            unreachable!()
        }

        Ok(TypedExpression::Float(TypedFloatExpr {
            location: expr.location,
            value: expr.value,
            size: expr.size,
            ty,
        }))
    }
}

impl<'ast> Default for Typer<'ast> {
    fn default() -> Self {
        Self::new()
    }
}
