mod error;

use std::collections::HashMap;
use std::sync::Arc;

use error::Error;
use kura_lexer::token::primitive::{FloatSizes, IntSize, Numeral, Signedness};
use kura_lexer::token::{Location, Operator};
use kura_parser::ast::*;
use miette::NamedSource;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct FunctionId(usize);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct TypeId(usize);

impl TypeId {
    pub fn next(&self) -> Self {
        Self(self.0 + 1)
    }
}

#[derive(Debug, Clone)]
struct ResolvedFunDecl<'src> {
    name: &'src str,
    return_type: ResolvedType,
    location: Location,
    args: Vec<ResolvedFunArgDecl<'src>>,
}

#[derive(Debug, Clone)]
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
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ResolvedType {
    Primitive(PrimitiveType),
    Struct(TypeId),
}

impl ResolvedType {
    fn name(&self, types: &[TypeValue<'_>]) -> String {
        match self {
            ResolvedType::Primitive(primitive) => format!("{}", primitive.kind),
            ResolvedType::Struct(type_id) => match &types[type_id.0] {
                TypeValue::Struct(st) => st.name.to_string(),
            },
        }
    }
}

#[derive(Debug)]
enum TypeStatus<'ast> {
    Resolved(ResolvedType),
    Pending(NamedType<'ast>),
}

impl<'ast> From<Type<'ast>> for TypeStatus<'ast> {
    fn from(value: Type<'ast>) -> Self {
        match value {
            Type::Primitive(primitive, _) => TypeStatus::Resolved(ResolvedType::Primitive(primitive)),
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
pub struct TypedBoolExpr {
    value: bool,
    location: Location,
    ty: ResolvedType,
}

#[derive(Debug)]
pub struct TypedVarExpr<'src> {
    mutable: bool,
    name: &'src str,
    ty: ResolvedType,
    value: Box<TypedExpression<'src>>,
    location: Location,
}

#[derive(Debug)]
pub struct TypedFloatExpr {
    value: f64,
    location: Location,
    size: Option<FloatSizes>,
    ty: ResolvedType,
}

#[derive(Debug)]
pub struct TypedBlockExpr<'src> {
    body: Vec<TypedExpression<'src>>,
    trailing_expr: Option<Box<TypedExpression<'src>>>,
    location: Location,
    ty: ResolvedType,
}

#[derive(Debug)]
pub struct TypedFunStatement<'src> {
    pub name: &'src str,
    pub arguments: Vec<TypedFunArgument<'src>>,
    pub body: Box<TypedExpression<'src>>,
    pub return_type: ResolvedType,
    pub location: Location,
}

#[derive(Debug)]
pub struct TypedFunArgument<'src> {
    pub name: &'src str,
    pub ty: ResolvedType,
    pub location: Location,
}

#[derive(Debug)]
pub struct TypedIdentExpr<'src> {
    name: &'src str,
    ty: ResolvedType,
    location: Location,
}

#[derive(Debug)]
pub struct TypedBinaryOpExpr<'ast> {
    operator: Operator,
    lhs: Box<TypedExpression<'ast>>,
    rhs: Box<TypedExpression<'ast>>,
    ty: ResolvedType,
    location: Location,
}

#[derive(Debug)]
pub enum TypedStatement<'src> {
    Fun(TypedFunStatement<'src>),
}

#[derive(Debug)]
pub struct TypedAssignExpr<'src> {
    ident: TypedIdentExpr<'src>,
    location: Location,
    value: Box<TypedExpression<'src>>,
    ty: ResolvedType,
}

#[derive(Debug)]
pub struct TypedIntExpr {
    value: Numeral,
    size: Option<IntSize>,
    location: Location,
    ty: ResolvedType,
}

#[derive(Debug)]
pub struct TypedStringExpr<'src> {
    value: &'src str,
    location: Location,
    ty: ResolvedType,
}

#[derive(Debug)]
pub struct TypedFunCallExpr<'src> {
    ident: &'src str,
    location: Location,
    arguments: Vec<TypedExpression<'src>>,
    ty: ResolvedType,
}

#[derive(Debug)]
pub struct TypedIfExpr<'src> {
    condition: Box<TypedExpression<'src>>,
    location: Location,
    truthy: Box<TypedExpression<'src>>,
    // each `else` or `else if` are added here, if nothing, then
    // this will be an empty vec
    falsy: Vec<TypedExpression<'src>>,
    ty: ResolvedType,
}

#[derive(Debug)]
pub struct TypedReturnExpr<'src> {
    value: Option<Box<TypedExpression<'src>>>,
    location: Location,
    ty: ResolvedType,
}

#[derive(Debug)]
pub enum TypedExpression<'src> {
    Bool(TypedBoolExpr),
    Var(TypedVarExpr<'src>),
    Float(TypedFloatExpr),
    Int(TypedIntExpr),
    Block(TypedBlockExpr<'src>),
    Ident(TypedIdentExpr<'src>),
    BinaryOp(TypedBinaryOpExpr<'src>),
    Assign(TypedAssignExpr<'src>),
    String(TypedStringExpr<'src>),
    FunCall(TypedFunCallExpr<'src>),
    If(TypedIfExpr<'src>),
    Return(TypedReturnExpr<'src>),
}

impl<'src> TypedExpression<'src> {
    fn ty(&self) -> &ResolvedType {
        match self {
            TypedExpression::Bool(expr) => &expr.ty,
            TypedExpression::Var(expr) => &expr.ty,
            TypedExpression::Float(expr) => &expr.ty,
            TypedExpression::Int(expr) => &expr.ty,
            TypedExpression::Block(expr) => &expr.ty,
            TypedExpression::Ident(expr) => &expr.ty,
            TypedExpression::BinaryOp(expr) => &expr.ty,
            TypedExpression::Assign(expr) => &expr.ty,
            TypedExpression::String(expr) => &expr.ty,
            TypedExpression::FunCall(expr) => &expr.ty,
            TypedExpression::If(expr) => &expr.ty,
            TypedExpression::Return(expr) => &expr.ty,
        }
    }

    fn location(&self) -> Location {
        match self {
            TypedExpression::Bool(expr) => expr.location,
            TypedExpression::Var(expr) => expr.location,
            TypedExpression::Float(expr) => expr.location,
            TypedExpression::Block(expr) => expr.location,
            TypedExpression::Ident(expr) => expr.location,
            TypedExpression::BinaryOp(expr) => expr.location,
            TypedExpression::Assign(expr) => expr.location,
            TypedExpression::Int(expr) => expr.location,
            TypedExpression::String(expr) => expr.location,
            TypedExpression::FunCall(expr) => expr.location,
            TypedExpression::If(expr) => expr.location,
            TypedExpression::Return(expr) => expr.location,
        }
    }
}

#[derive(Debug, Clone)]
struct Symbol {
    mutable: bool,
    ty: ResolvedType,
}

impl Symbol {
    pub fn new(mutable: bool, ty: ResolvedType) -> Self {
        Self { mutable, ty }
    }
}

#[derive(Debug)]
pub struct Typer<'src> {
    types: Vec<TypeValue<'src>>,
    type_ids: HashMap<&'src str, TypeId>,
    functions: HashMap<&'src str, ResolvedFunDecl<'src>>,
    scopes: Vec<HashMap<&'src str, Symbol>>,
    symbol_table: HashMap<&'src str, Symbol>,
    next_type_id: TypeId,
    source: Arc<String>,
}

impl<'src> Typer<'src> {
    pub fn new(source: Arc<String>) -> Self {
        Self {
            types: Vec::new(),
            type_ids: HashMap::new(),
            next_type_id: TypeId(0),
            functions: HashMap::new(),
            symbol_table: HashMap::new(),
            scopes: Vec::new(),
            source,
        }
    }

    pub fn check(&mut self, ast: Vec<Statement<'src>>) -> miette::Result<Vec<TypedStatement<'src>>> {
        self.collect_type_declarations(&ast);
        self.resolve_type_declarations();
        self.collect_function_declarations(&ast);

        let mut typed_ast = vec![];

        for statement in ast {
            let Statement::Fun(fun) = statement else { continue };
            typed_ast.push(self.typecheck_function(fun)?);
        }

        Ok(typed_ast)
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
            let TypeValue::Struct(st) = ty;

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
                    Type::Primitive(primitive, _) => ResolvedType::Primitive(primitive),
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
                    Type::Primitive(primitive, _) => ResolvedType::Primitive(primitive),
                    Type::Named(named_type) => {
                        let Some(type_id) = self.type_ids.get(named_type.name) else { unreachable!() };
                        ResolvedType::Struct(*type_id)
                    }
                },
                None => ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::Unit)),
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

    fn typecheck_function(&mut self, fun: FunStatement<'src>) -> miette::Result<TypedStatement<'src>> {
        self.scopes.push(HashMap::new());

        let signature = self
            .functions
            .get(fun.name)
            .expect("typechecking undeclared function")
            .clone();

        let typed_args = signature
            .args
            .iter()
            .map(|arg| TypedFunArgument {
                name: arg.name,
                ty: arg.ty.clone(),
                location: arg.location,
            })
            .collect::<Vec<_>>();

        typed_args
            .iter()
            .for_each(|arg| self.add_to_scope(arg.name, Symbol::new(false, arg.ty.clone())));

        let return_type = signature.return_type.clone();
        let typed_body = self.typecheck_expression(&fun.body, Some(&return_type))?;

        Ok(TypedStatement::Fun(TypedFunStatement {
            name: fun.name,
            arguments: typed_args,
            return_type: typed_body.ty().clone(),
            location: fun.location,
            body: Box::new(typed_body),
        }))
    }

    fn add_to_scope(&mut self, name: &'src str, symbol: Symbol) {
        let scope = self.scopes.last_mut().expect("should always have at least one scope");
        scope.insert(name, symbol);
    }

    fn get_from_scope(&mut self, name: &'src str) -> Option<Symbol> {
        for scope in self.scopes.iter_mut().rev() {
            let symbol = scope.get(name).cloned();
            if symbol.is_some() {
                return symbol;
            }
        }

        None
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
                ty: ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::Bool)),
            })),

            Expression::Var(expr) => self.typecheck_binding(expr, expected_type),
            Expression::FloatLiteral(expr) => self.typecheck_float_lit(expr, expected_type),
            Expression::IntLiteral(expr) => self.typecheck_int_lit(expr, expected_type),

            Expression::If(expr) => self.typecheck_if_stmt(expr, expected_type),
            Expression::FunCall(expr) => self.typecheck_function_call(expr, expected_type),
            Expression::Assign(expr) => self.typecheck_assign(expr),
            Expression::Ident(expr) => self.typecheck_ident(expr, expected_type),
            Expression::Block(expr) => self.typecheck_block(expr, expected_type),
            Expression::BinaryOp(expr) => self.typecheck_binary_op(expr, expected_type),
            Expression::Return(expr) => self.typecheck_return(expr, expected_type),
            Expression::String(expr) => self.typecheck_string(expr, expected_type),
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
            None => ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::Unit)),
        };

        // TODO: handle return statements and trailing expression types

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
        expected_type: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let Some(ident_type) = self.get_from_scope(expr.name) else {
            return Err(Error::new(
                expr.location,
                format!("Reference to undeclared identifier {}", expr.name),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        };

        if let Some(expected_type) = expected_type {
            if !self.type_equals(&ident_type.ty, expected_type) {
                return Err(Error::new(
                    expr.location,
                    "Type not allowed here".into(),
                    None,
                    NamedSource::new("file.kr", self.source.clone()),
                )
                .into());
            }
        }

        Ok(TypedExpression::Ident(TypedIdentExpr {
            name: expr.name,
            ty: ident_type.ty.clone(),
            location: expr.location,
        }))
    }

    fn typecheck_binary_op(
        &mut self,
        expr: &BinaryOpExpr<'src>,
        _: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let typed_lhs = self.typecheck_expression(expr.lhs.as_ref(), None)?;
        let typed_rhs = self.typecheck_expression(expr.rhs.as_ref(), None)?;

        if !self.valid_type_for_operator(&typed_lhs, expr.operator) {
            return Err(Error::new(
                typed_lhs.location(),
                format!("Binary op {} cannot be performed on this type", expr.operator),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        }

        if !self.type_equals(typed_lhs.ty(), typed_rhs.ty()) {
            let lhs_name = self.get_type_name(&typed_lhs);
            let rhs_name = self.get_type_name(&typed_rhs);

            return Err(Error::new(
                typed_rhs.location(),
                format!("Cannot apply {} between {lhs_name} and {rhs_name}", expr.operator),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        }

        Ok(TypedExpression::BinaryOp(TypedBinaryOpExpr {
            ty: typed_lhs.ty().clone(),
            operator: expr.operator,
            lhs: Box::new(typed_lhs),
            rhs: Box::new(typed_rhs),
            location: expr.location,
        }))
    }

    fn typecheck_binding(
        &mut self,
        expr: &VarExpr<'src>,
        _: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let variable_ty = match expr.ty {
            None => None,
            Some(ty) => match ty {
                Type::Primitive(primitive, _) => Some(ResolvedType::Primitive(primitive)),
                Type::Named(named) => {
                    let Some(type_id) = self.type_ids.get(named.name) else {
                        return Err(Error::new(
                            ty.location(),
                            format!("Reference to undeclared type `{}`", named.name),
                            None,
                            NamedSource::new("file.kr", self.source.clone()),
                        )
                        .into());
                    };
                    Some(ResolvedType::Struct(*type_id))
                }
            },
        };

        let value = self.typecheck_expression(expr.value.as_ref(), variable_ty.as_ref())?;

        if let Some(ref ty) = variable_ty
            && !self.type_equals(ty, value.ty())
        {
            return Err(Error::new(
                value.location(),
                format!(
                    "Value of type `{}` cannot be assigned to variable of type `{}`",
                    value.ty().name(&self.types),
                    ty.name(&self.types),
                ),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        }

        let symbol = Symbol {
            mutable: expr.mutable,
            ty: value.ty().clone(),
        };

        self.add_to_scope(expr.name, symbol);

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
        let ty = match (expected_type, expr.size) {
            (Some(_), None) => ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::F32)),
            (Some(_), Some(_)) => ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::F32)),
            (None, Some(_)) => ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::F32)),
            // No specified type and also no type suffix, so this is inferred as f32 unless it is
            // too big which we will error asking for a type
            (None, None) => {
                if expr.value > f32::MAX as f64 {
                    return Err(Error::new(
                        expr.location,
                        format!(
                            "Float literal {} is too large to fit an f32, please specify a type for it",
                            expr.value
                        ),
                        Some(format!("try {}f64", expr.value)),
                        NamedSource::new("file.kr", self.source.clone()),
                    )
                    .into());
                }

                ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::F32))
            }
        };

        Ok(TypedExpression::Float(TypedFloatExpr {
            location: expr.location,
            value: expr.value,
            size: expr.size,
            ty,
        }))
    }

    fn typecheck_int_lit(
        &mut self,
        expr: &IntLiteralExpr,
        expected_type: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let value_sign = expr.value.signedness();

        let expected_size = match expected_type {
            Some(ty) => match ty {
                ResolvedType::Primitive(primitive) => match primitive.kind {
                    PrimitiveTypeKind::U8 => Some(IntSize::U8),
                    PrimitiveTypeKind::U16 => Some(IntSize::U16),
                    PrimitiveTypeKind::U32 => Some(IntSize::U32),
                    PrimitiveTypeKind::U64 => Some(IntSize::U64),
                    PrimitiveTypeKind::Usize => Some(IntSize::Usize),
                    PrimitiveTypeKind::I8 => Some(IntSize::I8),
                    PrimitiveTypeKind::I16 => Some(IntSize::I16),
                    PrimitiveTypeKind::I32 => Some(IntSize::I32),
                    PrimitiveTypeKind::I64 => Some(IntSize::I64),
                    PrimitiveTypeKind::Isize => Some(IntSize::Isize),

                    PrimitiveTypeKind::Unit
                    | PrimitiveTypeKind::String
                    | PrimitiveTypeKind::F32
                    | PrimitiveTypeKind::F64
                    | PrimitiveTypeKind::Bool => {
                        return Err(Error::new(
                            expr.location,
                            format!(
                                "Integer literal of type `{}` cannot be assigned to type `{}`",
                                expr.size.unwrap_or(IntSize::I32),
                                ty.name(&self.types),
                            ),
                            None,
                            NamedSource::new("file.kr", self.source.clone()),
                        )
                        .into());
                    }
                },
                ResolvedType::Struct(_) => {
                    return Err(Error::new(
                        expr.location,
                        format!(
                            "Integer literal of type `{}` cannot be assigned to type `{}`",
                            expr.size.unwrap_or(IntSize::I32),
                            ty.name(&self.types),
                        ),
                        None,
                        NamedSource::new("file.kr", self.source.clone()),
                    )
                    .into());
                }
            },
            None => None,
        };

        // if integer has a specified size suffix, it wins
        if let Some(size) = expr.size {
            // signed literal + unsigned suffix = error
            if matches!(value_sign, Signedness::Signed) && matches!(size.signedness(), Signedness::Unsigned) {
                return Err(Error::new(
                    expr.location,
                    format!(
                        "Signed integer `{}` with `{}` suffix specifier",
                        expr.value.signed_inner(),
                        size
                    ),
                    None,
                    NamedSource::new("file.kr", self.source.clone()),
                )
                .into());
            }

            if !integer_fits(size, expr.value) {
                let msg = match int_size_bounds(size) {
                    None => "internal: no bounds for isize/usize".to_string(),
                    Some((min, max, Signedness::Signed)) => {
                        format!("Integer literal out of range for {size} (allowed {min}..={max})",)
                    }
                    Some((_, max, Signedness::Unsigned)) => {
                        format!("Integer literal out of range for {size} (allowed 0..={max})")
                    }
                };

                return Err(Error::new(
                    expr.location,
                    msg,
                    None,
                    NamedSource::new("file.kr", self.source.clone()),
                )
                .into());
            }

            // if we have both a size and expected_type then they must match exactly
            if let Some(expected_size) = expected_size {
                if expected_size != size {
                    return Err(Error::new(
                        expr.location,
                        format!("Literal is annotated as `{size}` but `{expected_size}` was expected"),
                        None,
                        NamedSource::new("file.kr", self.source.clone()),
                    )
                    .into());
                }
            }

            return Ok(TypedExpression::Int(TypedIntExpr {
                value: expr.value,
                size: expr.size,
                location: expr.location,
                ty: ResolvedType::Primitive(PrimitiveType::new(size.into())),
            }));
        }

        if let Some(expected_size) = expected_size {
            if matches!(expected_size.signedness(), Signedness::Unsigned) && matches!(value_sign, Signedness::Signed) {
                return Err(Error::new(
                    expr.location,
                    format!(
                        "Signed integer `{}` does not match expected unsigned type `{expected_size}`",
                        expr.value.signed_inner()
                    ),
                    None,
                    NamedSource::new("file.kr", self.source.clone()),
                )
                .into());
            }

            if !integer_fits(expected_size, expr.value) {
                let msg = match int_size_bounds(expected_size) {
                    None => format!("Integer literal may not fit in `{expected_size}` on this target"),
                    Some((min, max, Signedness::Signed)) => {
                        format!("Integer literal out of range for {expected_size} (allowed {min}..={max})",)
                    }
                    Some((_, max, Signedness::Unsigned)) => {
                        format!("Integer literal out of range for {expected_size} (allowed 0..={max})")
                    }
                };
                return Err(Error::new(
                    expr.location,
                    msg,
                    None,
                    NamedSource::new("file.kr", self.source.clone()),
                )
                .into());
            }

            return Ok(TypedExpression::Int(TypedIntExpr {
                value: expr.value,
                size: expr.size,
                location: expr.location,
                ty: ResolvedType::Primitive(PrimitiveType::new(expected_size.into())),
            }));
        }

        let default_size = IntSize::I32;
        if !integer_fits(default_size, expr.value) {
            return Err(Error::new(
                expr.location,
                format!("Integer literal {} is too large for a `i32`", expr.value),
                Some("consider adding a suffix (e.g., i64, u64)".into()),
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        }

        Ok(TypedExpression::Int(TypedIntExpr {
            value: expr.value,
            size: expr.size,
            location: expr.location,
            ty: ResolvedType::Primitive(PrimitiveType::new(default_size.into())),
        }))
    }

    fn typecheck_assign(&mut self, expr: &AssignExpr<'src>) -> miette::Result<TypedExpression<'src>> {
        let Some(symbol) = self.symbol_table.get(expr.ident.name).cloned() else {
            return Err(Error::new(
                expr.location,
                format!("Assignment to undeclared variable `{}`", expr.ident.name),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        };

        let typed_ident = TypedIdentExpr {
            name: expr.ident.name,
            ty: symbol.ty.clone(),
            location: expr.ident.location,
        };

        if !symbol.mutable {
            return Err(Error::new(
                expr.location,
                format!("Assigment to non-mutable binding `{}`", expr.ident.name),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        }

        let typed_value = self.typecheck_expression(expr.value.as_ref(), Some(&symbol.ty))?;
        if !self.type_equals(&symbol.ty, typed_value.ty()) {
            return Err(Error::new(
                typed_value.location(),
                format!(
                    "Cannot assign value of type `{}` to variable of type `{}`",
                    typed_value.ty().name(&self.types),
                    symbol.ty.name(&self.types)
                ),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        }

        Ok(TypedExpression::Assign(TypedAssignExpr {
            ty: typed_ident.ty.clone(),
            ident: typed_ident,
            location: expr.location,
            value: Box::new(typed_value),
        }))
    }

    fn typecheck_string(
        &mut self,
        expr: &StringExpr<'src>,
        expected_type: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let ty = ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::String));

        if let Some(et) = expected_type
            && &ty != et
        {
            return Err(Error::new(
                expr.location,
                format!(
                    "Value of type string is not valid where a `{}` is expected",
                    et.name(&self.types)
                ),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        }

        Ok(TypedExpression::String(TypedStringExpr {
            value: expr.value,
            location: expr.location,
            ty,
        }))
    }

    fn typecheck_function_call(
        &mut self,
        expr: &FunCallExpr<'src>,
        expected_type: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let Some(signature) = self.functions.get(expr.ident).cloned() else {
            return Err(Error::new(
                expr.location,
                format!("Call to undeclared function {}", expr.ident),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        };

        if expr.arguments.len() != signature.args.len() {
            return Err(Error::new(
                expr.location,
                format!(
                    "Function takes {} arguments, but {} were provided",
                    signature.args.len(),
                    expr.arguments.len()
                ),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        }

        let mut typed_args = vec![];
        for (idx, argument) in expr.arguments.iter().enumerate() {
            typed_args.push(self.typecheck_expression(argument, Some(&signature.args[idx].ty))?);
        }

        if let Some(expected_type) = expected_type
            && !self.type_equals(&signature.return_type, expected_type)
        {
            return Err(Error::new(
                expr.location,
                format!(
                    "Function call returns type {} which doesn't match type {}",
                    signature.return_type.name(&self.types),
                    expected_type.name(&self.types)
                ),
                None,
                NamedSource::new("file.kr", self.source.clone()),
            )
            .into());
        }

        Ok(TypedExpression::FunCall(TypedFunCallExpr {
            ident: expr.ident,
            arguments: typed_args,
            location: expr.location,
            ty: signature.return_type,
        }))
    }

    fn typecheck_if_stmt(
        &mut self,
        expr: &IfExpr<'src>,
        expected_type: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let condition = self.typecheck_expression(
            expr.condition.as_ref(),
            Some(&ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::Bool))),
        )?;

        let truthy = self.typecheck_expression(expr.truthy.as_ref(), expected_type)?;

        let mut falsy = vec![];
        for expr in expr.falsy.iter() {
            falsy.push(self.typecheck_expression(expr, expected_type)?);
        }

        Ok(TypedExpression::If(TypedIfExpr {
            ty: truthy.ty().clone(),
            condition: Box::new(condition),
            location: expr.location,
            falsy,
            truthy: Box::new(truthy),
        }))
    }

    fn typecheck_return(
        &mut self,
        expr: &ReturnExpr<'src>,
        expected_type: Option<&ResolvedType>,
    ) -> miette::Result<TypedExpression<'src>> {
        let value = match expr.value.as_ref() {
            Some(value) => Some(Box::new(self.typecheck_expression(value, expected_type)?)),
            None => None,
        };

        let ty = value
            .as_ref()
            .map(|val| val.ty().clone())
            .unwrap_or(ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::Unit)));

        Ok(TypedExpression::Return(TypedReturnExpr {
            location: expr.location,
            value,
            ty,
        }))
    }

    fn type_equals(&self, a: &ResolvedType, b: &ResolvedType) -> bool {
        match (a, b) {
            (ResolvedType::Primitive(a), ResolvedType::Primitive(b)) => a.kind == b.kind,
            (ResolvedType::Struct(a), ResolvedType::Struct(b)) => a == b,
            (ResolvedType::Primitive(_), ResolvedType::Struct(_)) => false,
            (ResolvedType::Struct(_), ResolvedType::Primitive(_)) => false,
        }
    }

    fn valid_type_for_operator(&self, expr: &TypedExpression<'src>, op: Operator) -> bool {
        match op {
            Operator::Minus
            | Operator::Plus
            | Operator::Slash
            | Operator::Less
            | Operator::LessEqual
            | Operator::Greater
            | Operator::GreaterEqual
            | Operator::NotEqual
            | Operator::Star => match expr.ty() {
                ResolvedType::Primitive(primitive) => primitive.is_numeral(),
                ResolvedType::Struct(_) => false,
            },

            Operator::LeftParen => todo!(),
            Operator::RightParen => todo!(),
            Operator::LeftBracket => todo!(),
            Operator::RightBracket => todo!(),
            Operator::LeftBrace => todo!(),
            Operator::RightBrace => todo!(),
            Operator::Comma => todo!(),
            Operator::Dot => todo!(),
            Operator::MinusEqual => todo!(),
            Operator::PlusEqual => todo!(),
            Operator::Equal => todo!(),
            Operator::ThickArrow => todo!(),
            Operator::EqualEqual => todo!(),
            Operator::StarEqual => todo!(),
            Operator::Ampersand => todo!(),
            Operator::SlashEqual => todo!(),
            Operator::Colon => todo!(),
            Operator::SemiColon => todo!(),
            Operator::Bang => todo!(),
            Operator::And => todo!(),
            Operator::Or => todo!(),
        }
    }

    fn get_type_name(&self, expr: &TypedExpression<'src>) -> String {
        match expr.ty() {
            ResolvedType::Primitive(primitive) => format!("{}", primitive.kind),
            ResolvedType::Struct(type_id) => match &self.types[type_id.0] {
                TypeValue::Struct(st) => st.name.to_string(),
            },
        }
    }
}

fn integer_fits(size: IntSize, value: Numeral) -> bool {
    match int_size_bounds(size) {
        // if there are no bounds, then it fits.
        None => true,
        Some((min, max, Signedness::Signed)) => match value {
            Numeral::Signed(v) => v >= min && v <= max as i64,
            Numeral::Unsigned(v) => v <= max,
        },
        Some((_, max, Signedness::Unsigned)) => match value {
            Numeral::Signed(_) => false, // signed number on unsigned size
            Numeral::Unsigned(v) => v <= max,
        },
    }
}

fn int_size_bounds(size: IntSize) -> Option<(i64, u64, Signedness)> {
    use IntSize::*;
    Some(match size {
        I8 => (i8::MIN as i64, i8::MAX as u64, Signedness::Signed),
        I16 => (i16::MIN as i64, i16::MAX as u64, Signedness::Signed),
        I32 => (i32::MIN as i64, i32::MAX as u64, Signedness::Signed),
        I64 => (i64::MIN, i64::MAX as u64, Signedness::Signed),

        U8 => (0, u8::MAX as u64, Signedness::Unsigned),
        U16 => (0, u16::MAX as u64, Signedness::Unsigned),
        U32 => (0, u32::MAX as u64, Signedness::Unsigned),
        U64 => (0, u64::MAX, Signedness::Unsigned),

        Isize | Usize => return None, // no bounds here; usize/isize are symbolic for now
    })
}
