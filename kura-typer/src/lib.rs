use std::collections::HashMap;

use kura_lexer::token::Location;
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
    return_type: Option<TypeStatus<'src>>,
    location: Location,
    args: Vec<ResolvedFunArgDecl<'src>>,
}

#[derive(Debug)]
struct ResolvedFunArgDecl<'src> {
    name: &'src str,
    ty: TypeStatus<'src>,
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

#[derive(Debug)]
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
enum TypedExpression {
    Bool(TypedBoolExpr),
}

#[derive(Debug)]
pub struct Typer<'src> {
    types: Vec<TypeValue<'src>>,
    type_ids: HashMap<&'src str, TypeId>,
    functions: HashMap<&'src str, ResolvedFunDecl<'src>>,
    next_type_id: TypeId,
}

impl<'src> Typer<'src> {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            type_ids: HashMap::new(),
            next_type_id: TypeId(0),
            functions: HashMap::new(),
        }
    }

    pub fn typecheck_ast(&mut self, ast: Vec<Statement<'src>>) {
        self.collect_type_declarations(&ast);
        self.resolve_type_declarations();
        self.collect_function_declarations(&ast);

        for statement in ast {
            let Statement::Fun(fun) = statement else { continue };
            self.typecheck_function(fun);
        }

        println!("{self:?}");
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
                    Type::Primitive(primitive) => TypeStatus::Resolved(ResolvedType::Primitive(primitive)),
                    Type::Named(named_type) => {
                        let Some(type_id) = self.type_ids.get(named_type.name) else { unreachable!() };
                        TypeStatus::Resolved(ResolvedType::Struct(*type_id))
                    }
                };
                let arg = ResolvedFunArgDecl {
                    name: arg.name,
                    ty: arg_ty,
                    location: arg.location,
                };
                typed_args.push(arg);
            }

            let return_type = match fun.return_type {
                Some(ty) => match ty {
                    Type::Primitive(primitive) => Some(TypeStatus::Resolved(ResolvedType::Primitive(primitive))),
                    Type::Named(named_type) => {
                        let Some(type_id) = self.type_ids.get(named_type.name) else { unreachable!() };
                        Some(TypeStatus::Resolved(ResolvedType::Struct(*type_id)))
                    }
                },
                None => None,
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

    fn typecheck_expression(&self, expr: &Expression<'src>) -> TypedExpression {
        match expr {
            Expression::Bool(expr) => TypedExpression::Bool(TypedBoolExpr {
                value: expr.value,
                location: expr.location,
                ty: ResolvedType::Primitive(PrimitiveType::new(PrimitiveTypeKind::Bool, expr.location)),
            }),

            Expression::FloatLiteral(expr) => todo!(),
            Expression::IntLiteral(expr) => todo!(),

            Expression::Var(expr) => todo!(),
            Expression::If(expr) => todo!(),
            Expression::FunCall(expr) => todo!(),
            Expression::Assign(expr) => todo!(),
            Expression::Ident(expr) => todo!(),
            Expression::Block(expr) => todo!(),
            Expression::BinaryOp(expr) => todo!(),
            Expression::Return(expr) => todo!(),
            Expression::String(expr) => todo!(),
        }
    }

    fn typecheck_function(&mut self, fun: FunStatement) {
        // let typed_body = self.typecheck_expression(&fun.body);
    }
}

impl<'ast> Default for Typer<'ast> {
    fn default() -> Self {
        Self::new()
    }
}
