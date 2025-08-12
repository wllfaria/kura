use kura_lexer::token::primitive::{FloatSizes, IntSizes, Numeral};
use kura_lexer::token::{Location, Operator};

static PRIMITIVE_MAP: &[(&str, PrimitiveTypeKind)] = &[
    ("u8", PrimitiveTypeKind::U8),
    ("u16", PrimitiveTypeKind::U16),
    ("u32", PrimitiveTypeKind::U32),
    ("u64", PrimitiveTypeKind::U64),
    ("i8", PrimitiveTypeKind::I8),
    ("i16", PrimitiveTypeKind::I16),
    ("i32", PrimitiveTypeKind::I32),
    ("i64", PrimitiveTypeKind::I64),
    ("bool", PrimitiveTypeKind::Bool),
    ("f32", PrimitiveTypeKind::F32),
    ("f64", PrimitiveTypeKind::F64),
];

#[derive(Debug)]
pub struct FunStatement<'ast> {
    pub name: &'ast str,
    pub arguments: Vec<FunArgument<'ast>>,
    pub body: Expression<'ast>,
    pub return_type: Option<Type<'ast>>,
    pub location: Location,
}

#[derive(Debug)]
pub enum Statement<'ast> {
    Fun(FunStatement<'ast>),
    Struct {
        name: &'ast str,
        fields: Vec<StructField<'ast>>,
        location: Location,
    },
}

#[derive(Debug)]
pub struct FunArgument<'ast> {
    pub name: &'ast str,
    pub ty: Type<'ast>,
    pub location: Location,
}

#[derive(Debug)]
pub struct StructField<'ast> {
    pub name: &'ast str,
    pub ty: Type<'ast>,
    pub location: Location,
}

impl<'ast> FunArgument<'ast> {
    pub fn new(name: &'ast str, ty: Type<'ast>, location: Location) -> Self {
        Self { name, ty, location }
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrimitiveTypeKind {
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

impl From<IntSizes> for PrimitiveTypeKind {
    fn from(size: IntSizes) -> Self {
        match size {
            IntSizes::I8 => PrimitiveTypeKind::I8,
            IntSizes::I16 => PrimitiveTypeKind::I16,
            IntSizes::I32 => PrimitiveTypeKind::I32,
            IntSizes::I64 => PrimitiveTypeKind::I64,
            IntSizes::Isize => PrimitiveTypeKind::Isize,
            IntSizes::U8 => PrimitiveTypeKind::U8,
            IntSizes::U16 => PrimitiveTypeKind::U16,
            IntSizes::U32 => PrimitiveTypeKind::U32,
            IntSizes::U64 => PrimitiveTypeKind::U64,
            IntSizes::Usize => PrimitiveTypeKind::Usize,
        }
    }
}

impl From<FloatSizes> for PrimitiveTypeKind {
    fn from(size: FloatSizes) -> Self {
        match size {
            FloatSizes::F32 => PrimitiveTypeKind::F32,
            FloatSizes::F64 => PrimitiveTypeKind::F64,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrimitiveType {
    pub kind: PrimitiveTypeKind,
    pub location: Location,
}

impl PrimitiveType {
    pub fn new(kind: PrimitiveTypeKind, location: Location) -> Self {
        Self { kind, location }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct NamedType<'ast> {
    pub name: &'ast str,
    pub location: Location,
}

#[derive(Debug, Copy, Clone)]
pub enum Type<'ast> {
    Primitive(PrimitiveType),
    Named(NamedType<'ast>),
}

impl<'ast> Type<'ast> {
    pub fn from_identifier(name: &'ast str, location: Location) -> Self {
        if let Some((_, ty)) = PRIMITIVE_MAP.iter().find(|(n, _)| name == *n) {
            return Type::Primitive(PrimitiveType { kind: *ty, location });
        };

        Type::Named(NamedType { name, location })
    }

    pub fn location(&self) -> Location {
        match self {
            Self::Primitive(primitive) => primitive.location,
            Self::Named(named) => named.location,
        }
    }
}

#[derive(Debug)]
pub struct VarExpr<'src> {
    pub mutable: bool,
    pub name: &'src str,
    pub ty: Option<Type<'src>>,
    pub value: Box<Expression<'src>>,
    pub location: Location,
}

#[derive(Debug)]
pub struct BoolExpr {
    pub value: bool,
    pub location: Location,
}

#[derive(Debug)]
pub struct IfExpr<'src> {
    pub condition: Box<Expression<'src>>,
    pub location: Location,
    pub truthy: Box<Expression<'src>>,
    // each `else` or `else if` are added here, if nothing, then
    // this will be an empty vec
    pub falsy: Vec<Expression<'src>>,
}

#[derive(Debug)]
pub struct FunCallExpr<'ast> {
    pub ident: &'ast str,
    pub location: Location,
    pub arguments: Vec<Expression<'ast>>,
}

#[derive(Debug)]
pub struct AssignExpr<'ast> {
    pub ident: Box<Expression<'ast>>,
    pub location: Location,
    pub value: Box<Expression<'ast>>,
}

#[derive(Debug)]
pub struct IdentExpr<'ast> {
    pub name: &'ast str,
    pub location: Location,
}

#[derive(Debug)]
pub struct BlockExpr<'ast> {
    pub body: Vec<Expression<'ast>>,
    pub trailing_expr: Option<Box<Expression<'ast>>>,
    pub location: Location,
}

#[derive(Debug)]
pub struct FloatLiteralExpr {
    pub value: f64,
    pub size: Option<FloatSizes>,
    pub location: Location,
}

#[derive(Debug)]
pub struct IntLiteralExpr {
    pub value: Numeral,
    pub size: Option<IntSizes>,
    pub location: Location,
}

#[derive(Debug)]
pub struct BinaryOpExpr<'ast> {
    pub operator: Operator,
    pub lhs: Box<Expression<'ast>>,
    pub rhs: Box<Expression<'ast>>,
    pub location: Location,
}

#[derive(Debug)]
pub struct ReturnExpr<'ast> {
    pub value: Box<Expression<'ast>>,
    pub location: Location,
}

#[derive(Debug)]
pub struct StringExpr<'ast> {
    pub value: &'ast str,
    pub location: Location,
}

#[derive(Debug)]
pub enum Expression<'ast> {
    Var(VarExpr<'ast>),
    Bool(BoolExpr),
    If(IfExpr<'ast>),
    FunCall(FunCallExpr<'ast>),
    Assign(AssignExpr<'ast>),
    Ident(IdentExpr<'ast>),
    Block(BlockExpr<'ast>),
    FloatLiteral(FloatLiteralExpr),
    IntLiteral(IntLiteralExpr),
    BinaryOp(BinaryOpExpr<'ast>),
    Return(ReturnExpr<'ast>),
    String(StringExpr<'ast>),
}

impl Expression<'_> {
    pub fn location(&self) -> Location {
        match self {
            Expression::Var(expr) => expr.location,
            Expression::If(expr) => expr.location,
            Expression::Ident(expr) => expr.location,
            Expression::Bool(expr) => expr.location,
            Expression::Block(expr) => expr.location,
            Expression::FunCall(expr) => expr.location,
            Expression::Assign(expr) => expr.location,
            Expression::Return(expr) => expr.location,
            Expression::FloatLiteral(expr) => expr.location,
            Expression::IntLiteral(expr) => expr.location,
            Expression::BinaryOp(expr) => expr.location,
            Expression::String(expr) => expr.location,
        }
    }
}
