use kura_lexer::token::primitive::{FloatSizes, IntSizes, Numeral};
use kura_lexer::token::{Location, Operator};

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

#[derive(Debug)]
pub enum Statement<'ast> {
    Fun {
        name: &'ast str,
        arguments: Vec<FunArgument<'ast>>,
        body: Expression<'ast>,
        return_type: Option<Type<'ast>>,
        location: Location,
    },
}

#[derive(Debug)]
pub struct FunArgument<'ast> {
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
    pub fn from_identifier(name: &'ast str, location: Location) -> Self {
        if let Some((_, ty)) = PRIMITIVE_MAP.iter().find(|(n, _)| name == *n) {
            return Type::Primitive { ty: *ty, location };
        };

        Type::Defined { name, location }
    }

    pub fn location(&self) -> Location {
        match self {
            Self::Primitive { location, .. } => *location,
            Self::Defined { location, .. } => *location,
        }
    }
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
        body: Vec<Expression<'ast>>,
        trailing_expr: Option<Box<Expression<'ast>>>,
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
    pub fn location(&self) -> Location {
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
