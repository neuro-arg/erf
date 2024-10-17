use crate::Span;

pub type Ident = String;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Bool,
    Float,
    Int,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnOp {
    Not,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Call,
    Pow,
    Mul,
    Div,
    FloorDiv,
    Rem,
    Add,
    Sub,
    Shl,
    Shr,
    BitAnd,
    BitXor,
    BitOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprInner {
    Literal(LiteralKind, String),
    Variable(String),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Lambda(Pattern, Box<Expr>),
    LetRec(Pattern, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Tld {
    pub pattern: Pattern,
    pub body: Expr,
    pub span: Span,
}

impl Tld {
    pub fn new(pattern: Pattern, body: Expr, span: Span) -> Tld {
        Self {
            pattern,
            body,
            span,
        }
    }
}

impl ExprInner {
    pub fn literal(kind: LiteralKind, s: impl Into<String>) -> Self {
        Self::Literal(kind, s.into())
    }
    pub fn var(s: impl Into<String>) -> Self {
        Self::Variable(s.into())
    }
    pub fn unary(op: UnOp, expr: impl Into<Box<Expr>>) -> Self {
        Self::UnOp(op, expr.into())
    }
    pub fn binary(op: BinOp, lhs: impl Into<Box<Expr>>, rhs: impl Into<Box<Expr>>) -> Self {
        Self::BinOp(op, lhs.into(), rhs.into())
    }
    pub fn lambda(lhs: impl Into<Pattern>, rhs: impl Into<Box<Expr>>) -> Self {
        Self::Lambda(lhs.into(), rhs.into())
    }
    pub fn letrec(
        target: impl Into<Pattern>,
        expr: impl Into<Box<Expr>>,
        body: impl Into<Box<Expr>>,
    ) -> Self {
        Self::LetRec(target.into(), expr.into(), body.into())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expr {
    pub inner: ExprInner,
    pub span: Span,
}

impl Expr {
    pub fn new(inner: ExprInner, span: Span) -> Self {
        Self { inner, span }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatternInner {
    Variable(String),
}

impl PatternInner {
    pub fn var(s: impl Into<String>) -> Self {
        Self::Variable(s.into())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pattern {
    pub inner: PatternInner,
    pub span: Span,
}

impl Pattern {
    pub fn new(inner: PatternInner, span: Span) -> Self {
        Self { inner, span }
    }
}

impl Pattern {
    pub fn label(&self) -> Option<String> {
        match &self.inner {
            PatternInner::Variable(x) => Some(x.clone()),
        }
    }
}

pub type Program = Vec<Tld>;
