use std::collections::BTreeMap;

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
    LetRec(BTreeMap<Ident, Vec<LetArm>>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetArm {
    pub pattern: LetPattern,
    pub body: Expr,
    pub span: Span,
}

impl LetArm {
    pub fn new(pattern: LetPattern, body: Expr, span: Span) -> LetArm {
        Self {
            pattern,
            body,
            span,
        }
    }
}

pub type Tld = LetArm;

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
    pub fn letrec(target: impl IntoIterator<Item = LetArm>, body: impl Into<Box<Expr>>) -> Self {
        let mut map = BTreeMap::<_, Vec<_>>::new();
        for arm in target.into_iter() {
            map.entry(arm.pattern.ident.clone()).or_default().push(arm);
        }
        Self::LetRec(map, body.into())
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

impl PatternInner {
    pub fn label(&self) -> Option<String> {
        match self {
            PatternInner::Variable(x) => Some(x.clone()),
        }
    }
}

impl Pattern {
    pub fn label(&self) -> Option<String> {
        self.inner.label()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LetPatternInner {
    Val,
    Func(Vec<Pattern>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetPattern {
    pub inner: LetPatternInner,
    pub ident: Ident,
    pub span: Span,
}

impl LetPattern {
    pub fn new_val(ident: Ident, span: Span) -> Self {
        Self {
            inner: LetPatternInner::Val,
            ident,
            span,
        }
    }
    pub fn new_func(ident: Ident, args: Vec<Pattern>, span: Span) -> Self {
        Self {
            inner: LetPatternInner::Func(args),
            ident,
            span,
        }
    }
}

pub type Program = BTreeMap<Ident, Vec<LetArm>>;
