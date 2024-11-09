use std::{collections::BTreeMap, fmt::Display};

use crate::Span;

pub type Ident = String;
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QualifiedIdent(pub Vec<String>);
impl QualifiedIdent {
    pub fn as_slice(&self) -> &[String] {
        &self.0
    }
}
impl IntoIterator for QualifiedIdent {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<'a> IntoIterator for &'a QualifiedIdent {
    type Item = &'a String;
    type IntoIter = std::slice::Iter<'a, String>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl Display for QualifiedIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for x in &self.0 {
            if first {
                first = false;
            } else {
                f.write_str("::")?;
            }
            f.write_str(&x)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Bool,
    Float,
    Int,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnOp {
    Not,
    BitNot,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
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

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Pow => "**",
            Self::Mul => "*",
            Self::Div => "/",
            Self::FloorDiv => "//",
            Self::Rem => "%",
            Self::Add => "+",
            Self::Sub => "-",
            Self::Shl => "<<",
            Self::Shr => ">>",
            Self::BitAnd => "&",
            Self::BitXor => "^",
            Self::BitOr => "|",
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Le => "<=",
            Self::Ge => ">=",
            Self::And => "&&",
            Self::Or => "||",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprInner {
    Literal(LiteralKind, String),
    Variable(QualifiedIdent),
    UnOp(UnOp, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Lambda(Vec<Pattern>, Box<Expr>),
    LetRec(BTreeMap<Ident, Vec<LetArm>>, Box<Expr>),
    TypeConstructor(Ident),
}

impl Expr {
    pub fn as_lambda(&self) -> Option<(&[Pattern], &Expr)> {
        match &self.inner {
            ExprInner::Lambda(a, b) => Some((a, b)),
            _ => None,
        }
    }
    pub fn into_lambda(self) -> Option<(Vec<Pattern>, Expr)> {
        match self.inner {
            ExprInner::Lambda(a, b) => Some((a, *b)),
            _ => None,
        }
    }
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
    pub fn var(s: impl Into<QualifiedIdent>) -> Self {
        Self::Variable(s.into())
    }
    pub fn unary(op: UnOp, expr: impl Into<Box<Expr>>) -> Self {
        Self::UnOp(op, expr.into())
    }
    pub fn binary(op: BinOp, lhs: impl Into<Box<Expr>>, rhs: impl Into<Box<Expr>>) -> Self {
        Self::BinOp(op, lhs.into(), rhs.into())
    }
    pub fn lambda(lhs: Vec<Pattern>, rhs: impl Into<Box<Expr>>) -> Self {
        Self::Lambda(lhs, rhs.into())
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PatternInner {
    Tag(QualifiedIdent, Span, Box<Pattern>),
    Variable(Ident),
}

impl PatternInner {
    pub fn var(s: impl Into<String>) -> Self {
        Self::Variable(s.into())
    }
    pub fn tag(s: QualifiedIdent, span: Span, pat: Pattern) -> Self {
        Self::Tag(s, span, Box::new(pat))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            PatternInner::Tag(_tag, _span, x) => x.label(),
        }
    }
}

impl Pattern {
    pub fn label(&self) -> Option<String> {
        self.inner.label()
    }
    pub fn iter_var_names(&self) -> impl Iterator<Item = &str> {
        match &self.inner {
            PatternInner::Tag(_, _, x) => x.iter_var_names(),
            PatternInner::Variable(x) => [x.as_str()].into_iter(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LetPatternInner {
    Val,
    Func(Vec<Pattern>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub fn as_func(&self) -> Option<&[Pattern]> {
        match &self.inner {
            LetPatternInner::Func(x) => Some(x),
            _ => None,
        }
    }
}

pub type Program = BTreeMap<Ident, Vec<LetArm>>;
