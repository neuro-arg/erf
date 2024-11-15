use std::{
    collections::{BTreeMap, HashMap, LinkedList},
    fmt::Display,
};

use itertools::Itertools;

use crate::{util::IterEither, Span};

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
            f.write_str(x)?;
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
pub enum BasicPatternInner {
    Variable(Ident),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicPattern {
    pub inner: BasicPatternInner,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PatternInner {
    Basic(BasicPatternInner),
    Or(Vec<Pattern>),
    And(Vec<Pattern>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pattern {
    pub inner: PatternInner,
    pub span: Span,
}

#[derive(PartialEq, Eq)]
pub struct PatConj(pub LinkedList<BasicPattern>);
impl PatConj {
    fn first_pat(&self) -> Option<&BasicPattern> {
        self.0.front()
    }
    pub fn pop_first_pat(&mut self) -> Option<BasicPattern> {
        self.0.pop_front()
    }
}
impl std::fmt::Debug for PatConj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("(")?;
        let mut first = true;
        for x in &self.0 {
            if first {
                first = false;
            } else {
                f.write_str(" & ")?;
            }
            x.fmt(f)?;
        }
        f.write_str(")")
    }
}
#[derive(PartialEq, Eq)]
pub struct PatDnf(pub Vec<PatConj>);
impl std::fmt::Debug for PatDnf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("(")?;
        let mut first = true;
        for x in &self.0 {
            if first {
                first = false;
            } else {
                f.write_str(" | ")?;
            }
            x.fmt(f)?;
        }
        f.write_str(")")
    }
}

impl Pattern {
    pub fn new(inner: PatternInner, span: Span) -> Self {
        Self { inner, span }
    }
    pub fn new1(inner: BasicPatternInner, span: Span) -> Self {
        Self::new(PatternInner::Basic(inner), span)
    }
    /// Convert into disjunction of conjunctions
    pub fn into_dnf(self) -> PatDnf {
        PatDnf(match self.inner {
            // for and, convert each item into dnf and make a cartesian product
            // [a | b] & [c | d] -> [a & c] | [a & d] | [b & c] | [b & d]
            PatternInner::And(x) => x
                .into_iter()
                .map(|x| {
                    x.into_dnf()
                        .0
                        .into_iter()
                        .map(|x| x.0)
                        .collect::<Vec<LinkedList<BasicPattern>>>()
                })
                .multi_cartesian_product()
                .map(|x| {
                    PatConj(x.into_iter().fold(LinkedList::new(), |mut old, mut new| {
                        old.append(&mut new);
                        old
                    }))
                })
                .collect(),
            // for or, concatenate all arms
            PatternInner::Or(pats) => pats.into_iter().flat_map(|x| x.into_dnf().0).collect(),
            // for basic pattern, just take it as-is
            PatternInner::Basic(inner) => vec![PatConj(
                [BasicPattern {
                    inner,
                    span: self.span,
                }]
                .into(),
            )],
        })
    }
}

impl PatternInner {
    fn labels(&self) -> impl Iterator<Item = &str> {
        match self {
            PatternInner::Basic(x) => match x {
                BasicPatternInner::Variable(x) => IterEither::A([x.as_str()].into_iter()),
            },
            PatternInner::Or(x) | PatternInner::And(x) => IterEither::B(
                // box it to avoid recursive types which cant be inferred in rust
                x.iter().flat_map(|x| {
                    let labels: Box<dyn Iterator<Item = &str>> = Box::new(x.inner.labels());
                    labels
                }),
            ),
        }
    }
    pub fn label(&self) -> Option<String> {
        let mut names = HashMap::<_, usize>::new();
        let mut ret = None;
        let mut max_c = 0;
        for name in self.labels() {
            let w = names.entry(name).or_default();
            *w += 1;
            if *w > max_c {
                max_c = *w;
                ret = Some(name);
            }
        }
        ret.map(|x| x.to_owned())
    }
}

impl Pattern {
    pub fn label(&self) -> Option<String> {
        self.inner.label()
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

#[cfg(test)]
mod test {
    use crate::{
        ast::{PatConj, PatDnf},
        Span,
    };

    use super::{BasicPattern, BasicPatternInner, Pattern, PatternInner};

    fn or(pats: impl IntoIterator<Item = Pattern>) -> Pattern {
        Pattern::new(
            PatternInner::Or(pats.into_iter().collect()),
            Span::default(),
        )
    }
    fn and(pats: impl IntoIterator<Item = Pattern>) -> Pattern {
        Pattern::new(
            PatternInner::And(pats.into_iter().collect()),
            Span::default(),
        )
    }
    fn var(name: impl AsRef<str>) -> Pattern {
        Pattern::new1(
            BasicPatternInner::Variable(name.as_ref().to_owned()),
            Span::default(),
        )
    }
    fn bvar(name: impl AsRef<str>) -> BasicPattern {
        BasicPattern {
            inner: BasicPatternInner::Variable(name.as_ref().to_owned()),
            span: Span::default(),
        }
    }

    #[test]
    fn test() {
        let pat = or([
            and([var("test"), var("test1")]),
            and([var("test2"), or([var("test3"), var("test4")])]),
        ]);
        let dnf = pat.into_dnf();
        assert_eq!(
            dnf,
            PatDnf(vec![
                PatConj([bvar("test"), bvar("test1")].into()),
                PatConj([bvar("test2"), bvar("test3")].into()),
                PatConj([bvar("test2"), bvar("test4")].into()),
            ])
        );
    }
}
