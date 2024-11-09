use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Display, Write},
};

use thiserror::Error;

use crate::{
    ast::QualifiedIdent,
    typeck::{Neg, NegId, NegPrim, Pos, PosId, PosPrim, TypeCk, VarId},
    Span,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(usize);

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            0..=25 => f.write_char(char::from_u32((b'a' + self.0 as u8).into()).unwrap()),
            26.. => write!(
                f,
                "{}{}",
                char::from_u32((b'a' + (self.0 % 26) as u8).into()).unwrap(),
                self.0 / 26
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub enum HumanType {
    Top,
    Bot,
    Void,
    Bool,
    Int {
        signed: Option<bool>,
        bits: Option<u8>,
    },
    Float {
        bits: Option<u8>,
    },
    Tagged(String, Box<HumanType>),
    Record(BTreeMap<String, HumanType>),
    Func(Vec<HumanType>, Box<HumanType>),
    Recursive(TypeVar, Box<HumanType>),
    Var(TypeVar),
    Union(Vec<HumanType>),
    Intersection(Vec<HumanType>),
}

impl fmt::Display for HumanType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Top => f.write_str("any"),
            Self::Bot => f.write_str("any"),
            Self::Void => f.write_str("()"),
            Self::Bool => f.write_str("bool"),
            Self::Tagged(a, b) => {
                f.write_str(a)?;
                f.write_char(' ')?;
                b.fmt(f)
            }
            Self::Int {
                signed: None,
                bits: None,
            } => f.write_str("int"),
            Self::Int {
                signed: Some(true),
                bits: None,
            } => f.write_str("signed"),
            Self::Int {
                signed: Some(false),
                bits: None,
            } => f.write_str("unsigned"),
            Self::Int {
                signed: None,
                bits: Some(bits),
            } => write!(f, "i{bits}"),
            Self::Int {
                signed: Some(true),
                bits: Some(bits),
            } => write!(f, "s{bits}"),
            Self::Int {
                signed: Some(false),
                bits: Some(bits),
            } => write!(f, "u{bits}"),
            Self::Float { bits: None } => f.write_str("float"),
            Self::Float { bits: Some(bits) } => write!(f, "f{bits}"),
            Self::Func(a, b) => {
                f.write_str("fn(")?;
                let mut first = true;
                for x in a {
                    if first {
                        first = false;
                    } else {
                        f.write_str(", ")?;
                    }
                    x.fmt(f)?;
                }
                f.write_str(") -> ")?;
                b.fmt(f)
            }
            Self::Record(fields) => {
                f.write_str("{")?;
                let mut is_first = true;
                for (k, v) in fields {
                    if is_first {
                        is_first = false;
                        f.write_str(" ")?;
                    } else {
                        f.write_str(", ")?;
                    }
                    write!(f, "{k}: {v}")?;
                }
                f.write_str(" }")
            }
            Self::Var(x) => x.fmt(f),
            Self::Recursive(v, x) => write!(f, "rec {v} {x}"),
            Self::Union(x) => {
                f.write_str("(")?;
                let mut is_first = true;
                for x in x {
                    if is_first {
                        is_first = false;
                    } else {
                        f.write_str(" | ")?;
                    }
                    x.fmt(f)?;
                }
                f.write_str(")")
            }
            Self::Intersection(x) => {
                f.write_str("(")?;
                let mut is_first = true;
                for x in x {
                    if is_first {
                        is_first = false;
                    } else {
                        f.write_str(" & ")?;
                    }
                    x.fmt(f)?;
                }
                f.write_str(")")
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeHint {
    ArityMismatch {
        count: usize,
        used: Span,
        declared: Vec<(usize, Span)>,
    },
    MissingField {
        field: String,
        used: Span,
    },
    UnhandledCase {
        case: String,
        created: Span,
    },
    NoCoercion {
        value: Span,
        used: Span,
    },
}

impl fmt::Display for TypeHint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ArityMismatch { count, .. } => write!(f, "arity mismatch ({count})"),
            Self::MissingField { field, .. } => write!(f, "missing field `{field}`"),
            Self::UnhandledCase { case, .. } => write!(f, "missing case `{case}`"),
            Self::NoCoercion { .. } => write!(f, "no coercion defined"),
        }
    }
}

#[derive(Clone, Debug, Default)]
struct TypeHints(Vec<TypeHint>);

impl fmt::Display for TypeHints {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for x in &self.0 {
            f.write_char('\n')?;
            x.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Error)]
pub struct TypeError {
    expected: HumanType,
    found: HumanType,
    hints: TypeHints,
}

impl TypeError {
    pub fn new(found: HumanType, expected: HumanType) -> Self {
        Self {
            expected,
            found,
            hints: TypeHints::default(),
        }
    }
    pub fn with_hint(mut self, hint: TypeHint) -> Self {
        self.hints.0.push(hint);
        self
    }
}

impl Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "type mismatch: expected {}, found {}{}",
            self.expected, self.found, self.hints
        )
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum AnyId {
    Pos(PosId),
    Neg(NegId),
    Var(VarId),
}

impl From<PosId> for AnyId {
    fn from(value: PosId) -> Self {
        Self::Pos(value)
    }
}
impl From<NegId> for AnyId {
    fn from(value: NegId) -> Self {
        Self::Neg(value)
    }
}
impl From<VarId> for AnyId {
    fn from(value: VarId) -> Self {
        Self::Var(value)
    }
}

impl HumanType {
    fn union(
        ck: &TypeCk,
        pos: impl Iterator<Item = PosId>,
        rec: &mut (HashMap<AnyId, Option<TypeVar>>, usize),
    ) -> Self {
        let x: Vec<_> = pos
            .flat_map(|pos| match Self::from_pos2(ck, pos, rec) {
                Self::Union(x) => x,
                Self::Bot => vec![],
                x => vec![x],
            })
            .collect();
        match x.len() {
            0 => Self::Bot,
            1 => x.into_iter().next().unwrap(),
            _ => Self::Union(x),
        }
    }
    fn inter(
        ck: &TypeCk,
        neg: impl Iterator<Item = NegId>,
        rec: &mut (HashMap<AnyId, Option<TypeVar>>, usize),
    ) -> Self {
        let x: Vec<_> = neg
            .flat_map(|neg| match Self::from_neg2(ck, neg, rec) {
                Self::Intersection(x) => x,
                Self::Top => vec![],
                x => vec![x],
            })
            .collect();
        match x.len() {
            0 => Self::Top,
            1 => x.into_iter().next().unwrap(),
            _ => Self::Intersection(x),
        }
    }
    fn inter2(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::Top, a) => a,
            (a, Self::Top) => a,
            (Self::Intersection(mut a), Self::Intersection(b)) => {
                a.extend(b);
                Self::Intersection(a)
            }
            (Self::Intersection(mut a), b) => {
                a.push(b);
                Self::Intersection(a)
            }
            (a, Self::Intersection(mut b)) => {
                b.push(a);
                Self::Intersection(b)
            }
            (a, b) => Self::Intersection(vec![a, b]),
        }
    }
    fn union2(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::Bot, a) => a,
            (a, Self::Bot) => a,
            (Self::Union(mut a), Self::Union(b)) => {
                a.extend(b);
                Self::Union(a)
            }
            (Self::Union(mut a), b) => {
                a.push(b);
                Self::Union(a)
            }
            (a, Self::Union(mut b)) => {
                b.push(a);
                Self::Union(b)
            }
            (a, b) => Self::Union(vec![a, b]),
        }
    }
    fn from_pos2(
        ck: &TypeCk,
        pos: PosId,
        rec: &mut (HashMap<AnyId, Option<TypeVar>>, usize),
    ) -> Self {
        if let Some(ret) = rec.0.get_mut(&pos.into()) {
            return Self::Var(ret.unwrap_or_else(|| {
                *ret = Some(TypeVar(rec.1));
                rec.1 += 1;
                TypeVar(rec.1 - 1)
            }));
        }
        rec.0.insert(pos.into(), None);
        let ret = match ck.ty(pos) {
            Pos::Prim(PosPrim::Void) => Self::Void,
            Pos::Prim(PosPrim::Bool) => Self::Bool,
            Pos::Prim(PosPrim::Int { signed, bits }) => Self::Int {
                signed: Some(*signed),
                bits: Some(*bits),
            },
            Pos::Prim(PosPrim::IntLiteral { signed, bits: _ }) => Self::Int {
                signed: (!signed).then_some(*signed),
                bits: None,
            },
            Pos::Prim(PosPrim::Float { bits }) => Self::Float { bits: Some(*bits) },
            Pos::Prim(PosPrim::FloatLiteral) => Self::Float { bits: None },
            Pos::Prim(PosPrim::Record(x)) => Self::Record(
                x.iter()
                    .map(|(k, v)| (k.clone(), Self::from_pos2(ck, v.id(), rec)))
                    .collect(),
            ),
            Pos::Var(x) => {
                let i = rec.0.entry((*x).into()).or_default();
                let (new, i) = match i {
                    Some(i) => (false, *i),
                    None => {
                        *i = Some(TypeVar(rec.1));
                        rec.1 += 1;
                        (true, TypeVar(rec.1 - 1))
                    }
                };
                if new || true {
                    let u = Self::union(ck, ck.var(*x), rec);
                    Self::union2(Self::Var(i), u)
                } else {
                    Self::Var(i)
                }
            }
            Pos::Prim(PosPrim::Func(cases)) => cases
                .values()
                .map(|(a, b)| {
                    Self::Func(
                        a.iter().map(|a| Self::from_neg2(ck, a.id(), rec)).collect(),
                        Box::new(Self::from_pos2(ck, b.id(), rec)),
                    )
                })
                .fold(Self::Bot, Self::union2),

            Pos::Prim(PosPrim::Label(a, b)) => Self::Tagged(
                ck.label(*a).to_owned(),
                Box::new(Self::from_pos2(ck, b.id(), rec)),
            ),
        };
        if let Some(var) = rec.0.get(&pos.into()).unwrap() {
            HumanType::Recursive(*var, Box::new(ret))
        } else {
            ret
        }
    }
    fn from_neg2(
        ck: &TypeCk,
        neg: NegId,
        rec: &mut (HashMap<AnyId, Option<TypeVar>>, usize),
    ) -> Self {
        if let Some(ret) = rec.0.get_mut(&neg.into()) {
            return Self::Var(ret.unwrap_or_else(|| {
                *ret = Some(TypeVar(rec.1));
                rec.1 += 1;
                TypeVar(rec.1 - 1)
            }));
        }
        rec.0.insert(neg.into(), None);
        let ret = match ck.ty(neg) {
            Neg::Prim(NegPrim::Void) => Self::Void,
            Neg::Prim(NegPrim::Bool) => Self::Bool,
            Neg::Prim(NegPrim::Int { signed, bits }) => Self::Int {
                signed: Some(*signed),
                bits: Some(*bits),
            },
            Neg::Prim(NegPrim::Float { bits }) => Self::Float { bits: Some(*bits) },
            Neg::Prim(NegPrim::Record(k, v)) => Self::Record(
                [(k.clone(), Self::from_neg2(ck, v.id(), rec))]
                    .into_iter()
                    .collect(),
            ),
            Neg::Var(x) => {
                let i = rec.0.entry((*x).into()).or_default();
                let (new, i) = match i {
                    Some(i) => (false, *i),
                    None => {
                        *i = Some(TypeVar(rec.1));
                        rec.1 += 1;
                        (true, TypeVar(rec.1 - 1))
                    }
                };
                if new || true {
                    let u = Self::inter(ck, ck.var(*x), rec);
                    Self::inter2(Self::Var(i), u)
                } else {
                    Self::Var(i)
                }
            }
            Neg::Prim(NegPrim::Func(a, b)) => Self::Func(
                a.iter().map(|a| Self::from_pos2(ck, a.id(), rec)).collect(),
                Box::new(Self::from_neg2(ck, b.id(), rec)),
            ),
            Neg::Prim(NegPrim::Label { cases, fallthrough }) => {
                let mut val = Self::Bot;
                for (label, (ty, _refutable, _flow)) in cases {
                    val = Self::union2(
                        val,
                        Self::Tagged(
                            ck.label(*label).to_owned(),
                            Box::new(Self::from_neg2(ck, ty.id(), rec)),
                        ),
                    );
                }
                if let Some((fallback, _flow)) = fallthrough {
                    val = Self::union2(val, Self::from_neg2(ck, fallback.id(), rec));
                }
                val
            }
        };
        if let Some(var) = rec.0.get(&neg.into()).unwrap() {
            HumanType::Recursive(*var, Box::new(ret))
        } else {
            ret
        }
    }
    pub fn from_pos(ck: &TypeCk, pos: PosId) -> Self {
        Self::from_pos2(ck, pos, &mut Default::default()).simplify(true)
    }
    pub fn from_neg(ck: &TypeCk, neg: NegId) -> Self {
        Self::from_neg2(ck, neg, &mut Default::default()).simplify(false)
    }
    fn collect_vars(&self, out: &mut HashMap<TypeVar, usize>) {
        match self {
            Self::Var(x) => *out.entry(*x).or_default() += 1,
            Self::Top
            | Self::Bot
            | Self::Void
            | Self::Bool
            | Self::Int { .. }
            | Self::Float { .. } => {}
            Self::Record(x) => x.values().for_each(|x| x.collect_vars(out)),
            Self::Func(a, b) => {
                for x in a {
                    x.collect_vars(out);
                }
                b.collect_vars(out);
            }
            Self::Recursive(a, b) => {
                *out.entry(*a).or_default() += 1;
                b.collect_vars(out);
            }
            Self::Tagged(_, b) => b.collect_vars(out),
            Self::Union(x) | Self::Intersection(x) => x.iter().for_each(|x| x.collect_vars(out)),
        }
    }
    fn direct_contains(&self, var: TypeVar) -> bool {
        match self {
            Self::Var(x) if *x == var => true,
            Self::Union(x) | Self::Intersection(x) => x.iter().any(|x| x.direct_contains(var)),
            _ => false,
        }
    }
    fn simplify_vec(v: Vec<Self>, vars: &HashMap<TypeVar, usize>, pos: bool) -> Vec<Self> {
        v.into_iter()
            .flat_map(|x| match x.simplify2(vars, pos) {
                Self::Top | Self::Bot => vec![],
                Self::Intersection(x) => x,
                Self::Union(x) => x,
                x => vec![x],
            })
            .collect()
    }
    fn simplify2(self, vars: &HashMap<TypeVar, usize>, pos: bool) -> Self {
        match self {
            Self::Var(x) if *vars.get(&x).unwrap() <= 1 => {
                if pos {
                    Self::Bot
                } else {
                    Self::Top
                }
            }
            Self::Union(x) => {
                let v = Self::simplify_vec(x, vars, pos);
                match v.len() {
                    0 => Self::Bot,
                    1 => v.into_iter().next().unwrap(),
                    _ => Self::Union(v),
                }
            }
            Self::Intersection(x) => {
                let v = Self::simplify_vec(x, vars, pos);
                match v.len() {
                    0 => Self::Top,
                    1 => v.into_iter().next().unwrap(),
                    _ => Self::Intersection(v),
                }
            }
            Self::Func(a, b) => Self::Func(
                a.into_iter().map(|x| x.simplify2(vars, pos)).collect(),
                Box::new(b.simplify2(vars, pos)),
            ),
            Self::Recursive(a, b) => {
                let b = b.simplify2(vars, pos);
                Self::Recursive(a, Box::new(b))
            }
            x => x.clone(),
        }
    }
    fn simplify(self, pos: bool) -> Self {
        let mut vars = Default::default();
        self.collect_vars(&mut vars);
        self.simplify2(&vars, pos)
    }
}

#[derive(Debug, Error)]
pub struct NameNotFoundError {
    span: Span,
    name: QualifiedIdent,
    is_type: bool,
}

impl NameNotFoundError {
    pub fn new<S: AsRef<str>>(
        name: impl IntoIterator<Item = S>,
        span: Span,
        is_type: bool,
    ) -> Self {
        Self {
            name: QualifiedIdent(name.into_iter().map(|x| x.as_ref().to_owned()).collect()),
            span,
            is_type,
        }
    }
}

impl fmt::Display for NameNotFoundError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} not found: {}",
            if self.is_type { "type" } else { "var" },
            self.name
        )
    }
}

#[derive(Debug, Error)]
pub struct VarRedefinitionError {
    spans: Vec<Span>,
    name: String,
}

impl VarRedefinitionError {
    pub fn new(name: impl Into<String>, spans: Vec<Span>) -> Self {
        Self {
            name: name.into(),
            spans,
        }
    }
}

impl fmt::Display for VarRedefinitionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "name redefinition: {}", self.name)
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("parse error")]
    Lalrpop(u8, lalrpop_util::ParseError<usize, (usize, String), String>),
    #[error("type error: {0}")]
    Type(
        #[source]
        #[from]
        TypeError,
    ),
    #[error("{0}")]
    NameNotFound(
        #[source]
        #[from]
        NameNotFoundError,
    ),
    #[error("{0}")]
    Redefinition(
        #[source]
        #[from]
        VarRedefinitionError,
    ),
    #[error("error: {0}")]
    Other(String, Span),
    #[error("(internal, this shouldn't be returned) no value")]
    NoValue,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ResolvedSpan<'a>(&'a str, crate::Span);
impl<'a> ariadne::Span for ResolvedSpan<'a> {
    type SourceId = &'a str;
    fn source(&self) -> &Self::SourceId {
        &self.0
    }
    fn start(&self) -> usize {
        self.1.left
    }
    fn end(&self) -> usize {
        self.1.right
    }
}

impl Error {
    pub fn to_ariadne<'a>(
        &self,
        span_table: &'a [impl AsRef<str>],
        ariadne: ariadne::ReportBuilder<'a, ResolvedSpan<'a>>,
    ) -> ariadne::Report<'a, ResolvedSpan<'a>> {
        let resolve =
            |span: &crate::Span| ResolvedSpan(span_table[span.file as usize].as_ref(), *span);
        match self {
            Self::NoValue => unreachable!("unexpected no value error"),
            Self::Type(TypeError {
                expected,
                found,
                hints,
            }) => {
                let mut ariadne = ariadne.with_message("type mismatch");
                for hint in &hints.0 {
                    match hint {
                        TypeHint::ArityMismatch {
                            count,
                            used,
                            declared,
                        } => {
                            ariadne = ariadne.with_label(
                                ariadne::Label::new(resolve(used))
                                    .with_message(format!(
                                        "can't call this function with {count} arguments"
                                    ))
                                    .with_order(-1),
                            );
                            for (count, decl) in declared {
                                ariadne = ariadne.with_label(
                                    ariadne::Label::new(resolve(decl)).with_message(format!(
                                        "function declared with {count} args here"
                                    )),
                                );
                            }
                        }
                        TypeHint::NoCoercion { value, used } => {
                            ariadne = ariadne.with_label(
                                ariadne::Label::new(resolve(value))
                                    .with_message(format!(
                                        "expected this to be usable as `{expected}`"
                                    ))
                                    .with_order(-1),
                            );
                            ariadne = ariadne.with_label(
                                ariadne::Label::new(resolve(used))
                                    .with_message(format!("found value of type `{found}`")),
                            );
                        }
                        TypeHint::MissingField { field, used } => {
                            ariadne = ariadne.with_label(
                                ariadne::Label::new(resolve(used))
                                    .with_message(format!("field `{field}` may be missing")),
                            );
                        }
                        TypeHint::UnhandledCase { case, created } => {
                            ariadne = ariadne.with_label(
                                ariadne::Label::new(resolve(created))
                                    .with_message(format!("case `{case}` isn't handled")),
                            );
                        }
                    }
                }
                ariadne
            }
            Self::NameNotFound(NameNotFoundError {
                span,
                name,
                is_type,
            }) => {
                let s = if *is_type { "type" } else { "name" };
                ariadne
                    .with_message(format!("{s} not in scope"))
                    .with_label(
                        ariadne::Label::new(resolve(span))
                            .with_message(format!("{s} `{name}` not found")),
                    )
            }
            Self::Redefinition(VarRedefinitionError { spans, name }) => {
                let mut msg = ariadne.with_message(format!("redefinition of `{name}`"));
                let mut first = true;
                for span in spans {
                    msg =
                        msg.with_label(ariadne::Label::new(resolve(span)).with_message(if first {
                            first = false;
                            "defined here"
                        } else {
                            "also defined here"
                        }));
                }
                msg = msg.with_note(
                    "variables can't be defined in multiple places within the same scope",
                );
                msg
            }
            Self::Other(error, span) => ariadne
                .with_message("unknown error")
                .with_label(ariadne::Label::new(resolve(span)).with_message(error)),
            Self::Lalrpop(file, error) => {
                let ariadne = ariadne.with_message("parse error");
                let resolve = |a: &usize, b: &usize| resolve(&Span::new(*file, *a, *b));
                match error {
                    lalrpop_util::ParseError::User { error } => ariadne.with_message(error),
                    lalrpop_util::ParseError::ExtraToken { token: (a, tok, b) } => ariadne
                        .with_label(
                            ariadne::Label::new(resolve(a, b))
                                .with_message(format!("unexpected token: `{}`", tok.1)),
                        ),
                    lalrpop_util::ParseError::InvalidToken { location } => ariadne.with_label(
                        ariadne::Label::new(resolve(location, &(location + 1)))
                            .with_message("invalid token"),
                    ),
                    lalrpop_util::ParseError::UnrecognizedEof {
                        location,
                        expected: _,
                    } => ariadne.with_label(
                        ariadne::Label::new(resolve(location, location))
                            .with_message("unexpected end of file"),
                    ),
                    lalrpop_util::ParseError::UnrecognizedToken {
                        token: (a, tok, b),
                        expected: _,
                    } => ariadne.with_label(
                        ariadne::Label::new(resolve(a, b))
                            .with_message(format!("unexpected token: `{}`", tok.1)),
                    ),
                }
            }
        }
        .finish()
    }
}
