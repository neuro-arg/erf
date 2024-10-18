use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::{self, Display, Write},
};

use thiserror::Error;

use crate::{
    typeck::{Neg, NegId, Pos, PosId, TypeCk, VarId},
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
    Record(BTreeMap<String, HumanType>),
    Func(Box<HumanType>, Box<HumanType>),
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
            Self::Func(a, b) => write!(f, "({a} -> {b})"),
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
    MissingField { field: String, used: Span },
    NoCoercion { value: Span, used: Span },
}

impl fmt::Display for TypeHint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingField { field, used: _ } => write!(f, "missing field `{field}`"),
            Self::NoCoercion { value: _, used: _ } => write!(f, "no coercion defined"),
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
        let ret = match ck.pos(pos) {
            Pos::Void => Self::Void,
            Pos::Bool => Self::Bool,
            Pos::Int { signed, bits } => Self::Int {
                signed: Some(*signed),
                bits: Some(*bits),
            },
            Pos::IntLiteral { signed, bits: _ } => Self::Int {
                signed: (!signed).then_some(*signed),
                bits: None,
            },
            Pos::Float { bits } => Self::Float { bits: Some(*bits) },
            Pos::FloatLiteral => Self::Float { bits: None },
            Pos::Record(x) => Self::Record(
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
                    let u = Self::union(ck, ck.pos_var(*x), rec);
                    Self::inter2(Self::Var(i), u)
                } else {
                    Self::Var(i)
                }
            }
            Pos::Func(a, b) => Self::Func(
                Box::new(Self::from_neg2(ck, a.id(), rec)),
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
        let ret = match ck.neg(neg) {
            Neg::Void => Self::Void,
            Neg::Bool => Self::Bool,
            Neg::Int { signed, bits } => Self::Int {
                signed: Some(*signed),
                bits: Some(*bits),
            },
            Neg::ArbitraryInt => Self::Int {
                signed: None,
                bits: None,
            },
            Neg::Float { bits } => Self::Float { bits: Some(*bits) },
            Neg::ArbitraryFloat => Self::Float { bits: None },
            Neg::Record(x) => Self::Record(
                x.iter()
                    .map(|(k, v)| (k.clone(), Self::from_neg2(ck, v.id(), rec)))
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
                    let u = Self::inter(ck, ck.neg_var(*x), rec);
                    Self::union2(Self::Var(i), u)
                } else {
                    Self::Var(i)
                }
            }
            Neg::Func(a, b) => Self::Func(
                Box::new(Self::from_pos2(ck, a.id(), rec)),
                Box::new(Self::from_neg2(ck, b.id(), rec)),
            ),
        };
        if let Some(var) = rec.0.get(&neg.into()).unwrap() {
            HumanType::Recursive(*var, Box::new(ret))
        } else {
            ret
        }
    }
    pub fn from_pos(ck: &TypeCk, pos: PosId) -> Self {
        Self::from_pos2(ck, pos, &mut Default::default())
    }
    pub fn from_neg(ck: &TypeCk, neg: NegId) -> Self {
        Self::from_neg2(ck, neg, &mut Default::default())
    }
    fn count_var(&self, var: &TypeVar) -> u32 {
        match self {
            Self::Var(x) => (var == x).into(),
            Self::Top
            | Self::Bot
            | Self::Void
            | Self::Bool
            | Self::Int { .. }
            | Self::Float { .. } => 0,
            Self::Record(x) => x.values().map(|x| x.count_var(var)).sum(),
            Self::Func(a, b) => a.count_var(var) + b.count_var(var),
            Self::Recursive(_a, b) => b.count_var(var),
            Self::Union(x) | Self::Intersection(x) => x.iter().map(|x| x.count_var(var)).sum(),
        }
    }
    fn simplify_vec(
        v: &[Self],
        dont_touch: &mut Vec<Self>,
        vis: &mut HashSet<TypeVar>,
    ) -> Vec<Self> {
        let count = |var: &TypeVar, dont_touch: &mut Vec<Self>| -> u32 {
            dont_touch.iter().map(|x| x.count_var(var)).sum()
        };
        dont_touch.extend(v.iter().cloned());
        let ret = v
            .iter()
            .filter_map(|x| match x {
                HumanType::Var(x) if count(x, dont_touch) == 1 => None,
                x => Some(x.simplify2(dont_touch, vis)),
            })
            .collect();
        dont_touch.truncate(dont_touch.len() - v.len());
        ret
    }
    fn simplify2(&self, dont_touch: &mut Vec<Self>, vis: &mut HashSet<TypeVar>) -> Self {
        match self {
            Self::Union(x) => {
                let v = Self::simplify_vec(x, dont_touch, vis);
                if v.len() == 1 {
                    v.into_iter().next().unwrap()
                } else {
                    Self::Union(v)
                }
            }
            Self::Intersection(x) => {
                let v = Self::simplify_vec(x, dont_touch, vis);
                if v.len() == 1 {
                    v.into_iter().next().unwrap()
                } else {
                    Self::Intersection(v)
                }
            }
            Self::Func(a, b) => {
                let a = (**a).clone();
                let b = (**b).clone();
                dont_touch.push(b);
                let a = a.simplify2(dont_touch, vis);
                let b = dont_touch.pop().unwrap();
                dont_touch.push(a);
                let b = b.simplify2(dont_touch, vis);
                let a = dont_touch.pop().unwrap();
                Self::Func(Box::new(a), Box::new(b))
            }
            x => x.clone(),
        }
    }
    pub fn simplify(&self) -> Self {
        self.simplify2(&mut Vec::new(), &mut HashSet::new())
    }
}

#[derive(Debug, Error)]
pub struct NameNotFoundError {
    span: Span,
    name: String,
}

impl NameNotFoundError {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }
}

impl fmt::Display for NameNotFoundError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "name not found: {}", self.name)
    }
}

#[derive(Debug, Error)]
pub enum Error {
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
    #[error("error: {0}")]
    Other(String, Span),
}
