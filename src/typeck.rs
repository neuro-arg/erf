use std::{
    collections::{BTreeMap, HashSet, VecDeque},
    hash::Hash,
};

use crate::{
    diag::{self, HumanType},
    util::OrderedSet,
    Span,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VarId(usize);

impl VarId {
    pub fn polarize(&self, ck: &mut TypeCk, span: Span) -> (PosIdS, NegIdS) {
        let (pos, neg) = (Pos::Var(*self), Neg::Var(*self));
        (ck.add_pos(pos, span), ck.add_neg(neg, span))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PosId(usize);
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NegId(usize);
pub type PosIdS = IdSpan<PosId>;
pub type NegIdS = IdSpan<NegId>;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct IdSpan<T>(T, Span);
impl<T: Copy> IdSpan<T> {
    pub fn new(id: T, span: Span) -> Self {
        Self(id, span)
    }
    pub fn id(&self) -> T {
        self.0
    }
    pub fn span(&self) -> Span {
        self.1
    }
}
impl<T: Copy> From<(T, Span)> for IdSpan<T> {
    fn from((a, b): (T, Span)) -> Self {
        Self::new(a, b)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Pos {
    // Bottom - the most concrete value
    Void,
    Bool,
    Int { signed: bool, bits: u8 },
    Float { bits: u8 },
    IntLiteral { signed: bool, bits: u8 },
    FloatLiteral,
    Record(BTreeMap<String, PosIdS>),
    Var(VarId),
    Func(NegIdS, PosIdS),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Neg {
    // Top - the most general value
    Void,
    Bool,
    Int { signed: bool, bits: u8 },
    ArbitraryInt,
    Float { bits: u8 },
    ArbitraryFloat,
    Record(BTreeMap<String, NegIdS>),
    Var(VarId),
    Func(PosIdS, NegIdS),
}

#[derive(Debug, Default)]
struct VarState {
    label: Option<String>,
    union: OrderedSet<PosIdS>,
    inter: OrderedSet<NegIdS>,
}

#[derive(Debug, Default)]
struct ConstraintGraph {
    constraints: HashSet<(PosIdS, NegIdS)>,
    queue: VecDeque<(PosIdS, NegIdS)>,
}

#[derive(Debug, Default)]
pub struct TypeCk {
    vars: Vec<VarState>,
    q: ConstraintGraph,
    pos: Vec<Pos>,
    neg: Vec<Neg>,
    pos_spans: Vec<Span>,
    neg_spans: Vec<Span>,
}

impl ConstraintGraph {
    fn enqueue(&mut self, lhs: PosIdS, rhs: NegIdS) {
        if self.constraints.insert((lhs, rhs)) {
            self.queue.push_back((lhs, rhs));
        }
    }
}

impl TypeCk {
    pub fn add_var(&mut self, label: Option<String>) -> VarId {
        self.vars.push(VarState {
            label,
            ..VarState::default()
        });
        VarId(self.vars.len() - 1)
    }
    pub fn add_pos(&mut self, pos: Pos, span: Span) -> PosIdS {
        self.pos.push(pos);
        self.pos_spans.push(span);
        PosIdS::new(PosId(self.pos.len() - 1), span)
    }
    pub fn add_neg(&mut self, neg: Neg, span: Span) -> NegIdS {
        self.neg.push(neg);
        self.neg_spans.push(span);
        NegIdS::new(NegId(self.neg.len() - 1), span)
    }
    pub fn pos_var(&self, var: VarId) -> impl '_ + Iterator<Item = PosId> {
        self.vars[var.0].union.into_iter().copied().map(|x| x.0)
    }
    pub fn neg_var(&self, var: VarId) -> impl '_ + Iterator<Item = NegId> {
        self.vars[var.0].inter.into_iter().copied().map(|x| x.0)
    }
    pub fn pos(&self, id: PosId) -> &Pos {
        &self.pos[id.0]
    }
    pub fn neg(&self, id: NegId) -> &Neg {
        &self.neg[id.0]
    }
    pub fn pos_as_var(&self, pos: PosId) -> Option<VarId> {
        match self.pos(pos) {
            Pos::Var(var) => Some(*var),
            _ => None,
        }
    }
    pub fn flow(&mut self, pos: PosIdS, neg: NegIdS) -> Result<(), diag::TypeError> {
        let q = &mut self.q;
        // reuse queue buffer, but clear it every time
        q.queue.clear();
        q.enqueue(pos, neg);
        while let Some((pos, neg)) = q.queue.pop_front() {
            match (&self.pos[pos.0 .0], &self.neg[neg.0 .0]) {
                (Pos::Void, Neg::Void) => {}
                (Pos::Bool, Neg::Bool) => {}
                (
                    Pos::Int {
                        signed: s1,
                        bits: b1,
                    },
                    Neg::Int {
                        signed: s2,
                        bits: b2,
                    },
                ) => {
                    if s1 != s2 || b1 != b2 {
                        // coercion not implemented
                        return Err(diag::TypeError::new(
                            HumanType::from_pos(self, pos.id()).simplify(),
                            HumanType::from_neg(self, neg.id()).simplify(),
                        )
                        .with_hint(diag::TypeHint::NoCoercion {
                            value: pos.span(),
                            used: neg.span(),
                        }));
                    }
                }
                (Pos::Int { .. }, Neg::ArbitraryInt) => {}
                (Pos::Float { bits: b1 }, Neg::Float { bits: b2 }) => {
                    if b1 != b2 {
                        // coercion not implemented
                        return Err(diag::TypeError::new(
                            HumanType::from_pos(self, pos.id()).simplify(),
                            HumanType::from_neg(self, neg.id()).simplify(),
                        )
                        .with_hint(diag::TypeHint::NoCoercion {
                            value: pos.span(),
                            used: neg.span(),
                        }));
                    }
                }
                (Pos::Float { .. }, Neg::ArbitraryFloat { .. }) => {}
                (Pos::IntLiteral { .. }, Neg::Float { .. }) => {}
                (Pos::IntLiteral { .. }, Neg::ArbitraryFloat { .. }) => {}
                (Pos::FloatLiteral { .. }, Neg::Float { .. }) => {}
                (Pos::FloatLiteral { .. }, Neg::ArbitraryFloat { .. }) => {}
                (
                    Pos::IntLiteral {
                        signed: s1,
                        bits: b1,
                    },
                    Neg::Int {
                        signed: s2,
                        bits: b2,
                    },
                ) => {
                    if (s1 != s2 || b1 > b2) && (!s2 || *s1 || b1 >= b2) {
                        return Err(diag::TypeError::new(
                            HumanType::from_pos(self, pos.id()).simplify(),
                            HumanType::from_neg(self, neg.id()).simplify(),
                        )
                        .with_hint(diag::TypeHint::NoCoercion {
                            value: pos.span(),
                            used: neg.span(),
                        }));
                    }
                }
                (Pos::IntLiteral { .. }, Neg::ArbitraryInt) => {}
                (Pos::Record(pos1), Neg::Record(neg)) => {
                    let (mut pos, pos1) = (pos1.iter(), pos);
                    for (k, neg) in neg {
                        if let Some((_, pos)) = pos.find(|x| x.0 == k) {
                            q.enqueue(*pos, *neg);
                        } else {
                            // missing field
                            return Err(diag::TypeError::new(
                                HumanType::from_pos(self, pos1.id()).simplify(),
                                HumanType::from_neg(self, neg.id()).simplify(),
                            )
                            .with_hint(diag::TypeHint::MissingField {
                                field: k.clone(),
                                used: neg.span(),
                            }));
                        }
                    }
                }
                (Pos::Func(pos_a, pos_b), Neg::Func(neg_a, neg_b)) => {
                    // ensure function of type pos_a -> pos_b can consume values of type neg_a
                    q.enqueue(*neg_a, *pos_a);
                    // ensure function of type pos_a -> pos_b produces values of type neg_b
                    q.enqueue(*pos_b, *neg_b);
                }
                (Pos::Var(pos), _) => {
                    if self.vars[pos.0].inter.insert(neg).0 {
                        for x in &self.vars[pos.0].union {
                            q.enqueue(*x, neg);
                        }
                    }
                }
                (_, Neg::Var(neg)) => {
                    if self.vars[neg.0].union.insert(pos).0 {
                        for y in &self.vars[neg.0].inter {
                            q.enqueue(pos, *y);
                        }
                    }
                }
                (_, _) => {
                    return Err(diag::TypeError::new(
                        HumanType::from_pos(self, pos.id()).simplify(),
                        HumanType::from_neg(self, neg.id()).simplify(),
                    )
                    .with_hint(diag::TypeHint::NoCoercion {
                        value: pos.span(),
                        used: neg.span(),
                    }));
                }
            }
        }
        Ok(())
    }
    fn pos_gv_node_id(&self, p: PosId) -> String {
        match self.pos(p) {
            Pos::Var(v) => format!("+v{}", v.0),
            _ => format!("+{}", p.0),
        }
    }
    fn neg_gv_node_id(&self, p: NegId) -> String {
        match self.neg(p) {
            Neg::Var(v) => format!("-v{}", v.0),
            _ => format!("-{}", p.0),
        }
    }
    pub fn graphviz(&self, src: &str) -> String {
        let mut ret = vec!["strict digraph t {".to_owned()];
        let id = |id: &str| format!("{:?}", id);
        for (i, _var) in self.pos.iter().enumerate() {
            let name = id(&self.pos_gv_node_id(PosId(i)));
            let mut label = self.pos_gv_node_id(PosId(i));
            if label.contains('v') {
                label.push('\n');
                label.push_str(&format!("+{i}"));
            }
            label.push('\n');
            label.push_str(&format!(
                "{}",
                HumanType::from_pos(self, PosId(i)).simplify()
            ));
            label.push('\n');
            label.push_str(self.pos_spans[i].text(src));
            ret.push(format!("  {name}[label={}]", id(&label)));
        }
        for (i, _var) in self.neg.iter().enumerate() {
            let name = id(&self.neg_gv_node_id(NegId(i)));
            let mut label = self.neg_gv_node_id(NegId(i));
            if label.contains('v') {
                label.push('\n');
                label.push_str(&format!("-{i}"));
            }
            label.push('\n');
            label.push_str(&format!(
                "{}",
                HumanType::from_neg(self, NegId(i)).simplify()
            ));
            label.push('\n');
            label.push_str(self.neg_spans[i].text(src));
            ret.push(format!("  {name}[label={}]", id(&label)));
        }
        for (i, var) in self.vars.iter().enumerate() {
            let pos = id(&format!("+v{}", i));
            let neg = id(&format!("-v{}", i));
            let label = id(&format!("{:?}", var));
            ret.push(format!("  {neg} -> {pos}[info={label}]"));
        }
        for (pos, neg) in &self.q.constraints {
            let pos = id(&self.pos_gv_node_id(pos.id()));
            let neg = id(&self.neg_gv_node_id(neg.id()));
            ret.push(format!("  {pos} -> {neg} [color=red]"));
        }
        ret.push("}".to_owned());
        ret.join("\n")
    }
}
