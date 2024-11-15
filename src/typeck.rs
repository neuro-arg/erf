use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    hash::Hash,
    num::NonZeroUsize,
};

use indexmap::IndexSet;
use polar::{AnyIdMut, AnyIdRef, PolarPrimitive};

use crate::{
    diag::{self, HumanType},
    util::{Id, IdSpan},
    Span,
};

mod polar;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RelevelId(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LabelId(NonZeroUsize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VarId(usize);

impl std::fmt::Debug for VarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("VarId(")?;
        self.0.fmt(f)?;
        f.write_str(")")
    }
}

impl VarId {
    pub fn polarize(&self, ck: &mut TypeCk, span: Span) -> (PosIdS, NegIdS) {
        let (pos, neg) = (Pos::Var(*self), Neg::Var(*self));
        (ck.add_ty(pos, span), ck.add_ty(neg, span))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PosPrim {
    Bool,
    Int {
        signed: bool,
        bits: u8,
    },
    Float {
        bits: u8,
    },
    IntLiteral {
        signed: bool,
        bits: u8,
    },
    FloatLiteral,
    Record(BTreeMap<String, IdSpan<Self>>),
    Label(LabelId, IdSpan<Self>),
    Lambda(IdSpan<NegPrim>, IdSpan<Self>),
    /// Lambda annotated with arity
    Func(usize, IdSpan<Self>),
}

pub type Flow = Option<(PosIdS, NegIdS)>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NegPrim {
    Bool,
    Int {
        signed: bool,
        bits: u8,
    },
    Float {
        bits: u8,
    },
    Record(String, IdSpan<Self>),
    Label {
        cases: BTreeMap<LabelId, (IdSpan<Self>, bool, Flow)>,
        fallthrough: Option<(IdSpan<Self>, Flow)>,
    },
    Lambda(IdSpan<PosPrim>, IdSpan<Self>),
    /// Lambda call annotated with arity
    Func(usize, IdSpan<Self>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PolarType<T: PolarPrimitive> {
    Prim(T),
    Var(VarId),
}

pub type Pos = PolarType<PosPrim>;
pub type Neg = PolarType<NegPrim>;

pub type PosId = Id<PosPrim>;
pub type NegId = Id<NegPrim>;

pub type PosIdS = IdSpan<PosPrim>;
pub type NegIdS = IdSpan<NegPrim>;

#[derive(Debug, Default)]
pub struct VarState {
    label: Option<String>,
    union: IndexSet<PosIdS>,
    inter: IndexSet<NegIdS>,
    level: u16,
    relevels: BTreeMap<RelevelId, VarId>,
}

#[derive(Debug, Default)]
pub struct Relevel {
    // bool 1: pos processed, bool 2: neg processed
    var_cache: HashMap<VarId, (VarId, bool, bool)>,
    ty_cache_1: HashMap<IdSpan<PosPrim>, IdSpan<PosPrim>>,
    ty_cache_2: HashMap<IdSpan<NegPrim>, IdSpan<NegPrim>>,
    level: u16,
    raise: bool,
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
    labels: Vec<String>,
    relevels: Vec<Relevel>,
}

impl ConstraintGraph {
    fn enqueue(&mut self, lhs: PosIdS, rhs: NegIdS) {
        if self.constraints.insert((lhs, rhs)) {
            self.queue.push_back((lhs, rhs));
        }
    }
}

impl TypeCk {
    pub fn add_label(&mut self, label: String) -> LabelId {
        self.labels.push(label);
        LabelId(NonZeroUsize::new(self.labels.len()).unwrap())
    }
    pub fn label(&self, id: LabelId) -> &str {
        &self.labels[id.0.get() - 1]
    }
    pub fn add_var(&mut self, label: Option<String>, level: u16) -> VarId {
        self.vars.push(VarState {
            label,
            level,
            ..VarState::default()
        });
        VarId(self.vars.len() - 1)
    }
    pub fn add_ty<T: PolarPrimitive>(&mut self, ty: PolarType<T>, span: Span) -> IdSpan<T> {
        let (types, spans) = T::typeck_data_mut(self);
        types.push(ty);
        spans.push(span);
        IdSpan::new(Id::new(types.len() - 1), span)
    }
    pub fn var<'a, T: 'a + PolarPrimitive>(
        &'a self,
        var: VarId,
    ) -> impl '_ + Iterator<Item = Id<T>> {
        T::var_data(&self.vars[var.0]).into_iter().map(IdSpan::id)
    }
    pub fn ty<T: PolarPrimitive>(&self, id: Id<T>) -> &PolarType<T> {
        &T::typeck_data(self)[id.id()]
    }
    pub fn pos_as_var(&self, pos: PosId) -> Option<VarId> {
        match self.ty(pos) {
            Pos::Var(var) => Some(*var),
            _ => None,
        }
    }
    pub fn level<T: PolarPrimitive>(&self, ty: &PolarType<T>) -> u16 {
        ty.ids()
            .map(|id| match id {
                AnyIdRef::Same(x) => self.level(self.ty(x.id())),
                AnyIdRef::Inverse(x) => self.level(self.ty(x.id())),
                AnyIdRef::Var(x) => self.vars[x.0].level,
            })
            .max()
            .unwrap_or(0)
    }
    pub fn flow(&mut self, pos: PosIdS, neg: NegIdS) -> Result<(), diag::TypeError> {
        // reuse queue buffer, but clear it every time
        self.q.queue.clear();
        self.q.enqueue(pos, neg);
        while let Some((pos, neg)) = self.q.queue.pop_front() {
            match (&self.pos[pos.id().id()], &self.neg[neg.id().id()]) {
                (Pos::Prim(PosPrim::Bool), Neg::Prim(NegPrim::Bool)) => {}
                (
                    Pos::Prim(PosPrim::Int {
                        signed: s1,
                        bits: b1,
                    }),
                    Neg::Prim(NegPrim::Int {
                        signed: s2,
                        bits: b2,
                    }),
                ) => {
                    if s1 != s2 || b1 != b2 {
                        // coercion not implemented
                        return Err(diag::TypeError::new(
                            HumanType::from_pos(self, pos.id()),
                            HumanType::from_neg(self, neg.id()),
                        )
                        .with_hint(diag::TypeHint::NoCoercion {
                            value: pos.span(),
                            used: neg.span(),
                        }));
                    }
                }
                (
                    Pos::Prim(PosPrim::Float { bits: b1 }),
                    Neg::Prim(NegPrim::Float { bits: b2 }),
                ) => {
                    if b1 != b2 {
                        // coercion not implemented
                        return Err(diag::TypeError::new(
                            HumanType::from_pos(self, pos.id()),
                            HumanType::from_neg(self, neg.id()),
                        )
                        .with_hint(diag::TypeHint::NoCoercion {
                            value: pos.span(),
                            used: neg.span(),
                        }));
                    }
                }
                (Pos::Prim(PosPrim::IntLiteral { .. }), Neg::Prim(NegPrim::Float { .. })) => {}
                (Pos::Prim(PosPrim::FloatLiteral { .. }), Neg::Prim(NegPrim::Float { .. })) => {}
                (
                    Pos::Prim(PosPrim::IntLiteral {
                        signed: s1,
                        bits: b1,
                    }),
                    Neg::Prim(NegPrim::Int {
                        signed: s2,
                        bits: b2,
                    }),
                ) => {
                    if (s1 != s2 || b1 > b2) && (!s2 || *s1 || b1 >= b2) {
                        return Err(diag::TypeError::new(
                            HumanType::from_pos(self, pos.id()),
                            HumanType::from_neg(self, neg.id()),
                        )
                        .with_hint(diag::TypeHint::NoCoercion {
                            value: pos.span(),
                            used: neg.span(),
                        }));
                    }
                }
                (Pos::Prim(PosPrim::Record(pos1)), Neg::Prim(NegPrim::Record(k, neg))) => {
                    let (mut pos, pos1) = (pos1.iter(), pos);
                    if let Some((_, pos)) = pos.find(|x| x.0 == k) {
                        self.q.enqueue(*pos, *neg);
                    } else {
                        // missing field
                        return Err(diag::TypeError::new(
                            HumanType::from_pos(self, pos1.id()),
                            HumanType::from_neg(self, neg.id()),
                        )
                        .with_hint(diag::TypeHint::MissingField {
                            field: k.clone(),
                            used: neg.span(),
                        }));
                    }
                }
                (
                    Pos::Prim(PosPrim::Lambda(pos_a, pos_b)),
                    Neg::Prim(NegPrim::Lambda(neg_a, neg_b)),
                ) => {
                    // ensure function of type pos_a -> pos_b can consume values of type neg_a
                    self.q.enqueue(*neg_a, *pos_a);
                    // ensure function of type pos_a -> pos_b produces values of type neg_b
                    self.q.enqueue(*pos_b, *neg_b);
                }
                (Pos::Prim(PosPrim::Func(argc0, pos1)), Neg::Prim(NegPrim::Func(argc1, neg1))) => {
                    if *argc0 == *argc1 {
                        self.q.enqueue(*pos1, *neg1);
                    } else {
                        // arity mismatch
                        return Err(diag::TypeError::new(
                            HumanType::from_pos(self, pos.id()),
                            HumanType::from_neg(self, neg.id()),
                        )
                        .with_hint(diag::TypeHint::ArityMismatch {
                            count: *argc1,
                            declared: vec![(*argc0, pos1.span())],
                            used: neg1.span(),
                        }));
                    }
                }
                (
                    Pos::Prim(PosPrim::Label(label1, _)),
                    Neg::Prim(NegPrim::Label { cases, fallthrough }),
                ) => {
                    let cases1 = if let Some((ty2, refutable, flow)) = cases.get(label1) {
                        Some((*ty2, *flow))
                            .into_iter()
                            .chain(refutable.then(|| *fallthrough).flatten())
                    } else {
                        None.into_iter().chain(*fallthrough)
                    };
                    let mut handled = false;
                    for (case, flow) in cases1 {
                        self.q.enqueue(pos, case);
                        if let Some((flow_pos, flow_neg)) = flow {
                            self.q.enqueue(flow_pos, flow_neg);
                        }
                        handled = true;
                    }
                    if !handled {
                        return Err(diag::TypeError::new(
                            HumanType::from_pos(self, pos.id()),
                            HumanType::from_neg(self, neg.id()),
                        )
                        .with_hint(diag::TypeHint::UnhandledCase {
                            case: self.label(*label1).to_owned(),
                            created: pos.span(),
                        }));
                    }
                }
                (Pos::Var(pos), neg0) => {
                    let pos = *pos;
                    let neg = if self.vars[pos.0].level < self.level(neg0) {
                        let relevel = self.add_relevel(self.vars[pos.0].level, true);
                        self.relevel(neg, relevel)
                    } else {
                        neg
                    };
                    if self.vars[pos.0].inter.insert(neg) {
                        for (relevel, pos1) in self.vars[pos.0].relevels.clone().iter() {
                            let changed = self.relevel(neg, *relevel);
                            self.vars[pos1.0].inter.insert(changed);
                        }
                        for x in &self.vars[pos.0].union {
                            self.q.enqueue(*x, neg);
                        }
                    }
                }
                (pos0, Neg::Var(neg)) => {
                    let neg = *neg;
                    let pos = if self.vars[neg.0].level < self.level(pos0) {
                        let relevel = self.add_relevel(self.vars[neg.0].level, true);
                        self.relevel(pos, relevel)
                    } else {
                        pos
                    };
                    if self.vars[neg.0].union.insert(pos) {
                        for (relevel, neg1) in self.vars[neg.0].relevels.clone().iter() {
                            let changed = self.relevel(pos, *relevel);
                            self.vars[neg1.0].union.insert(changed);
                        }
                        for y in &self.vars[neg.0].inter {
                            self.q.enqueue(pos, *y);
                        }
                    }
                }
                (Pos::Prim(PosPrim::Label(_, ty)), _) => {
                    self.q.enqueue(*ty, neg);
                }
                (
                    _,
                    Neg::Prim(NegPrim::Label {
                        fallthrough: Some((fallthrough, flow)),
                        ..
                    }),
                ) => {
                    self.q.enqueue(pos, *fallthrough);
                    if let Some((flow_pos, flow_neg)) = flow {
                        self.q.enqueue(*flow_pos, *flow_neg);
                    }
                }
                (pos1, neg1) => {
                    println!("{pos1:?} {neg1:?}");
                    return Err(diag::TypeError::new(
                        HumanType::from_pos(self, pos.id()),
                        HumanType::from_neg(self, neg.id()),
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
    pub fn add_relevel(&mut self, level: u16, raise: bool) -> RelevelId {
        self.relevels.push(Relevel {
            level,
            raise,
            ty_cache_1: HashMap::new(),
            ty_cache_2: HashMap::new(),
            var_cache: HashMap::new(),
        });
        RelevelId(self.relevels.len() - 1)
    }
    /// This is a function used for let polymorphism. Each time the let binding is used, it may
    /// have different types - for example, an identity function may have the type `bool -> bool`
    /// in one place and `int -> int` in another place. To achieve this, we need to quantify
    /// all free variables in the binding (instead of plain variables that we add constraints too,
    /// they become quantified "forall" variables that can have different values in different
    /// places)
    ///
    /// For this, we use the concept of levels. Levels keep track of how deeply this let binding
    /// was defined. Within the binding context (i.e. in all expressions that are to be bound to
    /// variables), all types have their level incremented by 1. Note that this doesn't happen for
    /// the let expression body (the expression to be evaluated with the bindings in scope).
    ///
    /// Levels allow us to efficiently keep track of where each type is defined, and whether we
    /// should treat it as a monomorphic or a polymorphic type.
    ///
    /// - If the type has a greater level than we are currently on, it is a polymorphic type.
    /// - Otherwise, it is a monomorphic type.
    ///
    /// This function serves dual purpose:
    ///
    /// - When types of a higher level are connected to variables from a lower level, the lower
    ///   level variable has to be "copied" to the higher level, because otherwise a direct flow
    ///   would be created between lower level types and higher level types, which defeats the
    ///   entire purpose of type levels (TODO explain why). This is the `raise == true`
    ///   behavior.
    /// - Copy all the variables on higher level to the current level. In that case, the type is
    ///   simply copied, to make sure we can type it differently this time - plain and simple.
    ///   This is the `raise == false` behavior.
    ///
    /// For more info, see https://okmij.org/ftp/ML/generalization.html#levels and
    /// https://lptk.github.io/programming/2020/03/26/demystifying-mlsub.html#efficient-generalization
    fn relevel<T: PolarPrimitive>(&mut self, ty: IdSpan<T>, relevel_id: RelevelId) -> IdSpan<T> {
        let mut relevel = &mut self.relevels[relevel_id.0];
        if let Some(entry) = T::relevel_data_mut(relevel).get(&ty) {
            return *entry;
        }
        let ty1 = self.ty(ty.id());
        let mut ty1 = ty1.clone();
        for id in ty1.ids_mut() {
            match id {
                AnyIdMut::Same(x) => {
                    *x = self.relevel(*x, relevel_id);
                }
                AnyIdMut::Inverse(x) => {
                    *x = self.relevel(*x, relevel_id);
                }
                AnyIdMut::Var(var) => {
                    let var1 = *var;
                    relevel = &mut self.relevels[relevel_id.0];
                    let (ret, ok1, ok2) = if let Some(x) = relevel.var_cache.get_mut(&var1) {
                        x
                    } else {
                        let level = self.relevels[relevel_id.0].level;
                        let var2 = self.add_var(self.vars[var1.0].label.clone(), level);
                        self.vars[var1.0].relevels.insert(relevel_id, var2);
                        let x = (var2, false, false);
                        relevel = &mut self.relevels[relevel_id.0];
                        relevel.var_cache.insert(var1, x);
                        relevel.var_cache.get_mut(&var1).unwrap()
                    };
                    let (ok1, ok2) = if T::POSITIVE { (ok1, ok2) } else { (ok2, ok1) };
                    let ret = *ret;
                    if !*ok1 {
                        *ok1 = true;
                        if relevel.raise {
                            // connect this var to the other var
                            let neg = self.add_ty(PolarType::Var(ret), ty.span());
                            T::Inverse::var_data_mut(&mut self.vars[var1.0]).insert(neg);
                        } else if !*ok2 {
                            // connect this var to whatever the other var is connected to
                            *ok2 = true;
                            for x in &T::Inverse::var_data(&self.vars[var1.0]).clone() {
                                let x = self.relevel(*x, relevel_id);
                                T::Inverse::var_data_mut(&mut self.vars[var1.0]).insert(x);
                            }
                        }
                        for x in &T::var_data(&self.vars[var1.0]).clone() {
                            let x = self.relevel(*x, relevel_id);
                            T::var_data_mut(&mut self.vars[ret.0]).insert(x);
                        }
                    }
                    *var = ret;
                }
            }
        }
        let ret = self.add_ty(ty1, ty.span());
        relevel = &mut self.relevels[relevel_id.0];
        T::relevel_data_mut(relevel).insert(ty, ret);
        ret
    }
    pub fn monomorphize<T: PolarPrimitive>(&mut self, ty: IdSpan<T>, level: u16) -> IdSpan<T> {
        let relevel = self.add_relevel(level, false);
        self.relevel(ty, relevel)
    }
    fn gv_node_id<T: PolarPrimitive>(&self, p: Id<T>) -> String {
        match self.ty(p) {
            PolarType::Var(v) => format!("{}v{}", if T::POSITIVE { '+' } else { '-' }, v.0),
            _ => format!("{}{}", if T::POSITIVE { '+' } else { '-' }, p.id()),
        }
    }
    pub fn graphviz(&self, src: &str) -> String {
        let mut ret = vec!["strict digraph t {".to_owned()];
        let id = |id: &str| format!("{:?}", id);
        for (i, _var) in self.pos.iter().enumerate() {
            let name = id(&self.gv_node_id(PosId::new(i)));
            let mut label = self.gv_node_id(PosId::new(i));
            if label.contains('v') {
                label.push('\n');
                label.push_str(&format!("+{i}"));
            }
            label.push('\n');
            label.push_str(&format!("{}", HumanType::from_pos(self, PosId::new(i))));
            label.push('\n');
            label.push_str(self.pos_spans[i].text(src));
            ret.push(format!("  {name}[label={}]", id(&label)));
        }
        for (i, _var) in self.neg.iter().enumerate() {
            let name = id(&self.gv_node_id(NegId::new(i)));
            let mut label = self.gv_node_id(NegId::new(i));
            if label.contains('v') {
                label.push('\n');
                label.push_str(&format!("-{i}"));
            }
            label.push('\n');
            label.push_str(&format!("{}", HumanType::from_neg(self, NegId::new(i))));
            label.push('\n');
            label.push_str(self.neg_spans[i].text(src));
            ret.push(format!("  {name}[label={}]", id(&label)));
        }
        for (i, var) in self.vars.iter().enumerate() {
            let pos = id(&format!("+v{}", i));
            let neg = id(&format!("-v{}", i));
            let label = id(&format!("{:?}", var.label));
            ret.push(format!("  {neg} -> {pos}[label={label}]"));
        }
        let mut added = HashSet::new();
        for (pos, neg) in &self.q.constraints {
            let pos = id(&self.gv_node_id(pos.id()));
            let neg = id(&self.gv_node_id(neg.id()));
            ret.push(format!("  {pos} -> {neg} [color=red]"));
            added.insert((pos, neg));
        }
        for var in self.neg.iter() {
            if let Neg::Prim(NegPrim::Label { cases, fallthrough }) = var {
                for (pos, neg) in cases
                    .values()
                    .filter_map(|case| case.2)
                    .chain(fallthrough.and_then(|x| x.1))
                {
                    let pos = id(&self.gv_node_id(pos.id()));
                    let neg = id(&self.gv_node_id(neg.id()));
                    if added.insert((pos.clone(), neg.clone())) {
                        ret.push(format!("  {pos} -> {neg} [color=blue]"));
                    }
                }
            }
        }
        ret.push("}".to_owned());
        ret.join("\n")
    }
}
