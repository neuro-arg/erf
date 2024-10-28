use std::{
    collections::{BTreeMap, HashMap, VecDeque},
    hash::Hash,
    num::NonZeroU32,
};

use indexmap::IndexMap;
use polar::{AnyId, AnyIdMut, AnyIdRef, PolarPrimitive};

use crate::{
    diag::{self, HumanType},
    util::{Id, IdSpan},
    Span,
};

mod polar;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VarId(usize);

impl VarId {
    pub fn polarize(&self, ck: &mut TypeCk, span: Span) -> (PosIdS, NegIdS) {
        let (pos, neg) = (Pos::Var(*self), Neg::Var(*self));
        (ck.add_ty(pos, span), ck.add_ty(neg, span))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PosPrim {
    Void,
    Bool,
    Int { signed: bool, bits: u8 },
    Float { bits: u8 },
    IntLiteral { signed: bool, bits: u8 },
    FloatLiteral,
    Record(BTreeMap<String, IdSpan<Self>>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NegPrim {
    Void,
    Bool,
    Int { signed: bool, bits: u8 },
    Float { bits: u8 },
    Record(String, IdSpan<Self>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PolarType<T: PolarPrimitive> {
    Prim(T),
    Var(VarId),
    Func(IdSpan<T::Inverse>, IdSpan<T>),
}

pub type Pos = PolarType<PosPrim>;
pub type Neg = PolarType<NegPrim>;

pub type PosId = Id<PosPrim>;
pub type NegId = Id<NegPrim>;

pub type PosIdS = IdSpan<PosPrim>;
pub type NegIdS = IdSpan<NegPrim>;

#[derive(Clone, Debug, Default)]
pub struct VarState {
    label: Option<String>,
    union: IndexMap<PosIdS, Edge>,
    inter: IndexMap<NegIdS, Edge>,
    level: u16,
    /// If a var is a boundary, it doesn't pass any constraints through itself.
    /// The only exception is the `Ordering` constraint, which it does pass.
    boundary: bool,
}

#[derive(Debug, Default)]
struct ConstraintGraph {
    constraints: HashMap<(PosIdS, NegIdS), Edge>,
    queue: VecDeque<(PosIdS, NegIdS, Edge)>,
    queries: Vec<Query>,
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
    fn enqueue(&mut self, lhs: PosIdS, rhs: NegIdS, edge: Edge) {
        let mut visit_anyway = false;
        let should_visit = self
            .constraints
            .entry((lhs, rhs))
            .or_insert_with(|| {
                visit_anyway = true;
                Edge::Noop
            })
            .merge(edge.clone());
        if should_visit || visit_anyway {
            self.queue.push_back((lhs, rhs, edge));
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct QueryId(usize);

/// Strong edges are used for mandatory constraints (i.e. check that the program is sound)
///
/// Weak edges are used for optional constraints that can be queried (i.e. are used for program
/// analysis, which may affect compiler optimizations or even type flow)
///
/// If a weak edge exists, a strong edge can still be added, but if a strong edge exists, there's
/// no point in adding a weak version of that exact edge
///
/// Everything is complicated by the fact there may exist some connections created by multiple weak
/// edges. In that case, we simply mark the edge as "potentially possible", but only visit it at
/// the end to make sure the other queries don't get invalidated by strict edges later.
#[derive(Clone, Debug)]
pub enum Edge {
    /// A no-op edge
    Noop,
    /// Weak edge, failure notifies a query.
    Weak { queries: HashMap<QueryId, u32> },
    /// Strong edge, failure to add = type error.
    Strong,
}

impl Edge {
    pub fn merge(&mut self, rhs: Edge) -> bool {
        match (self, &rhs) {
            // if this is already a strong edge, ignore
            (Edge::Strong, _) => false,
            // if this a weak edge and we're adding a weak edge, merge them
            (Edge::Weak { queries }, Edge::Weak { queries: q2 }) => {
                let mut new = false;
                for (query, generation) in q2 {
                    let old_gen = queries.entry(*query).or_insert_with(|| {
                        new = true;
                        0
                    });
                    // if this query is already there with a >= generation, ignore
                    // otherwise, visit
                    if *old_gen < *generation {
                        new = true;
                    }
                }
                new
            }
            // if the edge is currently weak and we want it to be strong, so be it
            (this @ Edge::Weak { .. }, Edge::Strong) => {
                *this = Edge::Strong;
                true
            }
            // if we want to merge in a no-op edge, nothing has to be done
            (_, Edge::Noop) => false,
            // if this is a no-op currently, change that
            (this @ Edge::Noop, _) => {
                *this = rhs;
                true
            }
        }
    }
}

#[derive(Debug)]
pub enum Query {
    /// Delayed query with no immediate fail handlers
    Delayed { success: bool },
}

impl Query {
    /// The current flow generation of this query. Flows created with the old generation are
    /// considered invalid.
    pub fn generation(&self) -> NonZeroU32 {
        match self {
            Self::Delayed { success } => {
                if *success {
                    1.try_into().unwrap()
                } else {
                    2.try_into().unwrap()
                }
            }
        }
    }
}

struct VarTypeCache<'a, T: PolarPrimitive> {
    var_cache: &'a mut HashMap<VarId, (VarId, bool, bool)>,
    ty_cache_1: &'a mut HashMap<IdSpan<T>, IdSpan<T>>,
    ty_cache_2: &'a mut HashMap<IdSpan<T::Inverse>, IdSpan<T::Inverse>>,
}

impl<'a, T: PolarPrimitive> VarTypeCache<'a, T> {
    fn invert(&mut self) -> VarTypeCache<'_, T::Inverse> {
        VarTypeCache {
            var_cache: self.var_cache,
            ty_cache_1: self.ty_cache_2,
            ty_cache_2: self.ty_cache_1,
        }
    }
}

impl TypeCk {
    pub fn add_query(&mut self, query: Query) -> QueryId {
        self.q.queries.push(query);
        QueryId(self.q.queries.len() - 1)
    }
    pub fn add_var(&mut self, label: Option<String>, level: u16) -> VarId {
        self.vars.push(VarState {
            label,
            level,
            ..VarState::default()
        });
        VarId(self.vars.len() - 1)
    }
    pub fn add_boundary_var(&mut self, label: Option<String>, level: u16) -> VarId {
        self.vars.push(VarState {
            label,
            level,
            // boundary: true,
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
        T::var_data(&self.vars[var.0]).keys().map(IdSpan::id)
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
    /// Remove a boundary from the var
    fn unblock_var(&mut self, var: VarId, var_span: Span) -> Result<(), diag::TypeError> {
        assert!(self.vars[var.0].boundary);
        let mut union = IndexMap::new();
        let mut inter = IndexMap::new();
        std::mem::swap(&mut union, &mut self.vars[var.0].union);
        std::mem::swap(&mut inter, &mut self.vars[var.0].inter);
        self.vars[var.0].boundary = false;
        let (var_pos, var_neg) = var.polarize(self, var_span);
        for (pos, edge) in union {
            if !matches!(edge, Edge::Noop) {
                self.flow(pos, var_neg, edge.clone())?;
            }
        }
        for (neg, edge) in inter {
            if !matches!(edge, Edge::Noop) {
                self.flow(var_pos, neg, edge.clone())?;
            }
        }
        Ok(())
    }
    /// Post-processing (should be done after all the types have been added)
    /// This removes all var boundaries, letting types flow freely
    pub fn postproc(&mut self) -> Result<(), diag::TypeError> {
        let mut stack = Vec::new();
        let mut var_vis = vec![false; self.vars.len()];
        let mut pos_vis = vec![false; self.pos.len()];
        enum StackElem {
            Visit(PosIdS),
            Unblock(VarId, Span),
        }
        // visit everything in the stack and all unvisited preceding flows
        let dfs = |stack: &mut Vec<StackElem>,
                   var_vis: &mut Vec<bool>,
                   pos_vis: &mut Vec<bool>,
                   this: &mut Self|
         -> Result<(), diag::TypeError> {
            while let Some(elem) = stack.pop() {
                let pos_id = match elem {
                    StackElem::Visit(x) => x,
                    StackElem::Unblock(var, span) => {
                        this.unblock_var(var, span)?;
                        continue;
                    }
                };
                for id in this.pos[pos_id.id().id()]
                    .ids()
                    .map(|x| x.into_owned())
                    .collect::<Vec<_>>()
                {
                    // Invariant: if AnyId::Var is returned, nothing else must be returned.
                    // Otherwise the stack might get messed up, as all preceding nodes must be
                    // visited before the var. Also, `pos_id.span()` is assumed to be the var's
                    // span, so if that's not the case, we have issues.
                    match id {
                        AnyId::Var(var) => {
                            if var_vis[var.0] {
                                continue;
                            }
                            var_vis[var.0] = true;
                            if this.vars[var.0].boundary {
                                stack.push(StackElem::Unblock(var, pos_id.span()));
                            }
                            for id in this.vars[var.0].union.keys() {
                                if pos_vis[id.id().id()] {
                                    continue;
                                }
                                pos_vis[id.id().id()] = true;
                                stack.push(StackElem::Visit(*id));
                            }
                        }
                        AnyId::Same(p) => {
                            if pos_vis[p.id().id()] {
                                continue;
                            }
                            pos_vis[p.id().id()] = true;
                            stack.push(StackElem::Visit(p));
                        }
                        AnyId::Inverse(_) => {}
                    }
                }
            }
            Ok(())
        };
        for id in 0..self.pos.len() {
            if !pos_vis[id] {
                pos_vis[id] = true;
                stack.push(StackElem::Visit(PosIdS::new(
                    PosId::new(id),
                    self.pos_spans[id],
                )));
            }
            dfs(&mut stack, &mut var_vis, &mut pos_vis, self)?;
        }
        for id in 0..self.pos.len() {
            match self.pos[id] {
                Pos::Var(v) if self.vars[v.0].boundary => {}
                _ => {}
            }
        }
        Ok(())
    }
    pub fn flow(&mut self, pos: PosIdS, neg: NegIdS, edge: Edge) -> Result<(), diag::TypeError> {
        // reuse queue buffer, but clear it every time
        self.q.queue.clear();
        self.q.enqueue(pos, neg, edge);
        while let Some((pos, neg, edge)) = self.q.queue.pop_front() {
            match (&self.pos[pos.id().id()], &self.neg[neg.id().id()]) {
                (Pos::Prim(PosPrim::Void), Neg::Prim(NegPrim::Void)) => {}
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
                        self.fail_edge(edge, |this| {
                            diag::TypeError::new(
                                HumanType::from_pos(this, pos.id()),
                                HumanType::from_neg(this, neg.id()),
                            )
                            .with_hint(diag::TypeHint::NoCoercion {
                                value: pos.span(),
                                used: neg.span(),
                            })
                        })?;
                    }
                }
                (
                    Pos::Prim(PosPrim::Float { bits: b1 }),
                    Neg::Prim(NegPrim::Float { bits: b2 }),
                ) => {
                    if b1 != b2 {
                        // coercion not implemented
                        self.fail_edge(edge, |this| {
                            diag::TypeError::new(
                                HumanType::from_pos(this, pos.id()),
                                HumanType::from_neg(this, neg.id()),
                            )
                            .with_hint(diag::TypeHint::NoCoercion {
                                value: pos.span(),
                                used: neg.span(),
                            })
                        })?;
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
                        self.fail_edge(edge, |this| {
                            diag::TypeError::new(
                                HumanType::from_pos(this, pos.id()),
                                HumanType::from_neg(this, neg.id()),
                            )
                            .with_hint(diag::TypeHint::NoCoercion {
                                value: pos.span(),
                                used: neg.span(),
                            })
                        })?;
                    }
                }
                (Pos::Prim(PosPrim::Record(pos0)), Neg::Prim(NegPrim::Record(k, neg))) => {
                    if let Some((_, pos)) = pos0.iter().find(|x| x.0 == k) {
                        self.q.enqueue(*pos, *neg, edge);
                    } else {
                        let field = k.clone();
                        let neg = *neg;
                        // missing field
                        self.fail_edge(edge, |this| {
                            diag::TypeError::new(
                                HumanType::from_pos(this, pos.id()),
                                HumanType::from_neg(this, neg.id()),
                            )
                            .with_hint(diag::TypeHint::MissingField {
                                field,
                                used: neg.span(),
                            })
                        })?;
                    }
                }
                (Pos::Func(pos_a, pos_b), Neg::Func(neg_a, neg_b)) => {
                    // ensure function of type pos_a -> pos_b can consume values of type neg_a
                    self.q.enqueue(*neg_a, *pos_a, edge.clone());
                    // ensure function of type pos_a -> pos_b produces values of type neg_b
                    self.q.enqueue(*pos_b, *neg_b, edge);
                }
                (Pos::Var(pos), neg0) => {
                    let pos = *pos;
                    let neg = if self.vars[pos.0].level < self.level(neg0) {
                        self.change_level(
                            neg,
                            self.vars[pos.0].level,
                            &mut VarTypeCache {
                                var_cache: &mut HashMap::new(),
                                ty_cache_1: &mut HashMap::new(),
                                ty_cache_2: &mut HashMap::new(),
                            },
                            true,
                            &edge,
                        )
                    } else {
                        neg
                    };
                    if self.vars[pos.0]
                        .inter
                        .entry(neg)
                        .or_insert_with(|| Edge::Noop)
                        .merge(edge.clone())
                    {
                        let edge = if self.vars[pos.0].boundary {
                            Edge::Noop
                        } else {
                            edge
                        };
                        for x in self.vars[pos.0].union.keys() {
                            self.q.enqueue(*x, neg, edge.clone());
                        }
                    }
                }
                (pos0, Neg::Var(neg)) => {
                    let neg = *neg;
                    let pos = if self.vars[neg.0].level < self.level(pos0) {
                        self.change_level(
                            pos,
                            self.vars[neg.0].level,
                            &mut VarTypeCache {
                                var_cache: &mut HashMap::new(),
                                ty_cache_1: &mut HashMap::new(),
                                ty_cache_2: &mut HashMap::new(),
                            },
                            true,
                            &edge,
                        )
                    } else {
                        pos
                    };
                    if self.vars[neg.0]
                        .union
                        .entry(pos)
                        .or_insert_with(|| Edge::Noop)
                        .merge(edge.clone())
                    {
                        let edge = if self.vars[neg.0].boundary {
                            Edge::Noop
                        } else {
                            edge
                        };
                        for y in self.vars[neg.0].inter.keys() {
                            self.q.enqueue(pos, *y, edge.clone());
                        }
                    }
                }
                (_, _) => {
                    self.fail_edge(edge, |this| {
                        diag::TypeError::new(
                            HumanType::from_pos(this, pos.id()),
                            HumanType::from_neg(this, neg.id()),
                        )
                        .with_hint(diag::TypeHint::NoCoercion {
                            value: pos.span(),
                            used: neg.span(),
                        })
                    })?;
                }
            }
        }
        Ok(())
    }
    /// Return an error returned by `ret` if `edge` is `Edge::Strong`,
    /// otherwise run the applicable processing rules
    fn fail_edge(
        &mut self,
        edge: Edge,
        ret: impl FnOnce(&mut Self) -> diag::TypeError,
    ) -> Result<(), diag::TypeError> {
        match edge {
            Edge::Noop => Ok(()),
            Edge::Weak { queries } => {
                for (query, generation) in &queries {
                    if self.q.queries[query.0].generation().get() == *generation {
                        self.fail_query(*query)?;
                    }
                }
                Ok(())
            }
            Edge::Strong => Err(ret(self)),
        }
    }
    /// This is called as part of `edge`. Fail the query, potentially refreshing the generation and
    /// creating new flows.
    fn fail_query(&mut self, query: QueryId) -> Result<(), diag::TypeError> {
        match &mut self.q.queries[query.0] {
            Query::Delayed { success } => {
                *success = false;
                Ok(())
            }
        }
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
    fn change_level<T: PolarPrimitive>(
        &mut self,
        ty: IdSpan<T>,
        level: u16,
        cache: &mut VarTypeCache<T>,
        raise: bool,
        edge: &Edge,
    ) -> IdSpan<T> {
        if let Some(entry) = cache.ty_cache_1.get(&ty) {
            return *entry;
        }
        let ty1 = self.ty(ty.id());
        let mut ty1 = ty1.clone();
        for id in ty1.ids_mut() {
            match id {
                AnyIdMut::Same(x) => {
                    *x = self.change_level(*x, level, cache, raise, edge);
                }
                AnyIdMut::Inverse(x) => {
                    *x = self.change_level(*x, level, &mut cache.invert(), raise, edge);
                }
                AnyIdMut::Var(var) => {
                    let var1 = *var;
                    let (ret, ok1, ok2) = cache.var_cache.entry(var1).or_insert_with(|| {
                        (
                            self.add_var(self.vars[var1.0].label.clone(), level),
                            false,
                            false,
                        )
                    });
                    let (ok1, ok2) = if T::POSITIVE { (ok1, ok2) } else { (ok2, ok1) };
                    let ret = *ret;
                    if !*ok1 {
                        *ok1 = true;
                        if raise {
                            let neg = self.add_ty(PolarType::Var(ret), ty.span());
                            T::Inverse::var_data_mut(&mut self.vars[var1.0])
                                .entry(neg)
                                .or_insert_with(|| Edge::Noop)
                                .merge(edge.clone());
                        } else {
                            *ok2 = true;
                            for x in T::Inverse::var_data(&self.vars[var1.0])
                                .keys()
                                .cloned()
                                .collect::<Vec<_>>()
                            {
                                let x =
                                    self.change_level(x, level, &mut cache.invert(), raise, edge);
                                T::Inverse::var_data_mut(&mut self.vars[var1.0])
                                    .entry(x)
                                    .or_insert_with(|| Edge::Noop)
                                    .merge(edge.clone());
                            }
                        }
                        for x in T::var_data(&self.vars[var1.0])
                            .keys()
                            .cloned()
                            .collect::<Vec<_>>()
                        {
                            let x = self.change_level(x, level, cache, raise, edge);
                            T::var_data_mut(&mut self.vars[ret.0])
                                .entry(x)
                                .or_insert_with(|| Edge::Noop)
                                .merge(edge.clone());
                        }
                    }
                    *var = ret;
                }
            }
        }
        let ret = self.add_ty(ty1, ty.span());
        cache.ty_cache_1.insert(ty, ret);
        ret
    }
    pub fn monomorphize<T: PolarPrimitive>(&mut self, ty: IdSpan<T>, level: u16) -> IdSpan<T> {
        self.change_level(
            ty,
            level,
            &mut VarTypeCache {
                var_cache: &mut HashMap::new(),
                ty_cache_1: &mut HashMap::new(),
                ty_cache_2: &mut HashMap::new(),
            },
            false,
            &Edge::Strong,
        )
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
            let label = id(&format!("{:?}", var));
            ret.push(format!("  {neg} -> {pos}[info={label}]"));
        }
        for (pos, neg) in self.q.constraints.keys() {
            let pos = id(&self.gv_node_id(pos.id()));
            let neg = id(&self.gv_node_id(neg.id()));
            ret.push(format!("  {pos} -> {neg} [color=red]"));
        }
        ret.push("}".to_owned());
        ret.join("\n")
    }
}
