use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    hash::Hash,
};

use polar::{AnyIdMut, AnyIdRef, PolarPrimitive};

use crate::{
    diag::{self, HumanType},
    util::{Id, IdSpan, OrderedSet},
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
    union: OrderedSet<PosIdS>,
    inter: OrderedSet<NegIdS>,
    level: u16,
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
                (Pos::Func(pos_a, pos_b), Neg::Func(neg_a, neg_b)) => {
                    // ensure function of type pos_a -> pos_b can consume values of type neg_a
                    self.q.enqueue(*neg_a, *pos_a);
                    // ensure function of type pos_a -> pos_b produces values of type neg_b
                    self.q.enqueue(*pos_b, *neg_b);
                }
                (Pos::Var(pos), neg0) => {
                    let pos = *pos;
                    let neg = if self.vars[pos.0].level < self.level(neg0) {
                        self.change_level(
                            neg,
                            self.vars[pos.0].level,
                            &mut HashMap::new(),
                            &mut HashMap::new(),
                            &mut HashMap::new(),
                            true,
                        )
                    } else {
                        neg
                    };
                    if self.vars[pos.0].inter.insert(neg).0 {
                        for x in &self.vars[pos.0].union {
                            self.q.enqueue(*x, neg);
                        }
                    }
                }
                (pos0, Neg::Var(neg)) => {
                    let neg = *neg;
                    let pos = if self.vars[neg.0].level < self.level(pos0) {
                        self.change_level(
                            pos,
                            self.vars[neg.0].level,
                            &mut HashMap::new(),
                            &mut HashMap::new(),
                            &mut HashMap::new(),
                            true,
                        )
                    } else {
                        pos
                    };
                    if self.vars[neg.0].union.insert(pos).0 {
                        for y in &self.vars[neg.0].inter {
                            self.q.enqueue(pos, *y);
                        }
                    }
                }
                (_, _) => {
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
        var_cache: &mut HashMap<VarId, (VarId, bool, bool)>,
        ty_cache_1: &mut HashMap<IdSpan<T>, IdSpan<T>>,
        ty_cache_2: &mut HashMap<IdSpan<T::Inverse>, IdSpan<T::Inverse>>,
        raise: bool,
    ) -> IdSpan<T> {
        if let Some(entry) = ty_cache_1.get(&ty) {
            return *entry;
        }
        let ty1 = self.ty(ty.id());
        let mut ty1 = ty1.clone();
        for id in ty1.ids_mut() {
            match id {
                AnyIdMut::Same(x) => {
                    *x = self.change_level(*x, level, var_cache, ty_cache_1, ty_cache_2, raise);
                }
                AnyIdMut::Inverse(x) => {
                    *x = self.change_level(*x, level, var_cache, ty_cache_2, ty_cache_1, raise);
                }
                AnyIdMut::Var(var) => {
                    let var1 = *var;
                    let (ret, ok1, ok2) = var_cache.entry(var1).or_insert_with(|| {
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
                            T::Inverse::var_data_mut(&mut self.vars[var1.0]).insert(neg);
                        } else {
                            *ok2 = true;
                            for x in &T::Inverse::var_data(&self.vars[var1.0]).clone() {
                                let x = self.change_level(
                                    *x, level, var_cache, ty_cache_2, ty_cache_1, raise,
                                );
                                T::Inverse::var_data_mut(&mut self.vars[var1.0]).insert(x);
                            }
                        }
                        for x in &T::var_data(&self.vars[var1.0]).clone() {
                            let x = self
                                .change_level(*x, level, var_cache, ty_cache_1, ty_cache_2, raise);
                            T::var_data_mut(&mut self.vars[ret.0]).insert(x);
                        }
                    }
                    *var = ret;
                }
            }
        }
        let ret = self.add_ty(ty1, ty.span());
        ty_cache_1.insert(ty, ret);
        ret
    }
    pub fn monomorphize<T: PolarPrimitive>(&mut self, ty: IdSpan<T>, level: u16) -> IdSpan<T> {
        self.change_level(
            ty,
            level,
            &mut HashMap::new(),
            &mut HashMap::new(),
            &mut HashMap::new(),
            false,
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
        for (pos, neg) in &self.q.constraints {
            let pos = id(&self.gv_node_id(pos.id()));
            let neg = id(&self.gv_node_id(neg.id()));
            ret.push(format!("  {pos} -> {neg} [color=red]"));
        }
        ret.push("}".to_owned());
        ret.join("\n")
    }
}
