use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    hash::Hash,
};

use crate::{
    diag::{self, HumanType},
    util::{Id, IdSpan, OrderedSet},
    Span,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VarId(usize);

impl VarId {
    pub fn polarize(&self, ck: &mut TypeCk, span: Span) -> (PosIdS, NegIdS) {
        let (pos, neg) = (Pos::Var(*self), Neg::Var(*self));
        (ck.add_ty(pos, span), ck.add_ty(neg, span))
    }
}

pub trait PolarPrimitive: Clone + Eq + Hash {
    type Inverse: PolarPrimitive<Inverse = Self>;
    const POSITIVE: bool;
    fn typeck_data_mut(ck: &mut TypeCk) -> (&mut Vec<PolarType<Self>>, &mut Vec<Span>);
    fn typeck_data(ck: &TypeCk) -> &Vec<PolarType<Self>>;
    fn var_data_mut(var: &mut VarState) -> &mut OrderedSet<IdSpan<Self>>;
    fn var_data(var: &VarState) -> &OrderedSet<IdSpan<Self>>;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PosPrim {
    Void,
    Bool,
    Int { signed: bool, bits: u8 },
    Float { bits: u8 },
    IntLiteral { signed: bool, bits: u8 },
    FloatLiteral,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum NegPrim {
    Void,
    Bool,
    Int { signed: bool, bits: u8 },
    Float { bits: u8 },
}

impl PolarPrimitive for PosPrim {
    type Inverse = NegPrim;
    const POSITIVE: bool = true;
    fn typeck_data_mut(ck: &mut TypeCk) -> (&mut Vec<PolarType<Self>>, &mut Vec<Span>) {
        (&mut ck.pos, &mut ck.pos_spans)
    }
    fn typeck_data(ck: &TypeCk) -> &Vec<PolarType<Self>> {
        &ck.pos
    }
    fn var_data_mut(var: &mut VarState) -> &mut OrderedSet<IdSpan<Self>> {
        &mut var.union
    }
    fn var_data(var: &VarState) -> &OrderedSet<IdSpan<Self>> {
        &var.union
    }
}

impl PolarPrimitive for NegPrim {
    type Inverse = PosPrim;
    const POSITIVE: bool = false;
    fn typeck_data_mut(ck: &mut TypeCk) -> (&mut Vec<PolarType<Self>>, &mut Vec<Span>) {
        (&mut ck.neg, &mut ck.neg_spans)
    }
    fn typeck_data(ck: &TypeCk) -> &Vec<PolarType<Self>> {
        &ck.neg
    }
    fn var_data_mut(var: &mut VarState) -> &mut OrderedSet<IdSpan<Self>> {
        &mut var.inter
    }
    fn var_data(var: &VarState) -> &OrderedSet<IdSpan<Self>> {
        &var.inter
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PolarType<T: PolarPrimitive> {
    Prim(T),
    Record(BTreeMap<String, IdSpan<T>>),
    Var(VarId),
    Func(IdSpan<T::Inverse>, IdSpan<T>),
}

pub type PosId = Id<PosPrim>;
pub type NegId = Id<NegPrim>;

pub type PosIdS = IdSpan<PosPrim>;
pub type NegIdS = IdSpan<NegPrim>;

pub type Pos = PolarType<PosPrim>;
pub type Neg = PolarType<NegPrim>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum AnyId {
    Pos(PosIdS),
    Neg(NegIdS),
}

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
        match ty {
            PolarType::Var(var) => self.vars[var.0].level,
            PolarType::Func(neg, pos) => self
                .level(self.ty(neg.id()))
                .max(self.level(self.ty(pos.id()))),
            PolarType::Record(rec) => rec
                .values()
                .map(|x| self.level(self.ty(x.id())))
                .max()
                .unwrap_or(0),
            _ => 0,
        }
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
                (Pos::Record(pos1), Neg::Record(neg)) => {
                    let (mut pos, pos1) = (pos1.iter(), pos);
                    for (k, neg) in neg {
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
                        self.monomorphize2(
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
                        self.monomorphize2(
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
    fn monomorphize2<T: PolarPrimitive>(
        &mut self,
        ty: IdSpan<T>,
        level: u16,
        var_cache: &mut HashMap<VarId, (VarId, bool, bool)>,
        ty_cache_1: &mut HashMap<IdSpan<T>, IdSpan<T>>,
        ty_cache_2: &mut HashMap<IdSpan<T::Inverse>, IdSpan<T::Inverse>>,
        add_link: bool,
    ) -> IdSpan<T> {
        if let Some(entry) = ty_cache_1.get(&ty) {
            return *entry;
        }
        let ty1 = self.ty(ty.id());
        let ty1 = match ty1 {
            PolarType::Var(var) => {
                let var = *var;
                let (ret, ok1, ok2) = var_cache.entry(var).or_insert_with(|| {
                    (
                        self.add_var(self.vars[var.0].label.clone(), level),
                        false,
                        false,
                    )
                });
                let (ok1, ok2) = if T::POSITIVE { (ok1, ok2) } else { (ok2, ok1) };
                let ret = *ret;
                if !*ok1 {
                    *ok1 = true;
                    if add_link {
                        let neg = self.add_ty(PolarType::Var(ret), ty.span());
                        T::Inverse::var_data_mut(&mut self.vars[var.0]).insert(neg);
                    } else {
                        *ok2 = true;
                        for x in &T::Inverse::var_data(&self.vars[var.0]).clone() {
                            let x = self.monomorphize2(
                                *x, level, var_cache, ty_cache_2, ty_cache_1, add_link,
                            );
                            T::Inverse::var_data_mut(&mut self.vars[var.0]).insert(x);
                        }
                    }
                    for x in &T::var_data(&self.vars[var.0]).clone() {
                        let x = self
                            .monomorphize2(*x, level, var_cache, ty_cache_1, ty_cache_2, add_link);
                        T::var_data_mut(&mut self.vars[ret.0]).insert(x);
                    }
                }
                PolarType::Var(ret)
            }
            PolarType::Func(neg, pos) => {
                let neg = *neg;
                let pos = *pos;
                PolarType::Func(
                    self.monomorphize2(neg, level, var_cache, ty_cache_2, ty_cache_1, add_link),
                    self.monomorphize2(pos, level, var_cache, ty_cache_1, ty_cache_2, add_link),
                )
            }
            PolarType::Record(x) => {
                let mut rec = x.clone();
                for v in rec.values_mut() {
                    self.monomorphize2(*v, level, var_cache, ty_cache_1, ty_cache_2, add_link);
                }
                PolarType::Record(rec)
            }
            _ => return ty,
        };
        let ret = self.add_ty(ty1, ty.span());
        ty_cache_1.insert(ty, ret);
        ret
    }
    pub fn monomorphize(&mut self, pos: PosIdS, level: u16) -> PosIdS {
        self.monomorphize2(
            pos,
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
