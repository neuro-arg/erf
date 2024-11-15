// an ir that offers a high level representation of types and control flow
// it's the best choice for stuff like constant folding

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, LinkedList},
    str::FromStr,
};

use indexmap::IndexMap;
use itertools::Itertools;
use malachite::num::logic::traits::SignificantBits;
use smallvec::SmallVec;

use crate::{
    ast::{self, Expr, Ident, LetArm, LetPatternInner},
    diag::{self, NameNotFoundError},
    typeck::{LabelId, Neg, NegIdS, NegPrim, Pos, PosIdS, PosPrim, TypeCk, VarId},
    Span,
};

// VarId technically comes from the type checker,
// but its variable IDs are reused here for convenience.
// Think of them as both HIR variables and type checker
// variables.

#[derive(Clone, Debug)]
pub enum Value {
    FuncEntry(Box<Term>),
    /// arg, body
    Lambda(VarId, Box<Term>),
    Bool(bool),
    Float(f64),
    Int(malachite_nz::integer::Integer),
    Intrinsic(String),
}

#[derive(Copy, Clone, Debug)]
pub enum VarType {
    Mono(VarId),
    Type(VarId, LabelId),
    Poly(VarId),
}

#[derive(Clone, Debug)]
pub enum TermInner {
    Application(Box<Term>, Option<Box<Term>>),
    Bind {
        bindings: Box<[(VarId, Term)]>,
        expr: Box<Term>,
    },
    VarAccess(VarId),
    Value(Value),
    AttachTag(LabelId, Box<Term>),
    CheckTag {
        var: VarId,
        branches: BTreeMap<LabelId, Term>,
        /// Note that this is very literal fallthrough, go here if none of the branches matched,
        /// and if this is None, then fallthrough further
        fallthrough: Option<Box<Term>>,
    },
    /// Run terms in a row, with fallthrough support
    Sequence(Vec<Term>),
    /// Exit this branch and go to If's else or CheckTag's fallthrough or next item in a Sequence.
    /// If there's no other branches, this is undefined behavior.
    /// TODO: disallow Fallthrough from exiting an If and make it only work on Sequence
    Fallthrough,
    If(Box<Term>, Box<Term>, Box<Term>),
}

#[derive(Clone, Debug)]
pub struct Term {
    pub inner: TermInner,
    pub ty: PosIdS,
}

type VarOrder = (BTreeMap<VarId, BTreeSet<VarId>>, Option<VarId>);

#[derive(Clone, Debug, Default)]
pub struct Bindings {
    ck_map: HashMap<String, Vec<(VarType, Option<usize>)>>,
    intrinsics: HashMap<String, (Term, TermMeta)>,
    orders: Vec<VarOrder>,
}

impl Bindings {
    fn insert_ck(&mut self, s: String, var: VarType, use_order: bool) {
        self.ck_map
            .entry(s)
            .or_default()
            .push((var, use_order.then_some(self.orders.len())));
    }
    fn remove_ck(&mut self, s: &str) {
        self.ck_map.get_mut(s).unwrap().pop();
    }
    pub fn contains(&self, s: &str) -> bool {
        self.ck_map.get(s).is_some_and(|x| !x.is_empty())
    }
    fn prim_neg_pos(&self, ty: &str, ck: &mut TypeCk) -> Option<(NegIdS, PosIdS)> {
        Some(match ty {
            "f32" => (
                ck.add_ty(Neg::Prim(NegPrim::Float { bits: 32 }), Span::default()),
                ck.add_ty(Pos::Prim(PosPrim::Float { bits: 32 }), Span::default()),
            ),
            "f64" => (
                ck.add_ty(Neg::Prim(NegPrim::Float { bits: 64 }), Span::default()),
                ck.add_ty(Pos::Prim(PosPrim::Float { bits: 64 }), Span::default()),
            ),
            "i8" | "u8" => (
                ck.add_ty(
                    Neg::Prim(NegPrim::Int {
                        signed: ty.starts_with('i'),
                        bits: 8,
                    }),
                    Span::default(),
                ),
                ck.add_ty(
                    Pos::Prim(PosPrim::Int {
                        signed: ty.starts_with('i'),
                        bits: 8,
                    }),
                    Span::default(),
                ),
            ),
            "i16" | "u16" => (
                ck.add_ty(
                    Neg::Prim(NegPrim::Int {
                        signed: ty.starts_with('i'),
                        bits: 16,
                    }),
                    Span::default(),
                ),
                ck.add_ty(
                    Pos::Prim(PosPrim::Int {
                        signed: ty.starts_with('i'),
                        bits: 16,
                    }),
                    Span::default(),
                ),
            ),
            "i32" | "u32" => (
                ck.add_ty(
                    Neg::Prim(NegPrim::Int {
                        signed: ty.starts_with('i'),
                        bits: 32,
                    }),
                    Span::default(),
                ),
                ck.add_ty(
                    Pos::Prim(PosPrim::Int {
                        signed: ty.starts_with('i'),
                        bits: 32,
                    }),
                    Span::default(),
                ),
            ),
            "i64" | "u64" => (
                ck.add_ty(
                    Neg::Prim(NegPrim::Int {
                        signed: ty.starts_with('i'),
                        bits: 64,
                    }),
                    Span::default(),
                ),
                ck.add_ty(
                    Pos::Prim(PosPrim::Int {
                        signed: ty.starts_with('i'),
                        bits: 64,
                    }),
                    Span::default(),
                ),
            ),
            "bool" => (
                ck.add_ty(Neg::Prim(NegPrim::Bool), Span::default()),
                ck.add_ty(Pos::Prim(PosPrim::Bool), Span::default()),
            ),
            _ => return None,
        })
    }
    fn get_intrinsic(
        &mut self,
        name: &str,
        ck: &mut TypeCk,
        level: u16,
    ) -> Option<(Term, TermMeta)> {
        if let Some(ret) = self.intrinsics.get(name) {
            return Some(ret.clone());
        }
        let ret = match name {
            "Equal" | "Less" | "Greater" | "f32" | "f64" | "i8" | "i16" | "i32" | "i64" | "u8"
            | "u16" | "u32" | "u64" | "bool" => {
                let id = ck.add_label(name.to_owned());
                // spans are kinda wacky but whatever
                let arg = ck.add_var(Some(format!("{name} arg")), level);
                let (arg_pos, arg_neg) = arg.polarize(ck, Span::default());
                let term = if let Some((neg, pos)) = self.prim_neg_pos(name, ck) {
                    let ret_ty = ck.add_ty(Pos::Prim(PosPrim::Label(id, pos)), Span::default());
                    Term {
                        inner: TermInner::Value(Value::Intrinsic(name.to_owned())),
                        ty: ck.add_ty(Pos::Prim(PosPrim::Lambda(neg, ret_ty)), Span::default()),
                    }
                } else {
                    let ret_ty = ck.add_ty(Pos::Prim(PosPrim::Label(id, arg_pos)), Span::default());
                    let term = Term {
                        inner: TermInner::AttachTag(
                            id,
                            Box::new(Term {
                                inner: TermInner::VarAccess(arg),
                                ty: arg_pos,
                            }),
                        ),
                        ty: ret_ty,
                    };
                    Term {
                        inner: TermInner::Value(Value::Lambda(arg, Box::new(term))),
                        ty: ck.add_ty(Pos::Prim(PosPrim::Lambda(arg_neg, ret_ty)), Span::default()),
                    }
                };
                let term = Term {
                    ty: ck.add_ty(Pos::Prim(PosPrim::Func(1, term.ty)), Span::default()),
                    inner: TermInner::Value(Value::FuncEntry(Box::new(term))),
                };
                (term, TermMeta::Type(id))
            }
            name if name.contains('_') => {
                let (func, ty) = name.split_once('_').unwrap();
                match func {
                    "rem" | "add" | "mul" | "div" | "sub" | "quot" | "pow" | "shl" | "shr"
                    | "bitxor" | "bitor" | "bitand" | "coerce" | "cmp" | "neg" => {
                        let TermMeta::Type(id) = self.get_intrinsic(ty, ck, level)?.1 else {
                            return None;
                        };

                        let (neg, pos) = self.prim_neg_pos(ty, ck)?;
                        let neg = ck.add_ty(
                            Neg::Prim(NegPrim::Label {
                                cases: {
                                    let mut cases = BTreeMap::new();
                                    cases.insert(id, (neg, false, None));
                                    cases
                                },
                                fallthrough: None,
                            }),
                            Span::default(),
                        );
                        let pos = ck.add_ty(Pos::Prim(PosPrim::Label(id, pos)), Span::default());
                        let unary = matches!(func, "coerce" | "neg");
                        let mut ty =
                            ck.add_ty(Pos::Prim(PosPrim::Lambda(neg, pos)), Span::default());
                        let argc = if unary {
                            1
                        } else {
                            ty = ck.add_ty(Pos::Prim(PosPrim::Lambda(neg, ty)), Span::default());
                            2
                        };
                        let term = Term {
                            inner: TermInner::Value(Value::Intrinsic(name.to_owned())),
                            ty,
                        };
                        let term = Term {
                            ty: ck.add_ty(Pos::Prim(PosPrim::Func(argc, term.ty)), Span::default()),
                            inner: TermInner::Value(Value::FuncEntry(Box::new(term))),
                        };
                        (term, TermMeta::None)
                    }
                    _ => return None,
                }
            }
            _ => return None,
        };
        self.intrinsics.insert(name.to_owned(), ret.clone());
        Some(ret)
    }
    pub fn contains_type<S: AsRef<str>>(&mut self, s: &[S], ck: &mut TypeCk, level: u16) -> bool {
        let (module, s) = match &s {
            [s] => (None, s),
            [a, b] => (Some(a), b),
            _ => return false,
        };
        match module.map(|x| x.as_ref()) {
            Some("intrinsic") => {
                return self
                    .get_intrinsic(s.as_ref(), ck, level)
                    .is_some_and(|x| matches!(x.1, TermMeta::Type(_)))
            }
            Some(_) => return false,
            None => {}
        }
        let Some((var, _lvl)) = self.ck_map.get(s.as_ref()).and_then(|x| x.last()) else {
            return false;
        };
        matches!(var, VarType::Type(..))
    }
    pub fn get<S: AsRef<str>>(
        &mut self,
        s: &[S],
        span: Span,
        ck: &mut TypeCk,
        level: u16,
    ) -> Result<Option<(Term, TermMeta)>, diag::Error> {
        let (module, s) = match &s {
            [s] => (None, s),
            [a, b] => (Some(a), b),
            _ => return Ok(None),
        };
        match module.map(|x| x.as_ref()) {
            Some("intrinsic") => return Ok(self.get_intrinsic(s.as_ref(), ck, level)),
            Some(_) => return Ok(None),
            None => {}
        }
        let Some((var, lvl)) = self.ck_map.get(s.as_ref()).and_then(|x| x.last()) else {
            return Ok(None);
        };
        let (var, poly, type_id) = match var {
            VarType::Mono(var) => (var, false, None),
            VarType::Poly(var) => (var, true, None),
            VarType::Type(var, id) => (var, true, Some(id)),
        };
        if let Some(lvl) = lvl {
            if !self.orders.is_empty() {
                if let Some(cur) = self.orders[*lvl].1 {
                    self.orders[*lvl].0.entry(cur).or_default().insert(*var);
                }
            }
        }
        let ty = ck.add_ty(Pos::Var(*var), span);
        let ty = if poly { ck.monomorphize(ty, level) } else { ty };

        Ok(Some((
            Term {
                inner: TermInner::VarAccess(*var),
                ty,
            },
            if let Some(id) = type_id {
                TermMeta::Type(*id)
            } else {
                TermMeta::None
            },
        )))
    }
    pub fn get_type<S: AsRef<str>>(
        &mut self,
        tag: &[S],
        span: Span,
        ck: &mut TypeCk,
        level: u16,
    ) -> Result<LabelId, diag::Error> {
        match self
            .get(tag, span, ck, level)?
            .ok_or_else(|| diag::Error::NameNotFound(NameNotFoundError::new(tag, span, false)))?
        {
            (_, TermMeta::Type(tag)) => Ok(tag),
            _ => Err(diag::Error::NameNotFound(NameNotFoundError::new(
                tag, span, true,
            ))),
        }
    }
    fn insert_pattern_let(&mut self, arms: Vec<(Ident, VarType)>) -> Vec<String> {
        let mut to_pop = Vec::new();
        // FIXME: toposort this NOW!
        // and, in fact, only allow cycles between functions
        // (if two values need each other or a value and a function need each other that can't
        // easily be lowered and more likely than not is broken)
        for (ident, var) in arms {
            to_pop.push(ident.clone());
            self.insert_ck(ident, var, true);
        }
        self.orders.push((BTreeMap::new(), None));
        to_pop
    }
    fn mark_cur(&mut self, var: VarId) {
        self.orders.last_mut().unwrap().1 = Some(var);
    }
    fn new_scope_let<T>(
        &mut self,
        arms: Vec<(Ident, VarType)>,
        func: impl FnOnce(&mut Self) -> T,
    ) -> (T, VarOrder) {
        let to_pop = self.insert_pattern_let(arms);
        let ret = func(self);
        for name in to_pop {
            self.remove_ck(&name);
        }
        let order = self.orders.pop().unwrap();
        (ret, order)
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    pub map: IndexMap<VarId, Term>,
}

impl Scope {
    fn insert(&mut self, var: VarId, term: Term) {
        self.map.insert(var, term);
    }
    fn extend(&mut self, vals: impl IntoIterator<Item = (VarId, Term)>) {
        for (k, v) in vals.into_iter() {
            self.insert(k, v);
        }
    }
}

#[derive(Default)]
pub struct Ctx {
    pub ck: TypeCk,
}

#[derive(Copy, Clone, Debug, Default)]
pub enum TermMeta {
    #[default]
    None,
    Type(LabelId),
}

fn toposort(
    vars: Vec<(VarId, Term)>,
    mut order: BTreeMap<VarId, BTreeSet<VarId>>,
) -> Vec<(VarId, Term)> {
    let mut vars: BTreeMap<VarId, Term> = vars.into_iter().collect();
    fn vis(
        var: VarId,
        vars: &mut BTreeMap<VarId, Term>,
        order: &mut BTreeMap<VarId, BTreeSet<VarId>>,
        out: &mut Vec<(VarId, Term)>,
    ) {
        let Some(term) = vars.remove(&var) else {
            return;
        };
        if let Some(order1) = order.remove(&var) {
            for var in order1 {
                vis(var, vars, order, out);
            }
        }
        out.push((var, term));
    }
    let mut ret = Vec::new();
    let all_vars = vars.keys().copied().collect::<Vec<_>>();
    for var in all_vars {
        vis(var, &mut vars, &mut order, &mut ret);
    }
    ret
}

/// A conjunction for each argument
type ConjSeq = LinkedList<ast::PatConj>;

/// Function DNF (patterns for multiple arguments and 1+ bodies)
struct ExtendedDnf<'a> {
    exprs: &'a [Expr],
    dnf: Vec<(ConjSeq, usize)>,
}

// TODO: represent refutability as an error plus a term to recover the term
// this allows better error messages
/// Returns (term, fallible, arities)
fn compile_patterns(
    ctx: &mut Ctx,
    bindings: &mut Bindings,
    level: u16,
    var: Option<(VarId, Option<LabelId>)>,
    dnf: ExtendedDnf,
) -> Result<(Term, bool, BTreeSet<usize>), diag::Error> {
    // First, check that there either are arguments left everywhere or nowhere
    let mut args_left = None;
    for (args, _) in &dnf.dnf {
        let left = !args.is_empty();
        if let Some(args_left) = args_left {
            if left != args_left {
                todo!("proper error (we have 0 and not 0 args at the same time here)")
            }
        } else {
            args_left = Some(left);
        }
    }
    // no args left
    if !args_left.unwrap_or_default() {
        if dnf.dnf.len() != 1 {
            todo!("proper error (overlapping patterns)")
        }
        let (args, expr) = dnf.dnf.into_iter().next().unwrap();
        assert!(var.is_none());
        assert!(args.is_empty());
        return Ok((
            ctx.lower_expr(bindings, [dnf.exprs[expr].clone()].into(), level)?
                .0,
            false,
            [0].into(),
        ));
    }
    let (var, label, is_top_level) = if let Some((var, label)) = var {
        (var, label, false)
    } else {
        (
            ctx.ck.add_var(Some("subpattern".to_owned()), level),
            None,
            true,
        )
    };
    // Group by first arg's first pattern
    let mut map = dnf
        .dnf
        .into_iter()
        .map(|(mut arg, expr)| (arg.front_mut().unwrap().pop_first_pat(), arg, expr))
        .into_group_map_by(|(first_arg_pat, ..)| first_arg_pat.clone())
        .into_iter()
        .map(|(first_arg_pat, elems)| {
            (
                first_arg_pat,
                elems
                    .into_iter()
                    .map(|(_first_arg, rem_args, expr)| (rem_args, expr))
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>();
    map.sort_by(|x, y| match (&x.0, &y.0) {
        // sort None after Some
        (None, Some(_)) => std::cmp::Ordering::Greater,
        (Some(_), None) => std::cmp::Ordering::Less,
        (None, None) => std::cmp::Ordering::Equal,
        (Some(a), Some(b)) => a.cmp(b),
    });
    let mut conds = Vec::<(Term, Term)>::new();
    let mut cases = BTreeMap::<LabelId, (VarId, Term, bool)>::new();
    let mut fallthrough = Vec::<Term>::new();
    let mut potentially_fallible = false;
    let mut definitely_infallible = true;
    let arbitrary_bad_span = dnf.exprs[map[0].1[0].1].span;
    let mut has_types = false;
    for (first_arg_pat, _elems) in &map {
        match first_arg_pat.as_ref().map(|x| &x.inner) {
            Some(ast::BasicPatternInner::Variable(name))
                if bindings.contains_type(&name.0, &mut ctx.ck, level) =>
            {
                has_types = true;
                break;
            }
            _ => {}
        }
    }
    let (var, mut var_fall_cache) = if has_types {
        // if we *do* use types, create a fallback var that contains all types that werent caught
        // by the match
        (var, None)
    } else {
        // if we *dont* use types, fallback var == var
        (var, Some(var))
    };
    let var_fall_cache1 = &mut var_fall_cache;
    let mut var_fall = move |ctx: &mut Ctx| {
        if let Some(ret) = *var_fall_cache1 {
            ret
        } else {
            let ret = ctx.ck.add_var(Some("fallback var".to_owned()), level);
            *var_fall_cache1 = Some(ret);
            ret
        }
    };
    let mut arities = BTreeSet::new();
    let arity_add = if is_top_level { 1 } else { 0 };
    for (first_arg_pat, elems) in map {
        let mut dnf = ExtendedDnf {
            exprs: dnf.exprs,
            dnf: elems,
        };
        match first_arg_pat.as_ref().map(|x| &x.inner) {
            Some(ast::BasicPatternInner::Variable(name))
                if name.0.len() != 1 || bindings.contains(&name.0[0]) =>
            {
                let span = first_arg_pat.as_ref().unwrap().span;
                match bindings
                    .get(&name.0, span, &mut ctx.ck, level)?
                    .ok_or_else(|| diag::NameNotFoundError::new(&name.0, span, false))?
                {
                    (term, TermMeta::None) => {
                        let (eq_func, _) = bindings
                            .get(&["=="], span, &mut ctx.ck, level)?
                            .ok_or_else(|| diag::Error::Other("== not found".to_owned(), span))?;
                        let var_fall = var_fall(ctx);
                        let var_pos = ctx.ck.add_ty(Pos::Var(var_fall), span);

                        let eq_ret1 = ctx.ck.add_var(Some("== _".to_owned()), level);
                        let (eq_ret1_pos, eq_ret1_neg) = eq_ret1.polarize(&mut ctx.ck, span);
                        let eq_neg1 = Neg::Prim(NegPrim::Lambda(var_pos, eq_ret1_neg));
                        let eq_neg1 = ctx.ck.add_ty(eq_neg1, span);
                        let eq_neg1 = Neg::Prim(NegPrim::Func(2, eq_neg1));
                        let eq_neg1 = ctx.ck.add_ty(eq_neg1, span);
                        ctx.ck.flow(eq_func.ty, eq_neg1)?;

                        let eq_ret2 = ctx.ck.add_var(Some("== _ _".to_owned()), level);
                        let (eq_ret2_pos, eq_ret2_neg) = eq_ret2.polarize(&mut ctx.ck, span);
                        let eq_neg2 = Neg::Prim(NegPrim::Lambda(term.ty, eq_ret2_neg));
                        let eq_neg2 = ctx.ck.add_ty(eq_neg2, span);
                        ctx.ck.flow(eq_ret1_pos, eq_neg2)?;

                        let cond = Term {
                            inner: TermInner::Application(
                                Box::new(Term {
                                    inner: TermInner::Application(
                                        Box::new(eq_func),
                                        Some(Box::new(Term {
                                            inner: TermInner::VarAccess(var_fall),
                                            ty: var_pos,
                                        })),
                                    ),
                                    ty: eq_ret1_pos,
                                }),
                                Some(Box::new(term)),
                            ),
                            ty: eq_ret2_pos,
                        };
                        let bool_neg = ctx.ck.add_ty(Neg::Prim(NegPrim::Bool), span);
                        ctx.ck.flow(cond.ty, bool_neg)?;

                        let (term, _fallible, arities1) =
                            compile_patterns(ctx, bindings, level, Some((var_fall, label)), dnf)?;
                        for arity in arities1 {
                            arities.insert(arity + arity_add);
                        }
                        potentially_fallible = true;
                        conds.push((cond, term));
                    }
                    (_, TermMeta::Type(label1)) => {
                        if label.is_some() {
                            todo!("proper error (cant have 2 labels in a conjunction)");
                        }
                        let var_case = ctx
                            .ck
                            .add_var(Some(format!("case {name} ({arities:?})")), level);
                        let (term, fallible, arities1) = compile_patterns(
                            ctx,
                            bindings,
                            level,
                            Some((var_case, Some(label1))),
                            dnf,
                        )?;
                        for arity in arities1 {
                            arities.insert(arity + arity_add);
                        }
                        let term = Term {
                            ty: term.ty,
                            inner: TermInner::Bind {
                                bindings: Box::new([(
                                    var_case,
                                    Term {
                                        inner: TermInner::VarAccess(var),
                                        ty: ctx.ck.add_ty(Pos::Var(var), span),
                                    },
                                )]),
                                expr: Box::new(term),
                            },
                        };
                        potentially_fallible |= fallible;
                        cases.insert(label1, (var_case, term, fallible));
                    }
                }
            }
            Some(ast::BasicPatternInner::Variable(name)) => {
                assert_eq!(name.0.len(), 1);
                let name = name.into_iter().next().unwrap();
                if name.starts_with(|x: char| x.is_uppercase()) {
                    // TODO: warn that this is probably meant to be a type
                    println!("this is meant to be a type right");
                }
                let var_fall = var_fall(ctx);
                bindings.insert_ck(name.clone(), VarType::Mono(var_fall), false);
                let (term, fallible, arities1) =
                    compile_patterns(ctx, bindings, level, Some((var_fall, label)), dnf)?;
                for arity in arities1 {
                    arities.insert(arity + arity_add);
                }
                bindings.remove_ck(name);
                fallthrough.push(term);
                if fallible {
                    potentially_fallible = true;
                } else {
                    definitely_infallible = false;
                }
            }
            None => {
                for (conj, _expr) in &mut dnf.dnf {
                    conj.pop_front().unwrap();
                }
                let (term, fallible, arities1) = compile_patterns(ctx, bindings, level, None, dnf)?;
                for arity in arities1 {
                    arities.insert(arity + arity_add);
                }
                fallthrough.push(term);
                if fallible {
                    potentially_fallible = true;
                } else {
                    definitely_infallible = false;
                }
            }
        }
    }
    let mut seq = vec![];
    let mut ret_fall_cache = None;
    let ret_fall_cache1 = &mut ret_fall_cache;
    let mut ret_fall = move |ctx: &mut Ctx| {
        if let Some(ret) = *ret_fall_cache1 {
            ret
        } else {
            let var = ctx.ck.add_var(
                Some(if has_types { "ret fallback" } else { "ret" }.to_owned()),
                level,
            );
            let (pos, neg) = var.polarize(&mut ctx.ck, arbitrary_bad_span);
            let ret = (var, pos, neg);
            *ret_fall_cache1 = Some(ret);
            ret
        }
    };
    let mut cond_term = None;
    for (cond, term) in conds.into_iter().rev() {
        let (_, ret_fall_pos, ret_fall_neg) = ret_fall(ctx);
        ctx.ck.flow(term.ty, ret_fall_neg)?;
        let cond_term1 = cond_term.unwrap_or_else(|| Term {
            inner: TermInner::Fallthrough,
            ty: ret_fall(ctx).1,
        });
        cond_term = Some(Term {
            inner: TermInner::If(Box::new(cond), Box::new(term), Box::new(cond_term1)),
            ty: ret_fall_pos,
        });
    }
    seq.extend(cond_term);
    for term in &fallthrough {
        let (_, _, ret_fall_neg) = ret_fall(ctx);
        ctx.ck.flow(term.ty, ret_fall_neg)?;
    }
    seq.extend(fallthrough);
    let fallible = potentially_fallible && !definitely_infallible;
    let term = (!seq.is_empty()).then(|| {
        let (_, ret_fall_pos, _) = ret_fall(ctx);
        let term = Term {
            inner: TermInner::Sequence(seq),
            ty: ret_fall_pos,
        };
        if var == var_fall(ctx) {
            term
        } else {
            Term {
                inner: TermInner::Bind {
                    bindings: Box::new([(
                        var_fall(ctx),
                        Term {
                            inner: TermInner::VarAccess(var),
                            ty: ctx.ck.add_ty(Pos::Var(var), arbitrary_bad_span),
                        },
                    )]),
                    expr: Box::new(term),
                },
                ty: ret_fall_pos,
            }
        }
    });
    let term = if has_types {
        let ret_ty = ctx.ck.add_var(Some("ret".to_owned()), level);
        let ret_neg = ctx.ck.add_ty(Neg::Var(ret_ty), arbitrary_bad_span);
        let mut neg = BTreeMap::new();
        let mut branches = BTreeMap::new();
        for (label, (var_case, term, fallible)) in cases {
            neg.insert(
                label,
                (
                    ctx.ck.add_ty(Neg::Var(var_case), arbitrary_bad_span),
                    fallible,
                    Some((term.ty, ret_neg)),
                ),
            );
            branches.insert(label, term);
        }
        let neg = Neg::Prim(NegPrim::Label {
            cases: neg,
            fallthrough: term.as_ref().map(|_| {
                let fall = var_fall(ctx);
                let (_, ret_fall_pos, _) = ret_fall(ctx);
                (
                    ctx.ck.add_ty(Neg::Var(fall), arbitrary_bad_span),
                    Some((ret_fall_pos, ret_neg)),
                )
            }),
        });
        let neg = ctx.ck.add_ty(neg, arbitrary_bad_span);
        let pos = ctx.ck.add_ty(Pos::Var(var), arbitrary_bad_span);
        ctx.ck.flow(pos, neg)?;
        let ret_pos = ctx.ck.add_ty(Pos::Var(ret_ty), arbitrary_bad_span);
        Term {
            inner: TermInner::CheckTag {
                var,
                branches,
                fallthrough: term.map(Box::new),
            },
            ty: ret_pos,
        }
    } else {
        term.unwrap()
    };
    let term = if is_top_level {
        let neg = ctx.ck.add_ty(Neg::Var(var), arbitrary_bad_span);
        let ty = ctx
            .ck
            .add_ty(Pos::Prim(PosPrim::Lambda(neg, term.ty)), arbitrary_bad_span);
        Term {
            inner: TermInner::Value(Value::Lambda(var, Box::new(term))),
            ty,
        }
    } else {
        term
    };
    Ok((term, fallible, arities))
}

impl Ctx {
    fn alloc_var(
        &mut self,
        ident: String,
        defs: Vec<LetArm>,
        level: u16,
    ) -> Result<
        (
            (bool, Ident, VarId, PosIdS),
            (VarId, NegIdS, SmallVec<[Expr; 1]>),
        ),
        diag::VarRedefinitionError,
    > {
        assert!(!defs.is_empty());
        match defs[0].pattern.inner {
            LetPatternInner::Val => {
                let mut defs = defs.into_iter();
                let def = defs.next().unwrap();
                if let Some(def1) = defs.next() {
                    return Err(diag::VarRedefinitionError::new(
                        ident,
                        [def.pattern.span, def1.pattern.span]
                            .into_iter()
                            .chain(defs.map(|x| x.pattern.span))
                            .collect(),
                    ));
                }
                let is_type = matches!(def.body.inner, ast::ExprInner::TypeConstructor(_));
                let var = self.ck.add_var(Some(ident.clone()), level + 1);
                let (var_pos, var_neg) = var.polarize(&mut self.ck, def.pattern.span);
                Ok((
                    (is_type, ident.clone(), var, var_pos),
                    (var, var_neg, [def.body].into()),
                ))
            }
            LetPatternInner::Func(_) => {
                for def1 in &defs {
                    if let LetPatternInner::Val = def1.pattern.inner {
                        return Err(diag::VarRedefinitionError::new(
                            ident,
                            [defs[0].pattern.span, def1.pattern.span]
                                .into_iter()
                                .collect(),
                        ));
                    }
                }
                let var = self.ck.add_var(Some(ident.clone()), level + 1);
                let (var_pos, var_neg) = var.polarize(&mut self.ck, defs[0].pattern.span);
                let mut defs = defs;
                defs.sort_by(|x, y| {
                    match x
                        .pattern
                        .as_func()
                        .unwrap()
                        .len()
                        .cmp(&y.pattern.as_func().unwrap().len())
                    {
                        std::cmp::Ordering::Equal => x.pattern.cmp(&y.pattern),
                        x => x,
                    }
                });
                defs.sort_by_key(|x| x.pattern.as_func().unwrap().len());
                Ok((
                    (false, ident.clone(), var, var_pos),
                    (
                        var,
                        var_neg,
                        defs.into_iter()
                            .map(|x| {
                                let LetArm {
                                    pattern,
                                    body,
                                    span,
                                } = x;
                                let pattern = match pattern.inner {
                                    LetPatternInner::Func(x) => x,
                                    _ => unreachable!(),
                                };
                                Expr::new(ast::ExprInner::Lambda(pattern, Box::new(body)), span)
                            })
                            .collect(),
                    ),
                ))
            }
        }
    }
    // There may be multiple exprs if this expr is defined in multiple places
    // This is currently only a thing for functions
    fn lower_expr(
        &mut self,
        bindings: &mut Bindings,
        exprs: SmallVec<[ast::Expr; 1]>,
        level: u16,
    ) -> Result<(Term, TermMeta), diag::Error> {
        let mut exprs = exprs.into_iter();
        let expr = exprs.next().unwrap();
        (match expr.inner {
            ast::ExprInner::LetRec(arms, body) => {
                assert!(exprs.next().is_none());
                // first, allocate the variable that is to be assigned <expr1>
                let (mut pats, exprs) = arms
                    .into_iter()
                    .map(|(ident, pat_exprs)| self.alloc_var(ident, pat_exprs, level))
                    .collect::<Result<(Vec<_>, Vec<_>), _>>()?;
                pats.sort_by_key(|(x, ..)| !*x);
                // now create a new scope with that var and lower the expr in that scope
                let (res, order) = bindings.new_scope_let(
                    pats.into_iter()
                        .map(|(is_type, ident, var, _var_pos)| {
                            let t = if is_type {
                                VarType::Type(var, self.ck.add_label(ident.clone()))
                            } else {
                                VarType::Poly(var)
                            };
                            (ident, t)
                        })
                        .collect(),
                    |bindings| {
                        let mut vars = Vec::new();
                        for (var, var_neg, expr1) in exprs {
                            bindings.mark_cur(var);
                            let expr1 = self.lower_expr(bindings, expr1, level + 1)?.0;
                            // now direct expr1 to var_neg (assignment)
                            self.ck.flow(expr1.ty, var_neg)?;
                            // and actually bind this variable
                            vars.push((var, expr1));
                        }
                        // finally, lower expr2 in the new scope
                        let expr = self.lower_expr(bindings, [*body].into(), level)?.0;
                        Ok::<_, diag::Error>((expr, vars))
                    },
                );
                let (expr, vars) = res?;
                let vars = toposort(vars, order.0);
                Ok(Term {
                    ty: expr.ty,
                    inner: TermInner::Bind {
                        bindings: vars.into(),
                        expr: Box::new(expr),
                    },
                })
            }
            ast::ExprInner::Lambda(..) => {
                let arbitrary_bad_span = expr.span;
                let mut exprs1 = vec![];
                let mut dnf = vec![];
                for (pats, expr) in [expr]
                    .into_iter()
                    .chain(exprs)
                    .map(|expr| expr.into_lambda().unwrap())
                {
                    exprs1.push(expr);
                    let expr = exprs1.len() - 1;
                    for pat_seq in pats
                        .into_iter()
                        .map(|x| x.into_dnf().0.into_iter().map(|x| x.0).collect::<Vec<_>>())
                        .multi_cartesian_product()
                    {
                        dnf.push((pat_seq.into_iter().map(ast::PatConj).collect(), expr));
                    }
                }
                // println!("dnf {dnf:?}");
                let dnf = ExtendedDnf {
                    exprs: &exprs1,
                    dnf,
                };
                let (term, refutable, arities) =
                    compile_patterns(self, bindings, level, None, dnf)?;
                if refutable {
                    todo!("proper error: refutability oh no ub something something")
                }
                if arities.len() != 1 {
                    todo!("proper error: not exactly one arity");
                }
                let argc = arities.into_iter().next().unwrap();
                let term = Term {
                    ty: self
                        .ck
                        .add_ty(Pos::Prim(PosPrim::Func(argc, term.ty)), arbitrary_bad_span),
                    inner: TermInner::Value(Value::FuncEntry(Box::new(term))),
                };
                Ok(term)
            }
            ast::ExprInner::TypeConstructor(ident) => {
                assert!(exprs.next().is_none());
                // spans are kinda wacky but whatever
                let id = bindings.get_type(&[ident.as_str()], expr.span, &mut self.ck, level)?;
                let arg = self.ck.add_var(Some(format!("{ident} arg")), level);
                let (arg_pos, arg_neg) = arg.polarize(&mut self.ck, expr.span);
                let ret_pos = self
                    .ck
                    .add_ty(Pos::Prim(PosPrim::Label(id, arg_pos)), expr.span);
                let term = Term {
                    inner: TermInner::AttachTag(
                        id,
                        Box::new(Term {
                            inner: TermInner::VarAccess(arg),
                            ty: arg_pos,
                        }),
                    ),
                    ty: ret_pos,
                };
                let term = Term {
                    inner: TermInner::Value(Value::Lambda(arg, Box::new(term))),
                    ty: self
                        .ck
                        .add_ty(Pos::Prim(PosPrim::Lambda(arg_neg, ret_pos)), expr.span),
                };
                let term = Term {
                    ty: self
                        .ck
                        .add_ty(Pos::Prim(PosPrim::Func(1, term.ty)), expr.span),
                    inner: TermInner::Value(Value::FuncEntry(Box::new(term))),
                };
                return Ok((term, TermMeta::Type(id)));
            }
            ast::ExprInner::Call(func, args) => {
                assert!(exprs.next().is_none());
                let func_span = func.span;
                let (func, func_meta) = self.lower_expr(bindings, [*func].into(), level)?;
                let args = args
                    .into_iter()
                    .map(|arg| Ok(self.lower_expr(bindings, [arg].into(), level)?.0))
                    .collect::<Result<Vec<_>, diag::Error>>()?;
                match func_meta {
                    TermMeta::Type(id) if args.len() == 1 => {
                        let arg = args.into_iter().next().unwrap();
                        let ty = self
                            .ck
                            .add_ty(Pos::Prim(PosPrim::Label(id, arg.ty)), expr.span);
                        Ok(Term {
                            inner: TermInner::AttachTag(id, Box::new(arg)),
                            ty,
                        })
                    }
                    TermMeta::None | TermMeta::Type(_) => {
                        let mut func = func;
                        let argc = args.len();
                        if argc == 0 {
                            func = Term {
                                ty: {
                                    let ret = self.ck.add_var(Some("ret".to_owned()), level);
                                    let (ret_pos, ret_neg) = ret.polarize(&mut self.ck, expr.span);
                                    let func_neg = Neg::Prim(NegPrim::Func(argc, ret_neg));
                                    let func_neg = self.ck.add_ty(func_neg, func_span);
                                    self.ck.flow(func.ty, func_neg)?;
                                    ret_pos
                                },
                                inner: TermInner::Application(Box::new(func), None),
                            };
                        }
                        for (i, arg) in args.into_iter().enumerate() {
                            let ret = self.ck.add_var(Some(format!("ret {i}")), level);
                            let (ret_pos, ret_neg) = ret.polarize(&mut self.ck, expr.span);
                            let func_neg = Neg::Prim(NegPrim::Lambda(arg.ty, ret_neg));
                            let func_neg = self.ck.add_ty(func_neg, func_span);
                            let func_neg = if i == 0 {
                                let func_neg = Neg::Prim(NegPrim::Func(argc, func_neg));
                                self.ck.add_ty(func_neg, func_span)
                            } else {
                                func_neg
                            };
                            self.ck.flow(func.ty, func_neg)?;
                            func = Term {
                                inner: TermInner::Application(Box::new(func), Some(Box::new(arg))),
                                ty: ret_pos,
                            };
                        }
                        Ok(func)
                    }
                }
            }
            ast::ExprInner::BinOp(op, _a, _b) => {
                assert!(exprs.next().is_none());
                match op {
                    // no adhoc polymorphism means no math, frick you
                    _ => panic!(),
                }
            }
            ast::ExprInner::UnOp(..) => {
                assert!(exprs.next().is_none());
                todo!()
            }
            ast::ExprInner::Literal(kind, value) => {
                assert!(exprs.next().is_none());
                (match kind {
                    ast::LiteralKind::Bool => match value.as_str() {
                        "false" => Ok((Value::Bool(false), Pos::Prim(PosPrim::Bool))),
                        "true" => Ok((Value::Bool(true), Pos::Prim(PosPrim::Bool))),
                        x => Err(diag::Error::Other(
                            format!("unexpected boolean literal: {x}"),
                            expr.span,
                        )),
                    },
                    ast::LiteralKind::Int => match malachite_nz::integer::Integer::from_str(&value)
                    {
                        Ok(val) => {
                            let Ok(bits) = u8::try_from({
                                let mut last_limb = None;
                                let mut bits = 0u64;
                                for limb in val.twos_complement_limbs() {
                                    bits += std::mem::size_of_val(&limb) as u64 * 8;
                                    last_limb = Some(limb);
                                }
                                if let Some(limb) = last_limb {
                                    bits -= std::mem::size_of_val(&limb) as u64 * 8;
                                    bits += limb.significant_bits();
                                }
                                bits
                            }) else {
                                return Err(diag::Error::Other(
                                    "invalid integer literal".to_owned(),
                                    expr.span,
                                ));
                            };
                            let signed = val < 0;
                            Ok((
                                Value::Int(val),
                                Pos::Prim(PosPrim::IntLiteral { signed, bits }),
                            ))
                        }
                        Err(()) => Err(diag::Error::Other(
                            "invalid integer literal".to_owned(),
                            expr.span,
                        )),
                    },
                    ast::LiteralKind::Float => match value.parse::<f64>() {
                        Ok(val) => Ok((Value::Float(val), Pos::Prim(PosPrim::FloatLiteral))),
                        Err(err) => Err(diag::Error::Other(
                            format!("invalid float literal: {err}"),
                            expr.span,
                        )),
                    },
                })
                .map(|(val, ty)| Term {
                    ty: self.ck.add_ty(ty, expr.span),
                    inner: TermInner::Value(val),
                })
            }
            ast::ExprInner::Variable(var) => {
                assert!(exprs.next().is_none());
                let (term, meta) = bindings
                    .get(&var.0, expr.span, &mut self.ck, level)?
                    .ok_or_else(|| diag::NameNotFoundError::new(&var.0, expr.span, false))?;
                return Ok((term, meta));
            }
        })
        .map(|ret| (ret, TermMeta::None))
    }

    pub fn lower_ast(&mut self, ast: ast::Program) -> Result<(Bindings, Scope), diag::Error> {
        let mut bindings = Bindings::default();
        let level = 0;
        let (mut pats, exprs) = ast
            .into_iter()
            .map(|(ident, pat_exprs)| self.alloc_var(ident, pat_exprs, level))
            .collect::<Result<(Vec<_>, Vec<_>), _>>()?;
        // now create a new scope with that var and lower the expr in that scope
        pats.sort_by_key(|(x, ..)| !*x);
        // now create a new scope with that var and lower the expr in that scope
        bindings.insert_pattern_let(
            pats.into_iter()
                .map(|(is_type, ident, var, _var_pos)| {
                    let t = if is_type {
                        VarType::Type(var, self.ck.add_label(ident.clone()))
                    } else {
                        VarType::Poly(var)
                    };
                    (ident, t)
                })
                .collect(),
        );
        let mut vars = Vec::new();
        for (var, var_neg, expr1) in exprs {
            bindings.mark_cur(var);
            let expr1 = self.lower_expr(&mut bindings, expr1, level + 1)?.0;
            self.ck.flow(expr1.ty, var_neg)?;
            vars.push((var, expr1));
        }
        let order = bindings.orders.pop().unwrap();
        let vars = toposort(vars, order.0);
        let mut scope = Scope::default();
        scope.extend(vars);
        Ok((bindings, scope))
    }
}
