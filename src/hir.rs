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
    Lambda(BTreeMap<usize, (Vec<VarId>, Term)>),
    Bool(bool),
    Float(f64),
    Int(malachite_nz::integer::Integer),
    Intrinsic(Intrinsic),
}

#[derive(Copy, Clone, Debug)]
pub enum VarType {
    Mono(VarId),
    TypeConstructor(VarId, LabelId),
    Poly(VarId),
}

#[derive(Clone, Debug)]
pub enum TermInner {
    Application(Box<Term>, Vec<Term>),
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

#[derive(Copy, Clone, Debug)]
pub enum Intrinsic {
    UnaryNot,
    BinaryPow,
    BinaryMul,
    BinaryDiv,
    BinaryFloorDiv,
    BinaryRem,
    BinaryAdd,
    BinarySub,
    BinaryShl,
    BinaryShr,
    BinaryBitAnd,
    BinaryBitXor,
    BinaryBitOr,
    BinaryEq,
    BinaryNe,
    BinaryLt,
    BinaryGt,
    BinaryLe,
    BinaryGe,
    BinaryAnd,
    BinaryOr,
}

#[derive(Clone, Debug)]
pub struct Term {
    pub inner: TermInner,
    pub ty: PosIdS,
}

type VarOrder = (BTreeMap<VarId, BTreeSet<VarId>>, Option<VarId>);

#[derive(Clone, Default)]
pub struct Bindings {
    ck_map: HashMap<String, Vec<(VarType, Option<usize>)>>,
    orders: Vec<VarOrder>,
}

#[derive(Clone, Debug)]
struct Binding {
    var: VarId,
    term: Term,
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
    pub fn get(
        &mut self,
        s: &str,
        span: Span,
        ck: &mut TypeCk,
        level: u16,
    ) -> Result<Option<(VarId, PosIdS, TermMeta)>, diag::Error> {
        let Some((var, lvl)) = self.ck_map.get(s).and_then(|x| x.last()) else {
            return Ok(None);
        };
        let (var, poly, type_id) = match var {
            VarType::Mono(var) => (var, false, None),
            VarType::Poly(var) => (var, true, None),
            VarType::TypeConstructor(var, id) => (var, true, Some(id)),
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
            *var,
            ty,
            if let Some(id) = type_id {
                TermMeta::Type(*id)
            } else {
                TermMeta::None
            },
        )))
    }
    pub fn get_type(
        &mut self,
        tag: &str,
        span: Span,
        ck: &mut TypeCk,
        level: u16,
    ) -> Result<LabelId, diag::Error> {
        match self
            .get(tag, span, ck, level)?
            .ok_or_else(|| diag::Error::NameNotFound(NameNotFoundError::new(tag, span, false)))?
        {
            (_, _, TermMeta::Type(tag)) => Ok(tag),
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
    /// Execute `func` in a new scope.
    ///
    /// # Parameters
    ///
    /// - `pat` - the pattern to bind
    /// - `var` - the variable to take the pattern's values from
    /// - `var_out` - where the variable's value comes from
    /// - `func` - the function to execute
    ///
    /// `func` additionally takes `Vec<(VarId, Term)>`, which contains the list of variables bound
    /// by the pattern (if any)
    fn new_scope<T>(
        &mut self,
        (vars, to_pop): (Vec<(VarId, Term)>, Vec<String>),
        func: impl FnOnce(&mut Self, Vec<(VarId, Term)>) -> T,
    ) -> T {
        let ret = func(self, vars);
        for name in to_pop {
            self.remove_ck(&name);
        }
        ret
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
                let (var_out, var_inp) = var.polarize(&mut self.ck, def.pattern.span);
                Ok((
                    (is_type, ident.clone(), var, var_out),
                    (var, var_inp, [def.body].into()),
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
                let (var_out, var_inp) = var.polarize(&mut self.ck, defs[0].pattern.span);
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
                    (false, ident.clone(), var, var_out),
                    (
                        var,
                        var_inp,
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
                        .map(|(is_type, ident, var, _var_out)| {
                            let t = if is_type {
                                VarType::TypeConstructor(var, self.ck.add_label(ident.clone()))
                            } else {
                                VarType::Poly(var)
                            };
                            (ident, t)
                        })
                        .collect(),
                    |bindings| {
                        let mut vars = Vec::new();
                        for (var, var_inp, expr1) in exprs {
                            bindings.mark_cur(var);
                            let expr1 = self.lower_expr(bindings, expr1, level + 1)?.0;
                            // now direct expr1 to var_inp (assignment)
                            self.ck.flow(expr1.ty, var_inp)?;
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
                let span = expr.span;
                let exprs = [expr].into_iter().chain(exprs);
                let (tys, exprs) = exprs
                    .chunk_by(|x| x.as_lambda().unwrap().0.len())
                    .into_iter()
                    .map(|(len, exprs)| {
                        let exprs: Vec<_> = exprs
                            .map(|expr| {
                                let (pat, expr) = expr.into_lambda().unwrap();
                                // this is too depressing to work with without linked lists
                                // sure, i can pass vectors and numbers around, but thats super annoying
                                (LinkedList::from_iter(pat), expr)
                            })
                            .collect();
                        // each subpattern's lowered version consists of 3 parts:
                        // 1. bind the subpattern variable if not already bound
                        // 2. check the variable tag if necessary
                        // 3. check the refutable conditions associated with that variable or prior
                        //    variables
                        // for example:
                        // f (A { x; y = 0; }) => 5;
                        // f (A { x = 0; y = B w; }) => 6;
                        // f (B { }) => 7;
                        // should compile to
                        // f =
                        //  arg: A { x : int; y : int | B _; } = arg0
                        //  match (tag arg)
                        //   A =>
                        //    x: int = arg.x;
                        //    if x == 0
                        //      y: B _ = arg.y;
                        //      match (tag y)
                        //       B =>
                        //        return 6;
                        //    y: int = arg.y;
                        //    if y == 0
                        //      return 5;
                        //   B =>
                        //    return y;
                        //
                        // when it comes to sorting checks, it's best to start with common
                        // subpatterns
                        // for example, if there's `A { x, y }` and `A { y, z }`, then the order
                        // y, x, z makes sense
                        // but also in case of (A, B, C) and (A, A, C) it's too annoying to check 1
                        // and 3 before 2, so who cares

                        // suboptimal implementation because i'm tired
                        // compile using classic recursive descent

                        // TODO: represent refutability as an error plus a term to recover the term
                        // this allows better error messages
                        /// Func 3: base case (tags and conditions must match)
                        fn compile_patterns_3(
                            ctx: &mut Ctx,
                            bindings: &mut Bindings,
                            level: u16,
                            vars: &mut LinkedList<(VarId, PosIdS)>,
                            cases: Vec<(LinkedList<ast::Pattern>, Expr)>,
                        ) -> Result<(Term, bool), diag::Error> {
                            let mut map = cases
                                .into_iter()
                                .map(|(mut x, y)| (x.pop_front(), x, y))
                                .into_group_map_by(|(x, ..)| x.clone())
                                .into_iter()
                                .map(|x| {
                                    (
                                        x.0,
                                        x.1.into_iter().map(|(_, x, y)| (x, y)).collect::<Vec<_>>(),
                                    )
                                })
                                .collect::<Vec<_>>();
                            map.sort_by(|x, y| match (&x.0, &y.0) {
                                // sort None after Some
                                // (i.e. no-label cases are to be processed last)
                                (None, Some(_)) => std::cmp::Ordering::Greater,
                                (Some(_), None) => std::cmp::Ordering::Less,
                                (None, None) => std::cmp::Ordering::Equal,
                                (Some(a), Some(b)) => a.cmp(b),
                            });
                            let out = ctx.ck.add_var(None, level);
                            let arbitrary_bad_span = map[0].1[0].1.span;
                            let mut out_pos = None;
                            let mut ret = vec![];
                            let mut refutable = true;
                            for (pat, cases) in map {
                                let (out_pos1, out_neg) =
                                    out.polarize(&mut ctx.ck, arbitrary_bad_span);
                                if out_pos.is_none() {
                                    out_pos = Some(out_pos1);
                                }
                                match pat.as_ref().map(|x| &x.inner) {
                                    Some(ast::PatternInner::Tag(..)) => unreachable!(),
                                    // TODO: check that there's no patterns after irrefutable
                                    // patterns?
                                    Some(ast::PatternInner::Variable(x)) => {
                                        let span = pat.as_ref().unwrap().span;
                                        let (var, var_pos) = vars.pop_front().unwrap();
                                        let arg = ctx.ck.add_var(Some(x.clone()), level);
                                        bindings.insert_ck(x.clone(), VarType::Mono(arg), false);
                                        let (rest, refutable1) = compile_patterns_1(ctx, bindings, level, vars, cases)?;
                                        bindings.remove_ck(x);
                                        let arg_inp = ctx.ck.add_ty(Neg::Var(arg), span);
                                        ctx.ck.flow(var_pos, arg_inp)?;
                                        let rest = Term {
                                            ty: rest.ty,
                                            inner: TermInner::Bind {
                                                bindings: Box::new([(arg, Term {
                                                    inner: TermInner::VarAccess(var),
                                                    ty: var_pos,
                                                })]),
                                                expr: Box::new(rest),
                                            },
                                        };
                                        if !refutable1 {
                                            refutable = false;
                                        }
                                        ctx.ck.flow(rest.ty, out_neg)?;
                                        ret.push(rest);
                                    }
                                    None => {
                                        refutable = false;
                                        let rest = ctx.lower_expr(bindings, [cases.into_iter().next().unwrap().1].into(), level)?.0;
                                        ctx.ck.flow(rest.ty, out_neg)?;
                                        ret.push(rest);
                                    }
                                }
                            }
                            if ret.len() == 1 {
                                return Ok((ret.into_iter().next().unwrap(), refutable));
                            }
                            Ok((Term {
                                inner: TermInner::Sequence(ret),
                                ty: out_pos.unwrap(),
                            }, refutable))
                        }
                        /// Func 2: separate by conditions (tags must match)
                        fn compile_patterns_2(
                            ctx: &mut Ctx,
                            bindings: &mut Bindings,
                            level: u16,
                            vars: &mut LinkedList<(VarId, PosIdS)>,
                            cases: Vec<(LinkedList<ast::Pattern>, Expr)>,
                        ) -> Result<(Term, bool), diag::Error> {
                            let mut map = cases
                                .into_iter()
                                .map(|(mut x, y)| (x.pop_front(), x, y))
                                .into_group_map_by(|(x, ..)| x.clone())
                                .into_iter()
                                .map(|x| {
                                    (
                                        x.0,
                                        x.1.into_iter().map(|(_, x, y)| (x, y)).collect::<Vec<_>>(),
                                    )
                                })
                                .collect::<Vec<_>>();
                            map.sort_by(|x, y| match (&x.0, &y.0) {
                                // sort None after Some
                                // (i.e. no-label cases are to be processed last)
                                (None, Some(_)) => std::cmp::Ordering::Greater,
                                (Some(_), None) => std::cmp::Ordering::Less,
                                (None, None) => std::cmp::Ordering::Equal,
                                (Some(a), Some(b)) => a.cmp(b),
                            });
                            // we don't have to check the tag here
                            let branches = vec![];
                            let mut unconditional = None;
                            let out = ctx.ck.add_var(None, level);
                            let arbitrary_bad_span = map[0].1[0].1.span;
                            let mut out_pos = None;
                            let mut refutable = true;
                            for (pat, mut cases) in map {
                                let (out_pos1, out_neg) =
                                    out.polarize(&mut ctx.ck, arbitrary_bad_span);
                                if out_pos.is_none() {
                                    out_pos = Some(out_pos1);
                                }
                                match pat.as_ref().map(|x| &x.inner) {
                                    Some(ast::PatternInner::Tag(..)) => unreachable!(),
                                    Some(ast::PatternInner::Variable(x))
                                        if bindings.contains(x) =>
                                    {
                                        let span = pat.as_ref().unwrap().span;
                                        let (_var, _var_pos, meta) = bindings
                                            .get(x, span, &mut ctx.ck, level)?
                                            .unwrap();
                                        match meta {
                                            TermMeta::Type(_) => todo!("TODO proper error, do Type _ instead of just Type, or maybe I should handle this in the previous case that deals with tags"),
                                            TermMeta::None => {}
                                        }
                                        todo!("compile to == condition")
                                    }
                                    /*let (rest, _refutable) = compile_patterns_3(
                                        ctx, bindings, level, var, var_out, cases,
                                    );
                                    refutable = true;
                                    branches.push(rest);*/
                                    None | Some(ast::PatternInner::Variable(_)) => {
                                        if let Some(pat) = pat {
                                            for case in &mut cases {
                                                case.0.push_front(pat.clone());
                                            }
                                        }
                                        let (rest, refutable1) = compile_patterns_3(
                                            ctx, bindings, level, vars, cases,
                                        )?;
                                        refutable = refutable1;
                                        ctx.ck.flow(rest.ty, out_neg)?;
                                        // TODO: error if two unconditional patterns match
                                        unconditional = Some(rest);
                                    }
                                }
                            }
                            let mut term = unconditional.unwrap_or_else(|| Term {
                                inner: TermInner::Fallthrough,
                                ty: out_pos.unwrap(),
                            });
                            for (cond, body) in branches {
                                term = Term {
                                    inner: TermInner::If(
                                        Box::new(cond),
                                        Box::new(body),
                                        Box::new(term),
                                    ),
                                    ty: out_pos.unwrap(),
                                }
                            }
                            Ok((term, refutable))
                        }
                        /// Func 1: separate by tag
                        fn compile_patterns_1(
                            ctx: &mut Ctx,
                            bindings: &mut Bindings,
                            level: u16,
                            vars: &mut LinkedList<(VarId, PosIdS)>,
                            cases: Vec<(LinkedList<ast::Pattern>, Expr)>,
                        ) -> Result<(Term, bool), diag::Error> {
                            // sort by resolved tag
                            let cases = cases
                                .into_iter()
                                .map(|(mut x, y)| match x.pop_front() {
                                    Some(x1) => match x1.inner {
                                        ast::PatternInner::Tag(tag, span, x1) => Ok((
                                            Some((
                                                bindings.get_type(
                                                    &tag,
                                                    span,
                                                    &mut ctx.ck,
                                                    level,
                                                )?,
                                                span,
                                            )),
                                            {
                                                x.push_front(*x1);
                                                x
                                            },
                                            y,
                                        )),
                                        ast::PatternInner::Variable(_) => {
                                            x.push_front(x1);
                                            Ok((None, x, y))
                                        }
                                    },
                                    None => Ok((None, x, y)),
                                })
                                .collect::<Result<Vec<_>, diag::Error>>()?;
                            let mut map = cases
                                .into_iter()
                                .into_group_map_by(|(t, ..)| t.map(|x| x.0))
                                .into_iter()
                                .map(|x| {
                                    (
                                        x.1[0].0,
                                        x.1.into_iter().map(|(_, x, y)| (x, y)).collect::<Vec<_>>(),
                                    )
                                })
                                .collect::<Vec<_>>();
                            map.sort_by(|x, y| match (&x.0.map(|x| x.0), &y.0.map(|y| y.0)) {
                                // sort None after Some
                                // (i.e. no-label cases are to be processed last)
                                (None, Some(_)) => std::cmp::Ordering::Greater,
                                (Some(_), None) => std::cmp::Ordering::Less,
                                (None, None) => std::cmp::Ordering::Equal,
                                (Some(a), Some(b)) => a.cmp(b),
                            });
                            let mut tag_map = BTreeMap::new();
                            let mut fallthrough = None;
                            let mut ty_tag_map = BTreeMap::new();
                            let mut ty_fallthrough = None;
                            let mut refutable = false;
                            let out = ctx.ck.add_var(None, level);
                            let mut out_pos = None;
                            // TODO: multiple and better spans (these spans frankly speaking make no sense)
                            let arbitrary_bad_span = map[0].1[0].1.span;
                            let var = vars.pop_front();
                            for (tag, cases) in map {
                                // if tag map is empty and we're at tag None, don't rewrite the var
                                let var1 = if let Some((var, var_out)) = var {
                                    if tag.is_some() || !tag_map.is_empty() {
                                        let var1 = ctx.ck.add_var(None, level);
                                        let (var1_out, var1_inp) =
                                            var1.polarize(&mut ctx.ck, arbitrary_bad_span);
                                        vars.push_front((var1, var1_out));
                                        Some((var1, var1_inp))
                                    } else {
                                        vars.push_front((var, var_out));
                                        None
                                    }
                                } else {
                                    None
                                };
                                let (rest, refutable1) = compile_patterns_2(
                                    ctx, bindings, level, vars, cases,
                                )?;
                                if var1.is_some() {
                                    vars.pop_front();
                                }
                                let (out_pos1, out_neg) =
                                    out.polarize(&mut ctx.ck, arbitrary_bad_span);
                                if out_pos.is_none() {
                                    out_pos = Some(out_pos1);
                                }
                                // we've grouped the cases by tag, now strip the tag
                                if let Some((tag, _tag_span)) = tag {
                                    if refutable1 {
                                        refutable = true;
                                    }
                                    let (var, var_out) = var.unwrap();
                                    let (var1, var1_inp) = var1.unwrap();
                                    ty_tag_map.insert(tag, (var1_inp, refutable1, (rest.ty, out_neg)));
                                    tag_map.insert(
                                        tag,
                                        Term {
                                            inner: TermInner::Bind {
                                                bindings: Box::new([(
                                                    var1,
                                                    Term {
                                                        inner: TermInner::VarAccess(var),
                                                        ty: var_out,
                                                    },
                                                )]),
                                                expr: Box::new(rest),
                                            },
                                            ty: out_pos1,
                                        },
                                    );
                                } else {
                                    refutable = refutable1;
                                    if let Some((var1, var1_inp)) = var1 {
                                        let (var, var_out) = var.unwrap();
                                        let rest = Term {
                                            inner: TermInner::Bind {
                                                bindings: Box::new([(
                                                    var1,
                                                    Term {
                                                        inner: TermInner::VarAccess(var),
                                                        ty: var_out,
                                                    },
                                                )]),
                                                expr: Box::new(rest),
                                            },
                                            ty: out_pos1,
                                        };
                                        ty_fallthrough = Some((var1_inp, (rest.ty, out_neg)));
                                        fallthrough = Some(Box::new(rest));
                                    } else {
                                        return Ok((rest, refutable1));
                                    }
                                }
                            }
                            let (var, var_out) = var.unwrap();
                            let ty = ctx.ck.add_ty(
                                Neg::Prim(NegPrim::Label {
                                    cases: ty_tag_map,
                                    fallthrough: ty_fallthrough,
                                }),
                                arbitrary_bad_span,
                            );
                            ctx.ck.flow(var_out, ty)?;
                            Ok((
                                Term {
                                    inner: TermInner::CheckTag {
                                        var,
                                        branches: tag_map,
                                        fallthrough,
                                    },
                                    ty: out_pos.unwrap(),
                                },
                                refutable,
                            ))
                        }
                        let mut args = LinkedList::new();
                        let mut args2 = Vec::new();
                        let mut args3 = Vec::new();
                        // TODO: multiple spans
                        // TODO: this should also show a different diag span depending on the branch
                        for pat in &exprs[0].0 {
                            let arg = self.ck.add_var(pat.label(), level);
                            let (pos, neg) = arg.polarize(&mut self.ck, pat.span);
                            args.push_back((arg, pos));
                            args2.push(neg);
                            args3.push(arg);
                        }
                        let (term, refutable) = compile_patterns_1(self, bindings, level, &mut args, exprs)?;
                        if refutable {
                            todo!("refutability oh no ub something something TODO error")
                        }
                        Ok(((len, (args2, term.ty)), (len, (args3, term))))
                    })
                    .collect::<Result<(BTreeMap<_, _>, BTreeMap<_, _>), diag::Error>>()?;
                // TODO: type with multiple spans
                let ty = Pos::Prim(PosPrim::Func(tys));
                let ty = self.ck.add_ty(ty, span);
                Ok(Term { ty, inner: TermInner::Value(Value::Lambda(exprs)) })
            }
            ast::ExprInner::TypeConstructor(ident) => {
                assert!(exprs.next().is_none());
                // spans are kinda wacky but whatever
                let id = bindings.get_type(&ident, expr.span, &mut self.ck, level)?;
                let arg = self.ck.add_var(None, level);
                let (arg_out, arg_inp) = arg.polarize(&mut self.ck, expr.span);
                let ret_out = self
                    .ck
                    .add_ty(Pos::Prim(PosPrim::Label(id, arg_out)), expr.span);
                let mut ty = BTreeMap::new();
                ty.insert(1, (vec![arg_inp], ret_out));
                let ty = Pos::Prim(PosPrim::Func(ty));
                let ty = self.ck.add_ty(ty, expr.span);
                let term = Term {
                    inner: TermInner::AttachTag(id, Box::new(Term {
                        inner: TermInner::VarAccess(arg),
                        ty: arg_out,
                    })),
                    ty: ret_out,
                };
                let lambda = [(1, (vec![arg], term))].into_iter().collect();
                return Ok((
                    Term {
                        inner: TermInner::Value(Value::Lambda(lambda)), ty,
                    },
                    TermMeta::Type(id),
                ));
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
                        // the var where we will store the result
                        let ret = self.ck.add_var(None, level);
                        // ret's span is just the entire expr
                        let (ret_out, ret_inp) = ret.polarize(&mut self.ck, expr.span);
                        let func_inp =
                            Neg::Prim(NegPrim::Func(args.iter().map(|x| x.ty).collect(), ret_inp));
                        // the func's span is, well, the func's span
                        let func_inp = self.ck.add_ty(func_inp, func_span);
                        self.ck.flow(func.ty, func_inp)?;
                        Ok(Term {
                            inner: TermInner::Application(Box::new(func), args),
                            ty: ret_out,
                        })
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
                let var = match *var.as_slice() {
                    [ref x] => x,
                    [ref x, ..] => {
                        let len = x.len();
                        return Err(diag::NameNotFoundError::new(
                            x,
                            Span {
                                right: expr.span.left + len,
                                ..expr.span
                            },
                            false,
                        )
                        .into());
                    }
                    _ => unreachable!(),
                };
                let (var, ty, meta) = bindings
                    .get(var, expr.span, &mut self.ck, level)?
                    .ok_or_else(|| diag::NameNotFoundError::new(var, expr.span, false))?;
                return Ok((
                    Term {
                        inner: TermInner::VarAccess(var),
                        ty,
                    },
                    meta,
                ));
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
                .map(|(is_type, ident, var, _var_out)| {
                    let t = if is_type {
                        VarType::TypeConstructor(var, self.ck.add_label(ident.clone()))
                    } else {
                        VarType::Poly(var)
                    };
                    (ident, t)
                })
                .collect(),
        );
        let mut vars = Vec::new();
        for (var, var_inp, expr1) in exprs {
            bindings.mark_cur(var);
            let expr1 = self.lower_expr(&mut bindings, expr1, level + 1)?.0;
            self.ck.flow(expr1.ty, var_inp)?;
            vars.push((var, expr1));
        }
        let order = bindings.orders.pop().unwrap();
        let vars = toposort(vars, order.0);
        let mut scope = Scope::default();
        scope.extend(vars);
        Ok((bindings, scope))
    }
}
