// an ir that offers a high level representation of types and control flow
// it's the best choice for stuff like constant folding

use std::{
    collections::{BTreeMap, HashMap},
    str::FromStr,
};

use indexmap::{IndexMap, IndexSet};
use malachite::num::logic::traits::SignificantBits;

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
    Lambda(VarId, Box<Term>),
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
    Application(Box<Term>, Box<Term>),
    Bind {
        bindings: Box<[(VarId, Term)]>,
        expr: Box<Term>,
    },
    VarAccess(VarId),
    Value(Value),
    AttachTag(LabelId, Box<Term>),
    CheckTag {
        var: VarId,
        branches: BTreeMap<Option<LabelId>, Term>,
    },
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

type VarOrder = IndexSet<VarId>;

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
        let (var, _poly, type_id) = match var {
            VarType::Mono(var) => (var, false, None),
            VarType::Poly(var) => (var, true, None),
            VarType::TypeConstructor(var, id) => (var, true, Some(id)),
        };
        // TODO: remove the following line
        let poly = false;
        if let Some(lvl) = lvl {
            if !self.orders.is_empty() {
                self.orders[*lvl].insert(*var);
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
    fn insert_pattern(
        &mut self,
        pat: ast::Pattern,
        var: VarId,
        var_out: PosIdS,
        ck: &mut TypeCk,
        level: u16,
    ) -> Result<(Vec<(VarId, Term)>, Vec<String>), diag::Error> {
        let mut vars = Vec::new();
        let mut to_pop = Vec::new();
        match pat.inner {
            ast::PatternInner::Variable(name) => {
                to_pop.push(name.clone());
                self.insert_ck(name, VarType::Mono(var), false);
            }
            ast::PatternInner::Tag(tag, span, pat) => {
                let tag = self.get_type(&tag, span, ck, level)?;
                let var1 = ck.add_var(pat.label(), level);
                let (var1_out, var1_inp) = var1.polarize(ck, pat.span);
                let neg = ck.add_ty(
                    Neg::Prim(NegPrim::Label {
                        cases: {
                            let mut cases = BTreeMap::new();
                            cases.insert(tag, (var1_inp, false));
                            cases
                        },
                        fallthrough: None,
                    }),
                    pat.span,
                );
                ck.flow(var_out, neg)?;
                vars.push((
                    var1,
                    Term {
                        inner: TermInner::VarAccess(var),
                        ty: var1_out,
                    },
                ));
                let (ret2, to_pop2) = self.insert_pattern(*pat, var1, var1_out, ck, level)?;
                vars.extend(ret2);
                to_pop.extend(to_pop2);
            }
        }
        Ok((vars, to_pop))
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
        self.orders.push(IndexSet::new());
        to_pop
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

impl Ctx {
    fn alloc_var(
        &mut self,
        ident: String,
        defs: Vec<LetArm>,
        level: u16,
    ) -> Result<
        ((bool, Ident, VarId, PosIdS), (Ident, VarId, NegIdS, Expr)),
        diag::VarRedefinitionError,
    > {
        assert!(!defs.is_empty());
        let mut defs = defs.into_iter();
        let def = defs.next().unwrap();
        match def.pattern.inner {
            LetPatternInner::Val => {
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
                // its span is to be the pattern's span
                let (var_out, var_inp) = var.polarize(&mut self.ck, def.pattern.span);
                Ok((
                    (is_type, ident.clone(), var, var_out),
                    (ident, var, var_inp, def.body),
                ))
            }
            LetPatternInner::Func(args) => {
                if let Some(def1) = defs.next() {
                    return Err(diag::VarRedefinitionError::new(
                        ident,
                        [def.pattern.span, def1.pattern.span]
                            .into_iter()
                            .chain(defs.map(|x| x.pattern.span))
                            .collect(),
                    ));
                }
                let mut lambda = def.body;
                for arg in args.into_iter().rev() {
                    lambda = Expr {
                        span: Span {
                            left: arg.span.left,
                            ..lambda.span
                        },
                        inner: ast::ExprInner::Lambda(arg, Box::new(lambda)),
                    };
                }
                let var = self.ck.add_var(Some(ident.clone()), level + 1);
                let (var_out, var_inp) = var.polarize(&mut self.ck, def.pattern.span);
                Ok((
                    (false, ident.clone(), var, var_out),
                    (ident, var, var_inp, lambda),
                ))
            }
        }
    }
    fn lower_expr(
        &mut self,
        bindings: &mut Bindings,
        expr: ast::Expr,
        level: u16,
    ) -> Result<(Term, TermMeta), diag::Error> {
        (match expr.inner {
            ast::ExprInner::LetRec(arms, body) => {
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
                        for (_ident, var, var_inp, expr1) in exprs {
                            let expr1 = self.lower_expr(bindings, expr1, level + 1)?.0;
                            // now direct expr1 to var_inp (assignment)
                            self.ck.flow(expr1.ty, var_inp)?;
                            // and actually bind this variable
                            vars.push((var, expr1));
                        }
                        // finally, lower expr2 in the new scope
                        let expr = self.lower_expr(bindings, *body, level)?.0;
                        Ok::<_, diag::Error>((expr, vars))
                    },
                );
                let (expr, mut vars) = res?;
                vars.sort_by(|a, b| order.get_index_of(&b.0).cmp(&order.get_index_of(&a.0)));
                Ok(Term {
                    ty: expr.ty,
                    inner: TermInner::Bind {
                        bindings: vars.into(),
                        expr: Box::new(expr),
                    },
                })
            }
            ast::ExprInner::Lambda(pat, body) => {
                // first, allocate a new variable to be used for the arg
                let arg = self.ck.add_var(pat.label(), level);
                // its span is to be the pattern's span
                let (arg_out, arg_inp) = arg.polarize(&mut self.ck, pat.span);
                let tmp = bindings.insert_pattern(pat, arg, arg_out, &mut self.ck, level)?;
                bindings.new_scope(tmp, |bindings, vars| {
                    let func_span = expr.span;
                    // now, lower the body with that var assigned
                    let body1 = self.lower_expr(bindings, *body, level)?.0;
                    // the type will be a function that takes arg's type and returns body's type
                    let ty = Pos::Func(arg_inp, body1.ty);
                    // its span will be the function's span
                    let ty = self.ck.add_ty(ty, func_span);
                    // if new vars were created during pattern destructuring, bind them
                    let expr1 = if vars.is_empty() {
                        body1
                    } else {
                        Term {
                            ty: body1.ty,
                            inner: TermInner::Bind {
                                bindings: vars.into(),
                                expr: Box::new(body1),
                            },
                        }
                    };
                    Ok(Term {
                        ty,
                        inner: TermInner::Value(Value::Lambda(arg, Box::new(expr1))),
                    })
                })
            }
            ast::ExprInner::TypeConstructor(ident) => {
                // spans are kinda wacky but whatever
                let id = bindings.get_type(&ident, expr.span, &mut self.ck, level)?;
                let arg = self.ck.add_var(None, level);
                let (arg_out, arg_inp) = arg.polarize(&mut self.ck, expr.span);
                let ret_out = self
                    .ck
                    .add_ty(Pos::Prim(PosPrim::Label(id, arg_out)), expr.span);
                let ty = Pos::Func(arg_inp, ret_out);
                let ty = self.ck.add_ty(ty, expr.span);
                return Ok((
                    Term {
                        inner: TermInner::Value(Value::Lambda(
                            arg,
                            Box::new(Term {
                                inner: TermInner::AttachTag(
                                    id,
                                    Box::new(Term {
                                        inner: TermInner::VarAccess(arg),
                                        ty: arg_out,
                                    }),
                                ),
                                ty: ret_out,
                            }),
                        )),
                        ty,
                    },
                    TermMeta::Type(id),
                ));
            }
            ast::ExprInner::BinOp(op, a, b) => {
                let func_span = a.span;
                let (func, func_meta) = self.lower_expr(bindings, *a, level)?;
                let arg = self.lower_expr(bindings, *b, level)?.0;
                match op {
                    ast::BinOp::Call => {
                        match func_meta {
                            TermMeta::None => {
                                // the var where we will store the result
                                let ret = self.ck.add_var(None, level);
                                // ret's span is just the entire expr
                                let (ret_out, ret_inp) = ret.polarize(&mut self.ck, expr.span);
                                let func_inp = Neg::Func(arg.ty, ret_inp);
                                // the func's span is, well, the func's span
                                let func_inp = self.ck.add_ty(func_inp, func_span);
                                self.ck.flow(func.ty, func_inp)?;
                                Ok(Term {
                                    inner: TermInner::Application(Box::new(func), Box::new(arg)),
                                    ty: ret_out,
                                })
                            }
                            TermMeta::Type(id) => {
                                let ty = self
                                    .ck
                                    .add_ty(Pos::Prim(PosPrim::Label(id, arg.ty)), expr.span);
                                Ok(Term {
                                    inner: TermInner::AttachTag(id, Box::new(arg)),
                                    ty,
                                })
                            }
                        }
                    }
                    // no adhoc polymorphism means no math, frick you
                    _ => panic!(),
                }
            }
            ast::ExprInner::UnOp(..) => todo!(),
            ast::ExprInner::Literal(kind, value) => (match kind {
                ast::LiteralKind::Bool => match value.as_str() {
                    "false" => Ok((Value::Bool(false), Pos::Prim(PosPrim::Bool))),
                    "true" => Ok((Value::Bool(true), Pos::Prim(PosPrim::Bool))),
                    x => Err(diag::Error::Other(
                        format!("unexpected boolean literal: {x}"),
                        expr.span,
                    )),
                },
                ast::LiteralKind::Int => match malachite_nz::integer::Integer::from_str(&value) {
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
            }),
            ast::ExprInner::Variable(var) => {
                let (var, ty, meta) = bindings
                    .get(&var, expr.span, &mut self.ck, level)?
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
        let mut scope = Scope::default();
        for (_ident, var, var_inp, expr1) in exprs {
            let expr1 = self.lower_expr(&mut bindings, expr1, level + 1)?.0;
            // now direct expr1 to var_inp (assignment)
            self.ck.flow(expr1.ty, var_inp)?;
            // and actually bind this variable
            scope.insert(var, expr1);
            // vars.0.push((var, expr1));
        }
        let order = bindings.orders.pop().unwrap();
        scope
            .map
            .sort_by(|a, _, b, _| order.get_index_of(b).cmp(&order.get_index_of(a)));
        Ok((bindings, scope))
    }
}
