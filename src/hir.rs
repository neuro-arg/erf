// an ir that offers a high level representation of types and control flow
// it's the best choice for stuff like constant folding

use std::{collections::HashMap, str::FromStr};

use indexmap::IndexMap;
use malachite::num::logic::traits::SignificantBits;

use crate::{
    ast::{self, Expr, Ident, LetArm, LetPatternInner},
    diag,
    typeck::{Edge, Neg, NegIdS, Pos, PosIdS, PosPrim, Query, QueryId, TypeCk, VarId},
    Span,
};

// VarId technically comes from the type checker,
// but its variable IDs are reused here for convenience.
// Think of them as both HIR variables and type checker
// variables.

#[derive(Clone, Debug)]
pub enum Value {
    Lambda(VarId, Box<Term>),
    Match(QueryId, Vec<Term>),
    Bool(bool),
    Float(f64),
    Int(malachite_nz::integer::Integer),
    Intrinsic(Intrinsic),
}

#[derive(Copy, Clone, Debug)]
pub enum VarType {
    Mono(VarId),
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

#[derive(Clone, Debug, Default)]
pub struct Bindings {
    ck_map: HashMap<String, Vec<VarType>>,
}

#[derive(Clone, Debug)]
struct Binding {
    var: VarId,
    term: Term,
}

impl Bindings {
    fn insert_ck(&mut self, s: String, var: VarType) {
        self.ck_map.entry(s).or_default().push(var);
    }
    fn remove_ck(&mut self, s: &str) {
        self.ck_map.get_mut(s).unwrap().pop();
    }
    pub fn get(&self, s: &str) -> Option<VarType> {
        self.ck_map.get(s).and_then(|x| x.last()).copied()
    }
    fn insert_pattern(
        &mut self,
        pat: ast::Pattern,
        var: VarId,
        _var_out: PosIdS,
    ) -> (Vec<(VarId, Term)>, Vec<String>) {
        let ret1 = Vec::new();
        let mut to_pop = Vec::new();
        match pat.inner {
            ast::PatternInner::Variable(name) => {
                to_pop.push(name.clone());
                self.insert_ck(name, VarType::Mono(var));
            }
        }
        (ret1, to_pop)
    }
    fn insert_pattern_let(&mut self, arms: Vec<(Ident, VarId, PosIdS)>) -> Vec<String> {
        let mut to_pop = Vec::new();
        // FIXME: toposort this NOW!
        // and, in fact, only allow cycles between functions
        // (if two values need each other or a value and a function need each other that can't
        // easily be lowered and more likely than not is broken)
        for (ident, var, _var_out) in arms {
            to_pop.push(ident.clone());
            self.insert_ck(ident, VarType::Poly(var));
        }
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
        pat: ast::Pattern,
        var: VarId,
        var_out: PosIdS,
        func: impl FnOnce(&mut Self, Vec<(VarId, Term)>) -> T,
    ) -> T {
        let (vars, to_pop) = self.insert_pattern(pat, var, var_out);
        let ret = func(self, vars);
        for name in to_pop {
            self.remove_ck(&name);
        }
        ret
    }
    fn new_scope_let<T>(
        &mut self,
        arms: Vec<(Ident, VarId, PosIdS)>,
        func: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let to_pop = self.insert_pattern_let(arms);
        let ret = func(self);
        for name in to_pop {
            self.remove_ck(&name);
        }
        ret
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

impl Ctx {
    fn alloc_var(
        &mut self,
        ident: String,
        exprs: Vec<LetArm>,
        level: u16,
    ) -> Result<((Ident, VarId, PosIdS), (Ident, VarId, NegIdS, Expr)), diag::Error> {
        assert!(!exprs.is_empty());
        if matches!(exprs.first().unwrap().pattern.inner, LetPatternInner::Val) {
            if exprs.len() != 1 {
                return Err(diag::VarRedefinitionError::new(
                    ident,
                    exprs.into_iter().map(|x| x.pattern.span).collect(),
                )
                .into());
            }
            let def = exprs.into_iter().next().unwrap();
            let pat = def.pattern;
            let expr = def.body;
            let var = self.ck.add_var(Some(ident.clone()), level + 1);
            // its span is to be the pattern's span
            let (var_out, var_inp) = var.polarize(&mut self.ck, pat.span);
            Ok(((ident.clone(), var, var_out), (ident, var, var_inp, expr)))
        } else {
            let span1 = exprs.first().unwrap().pattern.span;
            let exprs = exprs
                .into_iter()
                .map(|expr| {
                    Ok(match expr.pattern.inner {
                        LetPatternInner::Val => {
                            return Err(diag::MixedPolyMonoVarError::new(
                                ident.clone(),
                                expr.pattern.span,
                                span1,
                            ))
                        }
                        LetPatternInner::Func(f) => {
                            let mut expr = expr.body;
                            for pat in f.into_iter().rev() {
                                expr = ast::Expr {
                                    span: Span::new(expr.span.file, pat.span.left, expr.span.right),
                                    inner: ast::ExprInner::Lambda(pat, Box::new(expr)),
                                };
                            }
                            expr
                        }
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            if exprs.len() == 1 {
                let expr = exprs.into_iter().next().unwrap();
                // let pat = def.pattern;
                // let expr = def.body;
                let var = self.ck.add_var(Some(ident.clone()), level + 1);
                // its span is to be the pattern's span
                let (var_out, var_inp) = var.polarize(&mut self.ck, span1);
                Ok(((ident.clone(), var, var_out), (ident, var, var_inp, expr)))
            } else {
                let var = self.ck.add_var(Some(ident.clone()), level + 1);
                // let (var_out, var_inp) = var.polarize(&mut self.ck, span1);
                let var_out = self.ck.add_ty(Pos::Var(var), span1);
                let m = self.ck.add_query(Query::Match {
                    current: 0,
                    branches: (),
                    name: Some(ident.clone()),
                    in_var: (var, var_out),
                });
                todo!()
            }
        }
    }
    fn lower_expr(
        &mut self,
        bindings: &mut Bindings,
        expr: ast::Expr,
        level: u16,
    ) -> Result<Term, diag::Error> {
        match expr.inner {
            ast::ExprInner::LetRec(arms, body) => {
                // first, allocate the variable that is to be assigned <expr1>
                let (pats, exprs) = arms
                    .into_iter()
                    .map(|(ident, pat_exprs)| self.alloc_var(ident, pat_exprs, level))
                    .collect::<Result<(Vec<_>, Vec<_>), _>>()?;
                // now create a new scope with that var and lower the expr in that scope
                bindings.new_scope_let(pats, |bindings| {
                    let mut vars = Vec::new();
                    for (_ident, var, var_inp, expr1) in exprs {
                        let expr1 = self.lower_expr(bindings, expr1, level + 1)?;
                        // now direct expr1 to var_inp (assignment)
                        self.ck.flow(expr1.ty, var_inp, Edge::Strong)?;
                        // and actually bind this variable
                        vars.push((var, expr1));
                    }
                    // finally, lower expr2 in the new scope
                    let expr = self.lower_expr(bindings, *body, level)?;
                    Ok(Term {
                        ty: expr.ty,
                        inner: TermInner::Bind {
                            bindings: vars.into(),
                            expr: Box::new(expr),
                        },
                    })
                })
            }
            ast::ExprInner::Lambda(pat, body) => {
                // first, allocate a new variable to be used for the arg
                let arg = self.ck.add_var(pat.label(), level);
                // its span is to be the pattern's span
                let (arg_out, arg_inp) = arg.polarize(&mut self.ck, pat.span);
                bindings.new_scope(pat, arg, arg_out, |bindings, vars| {
                    let func_span = expr.span;
                    // now, lower the body with that var assigned
                    let body1 = self.lower_expr(bindings, *body, level)?;
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
            ast::ExprInner::BinOp(op, a, b) => {
                let func_span = a.span;
                let func = self.lower_expr(bindings, *a, level)?;
                let arg = self.lower_expr(bindings, *b, level)?;
                match op {
                    ast::BinOp::Call => {
                        // the var where we will store the result
                        let ret = self.ck.add_var(None, level);
                        // ret's span is just the entire expr
                        let (ret_out, ret_inp) = ret.polarize(&mut self.ck, expr.span);
                        let func_inp = Neg::Func(arg.ty, ret_inp);
                        // the func's span is, well, the func's span
                        let func_inp = self.ck.add_ty(func_inp, func_span);
                        self.ck.flow(func.ty, func_inp, Edge::Strong)?;
                        Ok(Term {
                            inner: TermInner::Application(Box::new(func), Box::new(arg)),
                            ty: ret_out,
                        })
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
                let var = bindings
                    .get(&var)
                    .ok_or_else(|| diag::NameNotFoundError::new(var, expr.span))?;
                let (var, poly) = match var {
                    VarType::Mono(var) => (var, false),
                    VarType::Poly(var) => (var, true),
                };
                let ty = self.ck.add_ty(Pos::Var(var), expr.span);
                let ty = if poly {
                    self.ck.monomorphize(ty, level)
                } else {
                    ty
                };
                Ok(Term {
                    inner: TermInner::VarAccess(var),
                    ty,
                })
            }
        }
    }

    pub fn lower_ast(&mut self, ast: ast::Program) -> Result<(Bindings, Scope), diag::Error> {
        let mut bindings = Bindings::default();
        let mut ret1 = Scope::default();
        let level = 0;
        let (pats, exprs) = ast
            .into_iter()
            .map(|(ident, pat_exprs)| self.alloc_var(ident, pat_exprs, level))
            .collect::<Result<(Vec<_>, Vec<_>), _>>()?;
        // now create a new scope with that var and lower the expr in that scope
        let _ = bindings.insert_pattern_let(pats);
        for (_ident, var, var_inp, expr1) in exprs {
            let expr1 = self.lower_expr(&mut bindings, expr1, level + 1)?;
            // now direct expr1 to var_inp (assignment)
            self.ck.flow(expr1.ty, var_inp, Edge::Strong)?;
            // and actually bind this variable
            ret1.insert(var, expr1);
            // vars.0.push((var, expr1));
        }
        /*for ast::Tld {
            pattern,
            body,
            span: _,
        } in ast
        {
            let var = self.ck.add_var(Some(pattern.label());
            let (var_out, var_inp) = var.polarize(&mut self.ck, pattern.span);
            let (vars, _) = bindings.insert_pattern_let(pattern, var, var_out);
            tmp.push((body, var, var_inp, vars));
        }
        for (body, var, var_inp, vars) in tmp {
            let body = self.lower_expr(&mut bindings, body)?;
            self.ck.flow(body.ty, var_inp)?;
            ret1.insert(var, body);
            ret1.extend(vars);
        }*/
        Ok((bindings, ret1))
    }
}
