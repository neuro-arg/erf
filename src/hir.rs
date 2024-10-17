// an ir that offers a high level representation of types and control flow
// it's the best choice for stuff like constant folding

use std::{collections::HashMap, str::FromStr};

use malachite::num::logic::traits::SignificantBits;

use crate::{
    ast, diag,
    typeck::{Neg, Pos, PosIdS, TypeCk, VarId},
    util::OrderedMap,
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

#[derive(Clone, Default)]
pub struct Bindings {
    ck_map: HashMap<String, Vec<VarId>>,
}

impl Bindings {
    fn insert_ck(&mut self, s: String, var: VarId) {
        self.ck_map.entry(s).or_default().push(var);
    }
    fn remove_ck(&mut self, s: &str) {
        self.ck_map.get_mut(s).unwrap().pop();
    }
    pub fn get(&self, s: &str) -> Option<VarId> {
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
                self.insert_ck(name, var);
            }
        }
        (ret1, to_pop)
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
}

#[derive(Debug, Default)]
pub struct Scope {
    pub map: OrderedMap<VarId, Term>,
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
    fn lower_expr(
        &mut self,
        bindings: &mut Bindings,
        expr: ast::Expr,
    ) -> Result<Term, diag::Error> {
        match expr.inner {
            ast::ExprInner::LetRec(pat, expr1, expr2) => {
                // first, allocate the variable that is to be assigned <expr1>
                let var = self.ck.add_var(pat.label());
                // its span is to be the pattern's span
                let (var_out, var_inp) = var.polarize(&mut self.ck, pat.span);
                // now create a new scope with that var and lower the expr in that scope
                bindings.new_scope(pat, var, var_out, |bindings, mut vars| {
                    let expr1 = self.lower_expr(bindings, *expr1)?;
                    // now direct expr1 to var_inp (assignment)
                    self.ck.flow(expr1.ty, var_inp)?;
                    // and actually bind this variable
                    vars.insert(0, (var, expr1));
                    // finally, lower expr2 in the new scope
                    let expr = self.lower_expr(bindings, *expr2)?;
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
                let arg = self.ck.add_var(pat.label());
                // its span is to be the pattern's span
                let (arg_out, arg_inp) = arg.polarize(&mut self.ck, pat.span);
                bindings.new_scope(pat, arg, arg_out, |bindings, vars| {
                    let func_span = expr.span;
                    // now, lower the body with that var assigned
                    let body1 = self.lower_expr(bindings, *body)?;
                    // the type will be a function that takes arg's type and returns body's type
                    let ty = Pos::Func(arg_inp, body1.ty);
                    // its span will be the function's span
                    let ty = self.ck.add_pos(ty, func_span);
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
                let func = self.lower_expr(bindings, *a)?;
                let arg_span = b.span;
                let arg = self.lower_expr(bindings, *b)?;
                match op {
                    ast::BinOp::Call => {
                        // the var where we will store the result
                        let ret = self.ck.add_var(None);
                        // ret's span is just the entire expr
                        let (ret_out, ret_inp) = ret.polarize(&mut self.ck, expr.span);
                        let func_inp = Neg::Func(arg.ty, ret_inp);
                        // the input's span is just the arg's span
                        let func_inp = self.ck.add_neg(func_inp, arg_span);
                        self.ck.flow(func.ty, func_inp)?;
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
                    "false" => Ok((Value::Bool(false), Pos::Bool)),
                    "true" => Ok((Value::Bool(true), Pos::Bool)),
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
                        Ok((Value::Int(val), Pos::IntLiteral { signed, bits }))
                    }
                    Err(()) => Err(diag::Error::Other(
                        "invalid integer literal".to_owned(),
                        expr.span,
                    )),
                },
                ast::LiteralKind::Float => match value.parse::<f64>() {
                    Ok(val) => Ok((Value::Float(val), Pos::FloatLiteral)),
                    Err(err) => Err(diag::Error::Other(
                        format!("invalid float literal: {err}"),
                        expr.span,
                    )),
                },
            })
            .map(|(val, ty)| Term {
                ty: self.ck.add_pos(ty, expr.span),
                inner: TermInner::Value(val),
            }),
            ast::ExprInner::Variable(var) => {
                let var = bindings
                    .get(&var)
                    .ok_or_else(|| diag::NameNotFoundError::new(var, expr.span))?;
                let ty = self.ck.add_pos(Pos::Var(var), expr.span);
                Ok(Term {
                    inner: TermInner::VarAccess(var),
                    ty,
                })
            }
        }
    }

    pub fn lower_ast(&mut self, ast: ast::Program) -> Result<(Bindings, Scope), diag::Error> {
        let mut ret0 = Bindings::default();
        let mut ret1 = Scope::default();
        let mut tmp = Vec::new();
        for ast::Tld {
            pattern,
            body,
            span: _,
        } in ast
        {
            let var = self.ck.add_var(pattern.label());
            let (var_out, var_inp) = var.polarize(&mut self.ck, pattern.span);
            let (vars, _) = ret0.insert_pattern(pattern, var, var_out);
            tmp.push((body, var, var_inp, vars));
        }
        for (body, var, var_inp, vars) in tmp {
            let body = self.lower_expr(&mut ret0, body)?;
            self.ck.flow(body.ty, var_inp)?;
            ret1.insert(var, body);
            ret1.extend(vars);
        }
        Ok((ret0, ret1))
    }
}
