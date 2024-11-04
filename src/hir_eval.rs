use std::collections::HashMap;

use crate::{
    diag::Error,
    hir::{self, Term},
    typeck::{TypeCk, VarId},
};

#[derive(Clone, Debug)]
pub enum Value {
    Lambda(Scope, VarId, Box<Term>),
    Bool(bool),
    Float(f64),
    Int(malachite_nz::integer::Integer),
    Intrinsic(hir::Intrinsic),
}

#[derive(Clone, Debug, Default)]
pub struct Scope {
    vars: HashMap<VarId, Vec<Value>>,
}

#[derive(Debug, Default)]
pub struct ScopeHandle(Vec<VarId>);

impl Scope {
    fn raw_insert(&mut self, var: VarId, val: Value) {
        self.vars.entry(var).or_default().push(val);
    }
    fn raw_remove(&mut self, var: VarId) {
        self.vars.entry(var).or_default().pop();
    }
    pub fn insert(&mut self, handle: &mut ScopeHandle, var: VarId, val: Value) {
        handle.0.push(var);
        self.raw_insert(var, val);
    }
    pub fn get(&self, var: VarId) -> Option<&Value> {
        self.vars.get(&var).and_then(|x| x.last())
    }
    pub fn scope<T>(&mut self, func: impl FnOnce(&mut Self, &mut ScopeHandle) -> T) -> T {
        let mut bindings = ScopeHandle::default();
        let ret = func(self, &mut bindings);
        for var in bindings.0.into_iter().rev() {
            self.raw_remove(var);
        }
        ret
    }
    pub fn from_hir_scope(value: hir::Scope, ck: &TypeCk) -> Result<Self, Error> {
        let mut ret = Self::default();
        for (var, x) in value.map {
            let term = eval_term(&mut ret, ck, x)?;
            ret.raw_insert(var, term);
        }
        Ok(ret)
    }
}

pub fn eval_term(scope: &mut Scope, ck: &TypeCk, term: Term) -> Result<Value, Error> {
    match term.inner {
        hir::TermInner::Bind { bindings, expr } => scope.scope(|scope, handle| {
            for (var, term) in Vec::from(bindings) {
                let val = eval_term(scope, ck, term)?;
                scope.insert(handle, var, val);
            }
            eval_term(scope, ck, *expr)
        }),
        hir::TermInner::Value(val) => Ok(match val {
            hir::Value::Match(id, b) => {
                eval_term(scope, ck, b.into_iter().nth(ck.match_branch(id)).unwrap())?
            }
            hir::Value::Lambda(a, b) => Value::Lambda(scope.clone(), a, b),
            hir::Value::Int(x) => Value::Int(x),
            hir::Value::Bool(x) => Value::Bool(x),
            hir::Value::Float(x) => Value::Float(x),
            hir::Value::Intrinsic(x) => Value::Intrinsic(x),
        }),
        hir::TermInner::VarAccess(var) => Ok(scope.get(var).unwrap().clone()),
        hir::TermInner::Application(func, expr) => {
            let func = eval_term(scope, ck, *func)?;
            let Value::Lambda(mut func_scope, arg, body) = func else {
                unreachable!("unexpected application to {func:?}, why did this typecheck?");
            };
            let expr = eval_term(scope, ck, *expr)?;
            func_scope.scope(|scope, handle| {
                scope.insert(handle, arg, expr);
                eval_term(scope, ck, *body)
            })
        }
    }
}
