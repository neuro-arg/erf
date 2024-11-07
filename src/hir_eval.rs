use std::collections::{BTreeMap, HashMap};

use crate::{
    diag::Error,
    hir::{self, Term},
    typeck::{LabelId, VarId},
};

#[derive(Clone, Debug)]
pub enum Value {
    Lambda(Scope, BTreeMap<usize, (Vec<VarId>, Term)>),
    Bool(bool),
    Float(f64),
    Int(malachite_nz::integer::Integer),
    Intrinsic(hir::Intrinsic),
    Tagged(LabelId, Box<Value>),
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
}

impl TryFrom<hir::Scope> for Scope {
    type Error = Error;
    fn try_from(value: hir::Scope) -> Result<Self, Self::Error> {
        let mut ret = Self::default();
        for (var, x) in value.map {
            let term = eval_term(&mut ret, x)?;
            ret.raw_insert(var, term);
        }
        Ok(ret)
    }
}

pub fn eval_term(scope: &mut Scope, term: Term) -> Result<Value, Error> {
    println!("{:#?}", term.inner);
    match term.inner {
        hir::TermInner::CheckTag {
            var,
            mut branches,
            fallthrough,
        } => {
            if let Value::Tagged(tag, _) = scope.get(var).unwrap() {
                if let Some(branch) = branches.remove(tag) {
                    match eval_term(scope, branch) {
                        Err(Error::NoValue) => {}
                        x => return x,
                    }
                }
            }
            if let Some(fallthrough) = fallthrough {
                eval_term(scope, *fallthrough)
            } else {
                Err(Error::NoValue)
            }
        }
        hir::TermInner::Fallthrough => Err(Error::NoValue),
        hir::TermInner::If(..) => todo!(),
        hir::TermInner::Sequence(x) => {
            for x in x {
                match eval_term(scope, x) {
                    Err(Error::NoValue) => {}
                    x => return x,
                }
            }
            Err(Error::NoValue)
        }
        hir::TermInner::AttachTag(tag, val) => {
            Ok(Value::Tagged(tag, Box::new(eval_term(scope, *val)?)))
        }
        hir::TermInner::Bind { bindings, expr } => scope.scope(|scope, handle| {
            for (var, term) in Vec::from(bindings) {
                let val = eval_term(scope, term)?;
                scope.insert(handle, var, val);
            }
            eval_term(scope, *expr)
        }),
        hir::TermInner::Value(val) => Ok(match val {
            hir::Value::Lambda(x) => Value::Lambda(scope.clone(), x),
            hir::Value::Int(x) => Value::Int(x),
            hir::Value::Bool(x) => Value::Bool(x),
            hir::Value::Float(x) => Value::Float(x),
            hir::Value::Intrinsic(x) => Value::Intrinsic(x),
        }),
        hir::TermInner::VarAccess(var) => Ok(scope.get(var).unwrap().clone()),
        hir::TermInner::Application(func, exprs) => {
            let mut func = eval_term(scope, *func)?;
            let (mut func_scope, mut func) = loop {
                match func {
                    Value::Tagged(_, x) => func = *x,
                    Value::Lambda(func_scope, func) => break (func_scope, func),
                    _ => {
                        unreachable!("unexpected application to {func:?}, why did this typecheck?");
                    }
                }
            };
            let exprs = exprs.into_iter().map(|expr| eval_term(scope, expr));
            let (args, body) = func
                .remove(&exprs.len())
                .expect("invalid argc????? why did this typecheck?");
            func_scope.scope(|scope, handle| {
                for (arg, expr) in args.into_iter().zip(exprs) {
                    scope.insert(handle, arg, expr?);
                }
                eval_term(scope, body)
            })
        }
    }
}
