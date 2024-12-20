use std::collections::BTreeMap;

use indexmap::IndexMap;

use crate::{
    diag::Error,
    hir::{self, Term},
    typeck::{LabelId, VarId},
};

#[derive(Clone, Debug)]
pub enum Value {
    Lambda(Scope, Option<VarId>, Term),
    Bool(bool),
    Float(f64),
    Int(malachite_nz::integer::Integer),
    Intrinsic(String),
    Tagged(LabelId, Box<Value>),
    Record(BTreeMap<String, Value>),
}

#[derive(Clone, Debug, Default)]
pub struct Scope {
    vars: IndexMap<VarId, Vec<Value>>,
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
    // println!("eval in {:?}", scope.vars.keys().collect::<Vec<_>>());
    match term.inner {
        hir::TermInner::Record(x) => {
            let mut ret = BTreeMap::new();
            for (k, v) in x {
                ret.insert(k, eval_term(scope, v)?);
            }
            Ok(Value::Record(ret))
        }
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
        hir::TermInner::If(a, b, c) => {
            let a = eval_term(scope, *a)?;
            match a {
                Value::Bool(true) => eval_term(scope, *b),
                Value::Bool(false) => eval_term(scope, *c),
                _ => panic!("not bool, this shouldn't typecheck"),
            }
        }
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
            let mut ret = eval_term(scope, *val)?;
            Ok(match &mut ret {
                Value::Tagged(tag1, _) => {
                    *tag1 = tag;
                    ret
                }
                _ => Value::Tagged(tag, Box::new(ret)),
            })
        }
        hir::TermInner::Bind { bindings, expr } => scope.scope(|scope, handle| {
            for (var, term) in Vec::from(bindings) {
                let val = eval_term(scope, term)?;
                scope.insert(handle, var, val);
            }
            eval_term(scope, *expr)
        }),
        hir::TermInner::Field(term, field) => {
            let val = eval_term(scope, *term)?;
            match val {
                Value::Record(mut x) => Ok(x.remove(&field).unwrap()),
                _ => panic!("no field {field} on non-record"),
            }
        }
        hir::TermInner::Value(val) => Ok(match val {
            hir::Value::Lambda(x, y) => Value::Lambda(scope.clone(), Some(x), *y),
            hir::Value::FuncEntry(y) => Value::Lambda(scope.clone(), None, *y),
            hir::Value::Int(x) => Value::Int(x),
            hir::Value::Bool(x) => Value::Bool(x),
            hir::Value::Float(x) => Value::Float(x),
            hir::Value::Intrinsic(x) => Value::Intrinsic(x),
        }),
        hir::TermInner::VarAccess(var) => Ok(scope
            .get(var)
            .unwrap_or_else(|| {
                panic!(
                    "{var:?} not found, available vars are {:?}",
                    scope.vars.keys().collect::<Vec<_>>()
                )
            })
            .clone()),
        hir::TermInner::Application(func, expr) => {
            let mut func = eval_term(scope, *func)?;
            let (mut func_scope, arg, body) = loop {
                match func {
                    Value::Tagged(_, x) => func = *x,
                    Value::Lambda(func_scope, arg, body) if arg.is_some() == expr.is_some() => {
                        break (func_scope, arg, body)
                    }
                    Value::Lambda(mut func_scope, arg, body) => {
                        assert!(arg.is_none());
                        func = func_scope.scope(|scope, _handle| eval_term(scope, body))?;
                    }
                    _ => {
                        unreachable!("unexpected application to {func:?}, why did this typecheck?");
                    }
                }
            };
            let expr = expr.map(|expr| eval_term(scope, *expr)).transpose()?;
            func_scope.scope(|scope, handle| {
                if let Some(expr) = expr {
                    scope.insert(handle, arg.unwrap(), expr);
                }
                eval_term(scope, body)
            })
        }
    }
}
