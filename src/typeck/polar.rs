//! This module has the most boring code I ever wrote in my life
use std::hash::Hash;

use indexmap::IndexSet;

use crate::{util::IdSpan, Span};

use super::{NegPrim, PolarType, PosPrim, TypeCk, VarId, VarState};

#[derive(Copy, Clone, Debug)]
pub enum AnyIdRef<'a, T: PolarPrimitive> {
    Same(&'a IdSpan<T>),
    Inverse(&'a IdSpan<T::Inverse>),
    Var(&'a VarId),
}

#[derive(Debug)]
pub enum AnyIdMut<'a, T: PolarPrimitive> {
    Same(&'a mut IdSpan<T>),
    Inverse(&'a mut IdSpan<T::Inverse>),
    Var(&'a mut VarId),
}

enum IterEither<A, B> {
    A(A),
    B(B),
}

impl<A: Iterator, B: Iterator<Item = A::Item>> Iterator for IterEither<A, B> {
    type Item = A::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::A(x) => x.next(),
            Self::B(x) => x.next(),
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::A(x) => x.size_hint(),
            Self::B(x) => x.size_hint(),
        }
    }
}

pub trait PolarPrimitive: Clone + Eq + Hash {
    type Inverse: PolarPrimitive<Inverse = Self>;
    const POSITIVE: bool;
    fn typeck_data_mut(ck: &mut TypeCk) -> (&mut Vec<PolarType<Self>>, &mut Vec<Span>);
    fn typeck_data(ck: &TypeCk) -> &Vec<PolarType<Self>>;
    fn var_data_mut(var: &mut VarState) -> &mut IndexSet<IdSpan<Self>>;
    fn var_data(var: &VarState) -> &IndexSet<IdSpan<Self>>;
    fn ids(&self) -> impl '_ + Iterator<Item = AnyIdRef<Self>>;
    fn ids_mut(&mut self) -> impl '_ + Iterator<Item = AnyIdMut<Self>>;
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
    fn var_data_mut(var: &mut VarState) -> &mut IndexSet<IdSpan<Self>> {
        &mut var.union
    }
    fn var_data(var: &VarState) -> &IndexSet<IdSpan<Self>> {
        &var.union
    }
    fn ids(&self) -> impl '_ + Iterator<Item = AnyIdRef<Self>> {
        match self {
            Self::Label(_, id) => {
                IterEither::A(IterEither::A([id].into_iter().map(AnyIdRef::Same)))
            }
            Self::Record(x) => IterEither::A(IterEither::B(x.values().map(AnyIdRef::Same))),
            Self::Func(cases) => IterEither::B(IterEither::A(cases.values().flat_map(|x| {
                x.0.iter()
                    .map(AnyIdRef::Inverse)
                    .chain([AnyIdRef::Same(&x.1)])
            }))),
            Self::Void
            | Self::Bool
            | Self::Int { .. }
            | Self::Float { .. }
            | Self::IntLiteral { .. }
            | Self::FloatLiteral => IterEither::B(IterEither::B([].into_iter())),
        }
    }
    fn ids_mut(&mut self) -> impl '_ + Iterator<Item = AnyIdMut<Self>> {
        match self {
            Self::Label(_, id) => {
                IterEither::A(IterEither::A([id].into_iter().map(AnyIdMut::Same)))
            }
            Self::Record(x) => IterEither::A(IterEither::B(x.values_mut().map(AnyIdMut::Same))),
            Self::Func(cases) => IterEither::B(IterEither::A(cases.values_mut().flat_map(|x| {
                x.0.iter_mut()
                    .map(AnyIdMut::Inverse)
                    .chain([AnyIdMut::Same(&mut x.1)])
            }))),
            Self::Void
            | Self::Bool
            | Self::Int { .. }
            | Self::Float { .. }
            | Self::IntLiteral { .. }
            | Self::FloatLiteral => IterEither::B(IterEither::B([].into_iter())),
        }
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
    fn var_data_mut(var: &mut VarState) -> &mut IndexSet<IdSpan<Self>> {
        &mut var.inter
    }
    fn var_data(var: &VarState) -> &IndexSet<IdSpan<Self>> {
        &var.inter
    }
    fn ids(&self) -> impl '_ + Iterator<Item = AnyIdRef<Self>> {
        match self {
            Self::Label { cases, fallthrough } => IterEither::A(IterEither::A(
                cases
                    .values()
                    .map(|x| &x.0)
                    .chain(fallthrough.as_ref())
                    .map(AnyIdRef::Same),
            )),
            Self::Func(a, b) => IterEither::A(IterEither::B(
                a.iter().map(AnyIdRef::Inverse).chain([AnyIdRef::Same(&b)]),
            )),
            Self::Record(_, x) => IterEither::B(IterEither::A(Some(AnyIdRef::Same(x)).into_iter())),
            Self::Void | Self::Bool | Self::Int { .. } | Self::Float { .. } => {
                IterEither::B(IterEither::B([].into_iter()))
            }
        }
    }
    fn ids_mut(&mut self) -> impl '_ + Iterator<Item = AnyIdMut<Self>> {
        match self {
            Self::Label { cases, fallthrough } => IterEither::A(IterEither::A(
                cases
                    .values_mut()
                    .map(|x| &mut x.0)
                    .chain(fallthrough.as_mut())
                    .map(AnyIdMut::Same),
            )),
            Self::Func(a, b) => IterEither::A(IterEither::B(
                a.iter_mut()
                    .map(AnyIdMut::Inverse)
                    .chain([AnyIdMut::Same(b)]),
            )),
            Self::Record(_, x) => IterEither::B(IterEither::A(Some(AnyIdMut::Same(x)).into_iter())),
            Self::Void | Self::Bool | Self::Int { .. } | Self::Float { .. } => {
                IterEither::B(IterEither::B([].into_iter()))
            }
        }
    }
}

impl<T: PolarPrimitive> PolarType<T> {
    pub fn ids(&self) -> impl '_ + Iterator<Item = AnyIdRef<T>> {
        enum Iter<'a, T: 'a + PolarPrimitive, I: Iterator<Item = AnyIdRef<'a, T>>> {
            Var(std::option::IntoIter<&'a VarId>),
            Prim(I),
        }
        impl<'a, T: 'a + PolarPrimitive, I: Iterator<Item = AnyIdRef<'a, T>>> Iterator for Iter<'a, T, I> {
            type Item = AnyIdRef<'a, T>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Var(x) => x.size_hint(),
                    Self::Prim(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Var(x) => x.next().map(AnyIdRef::Var),
                    Self::Prim(x) => x.next(),
                }
            }
        }
        match self {
            Self::Prim(x) => Iter::Prim(x.ids()),
            Self::Var(v) => Iter::Var(Some(v).into_iter()),
        }
    }
    pub fn ids_mut(&mut self) -> impl '_ + Iterator<Item = AnyIdMut<T>> {
        enum Iter<'a, T: 'a + PolarPrimitive, I: Iterator<Item = AnyIdMut<'a, T>>> {
            Var(std::option::IntoIter<&'a mut VarId>),
            Prim(I),
        }
        impl<'a, T: PolarPrimitive, I: Iterator<Item = AnyIdMut<'a, T>>> Iterator for Iter<'a, T, I> {
            type Item = AnyIdMut<'a, T>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Var(x) => x.size_hint(),
                    Self::Prim(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Var(x) => x.next().map(AnyIdMut::Var),
                    Self::Prim(x) => x.next(),
                }
            }
        }
        match self {
            Self::Prim(x) => Iter::Prim(x.ids_mut()),
            Self::Var(v) => Iter::Var(Some(v).into_iter()),
        }
    }
}
