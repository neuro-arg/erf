//! This module has the most boring code I ever wrote in my life
use std::hash::Hash;

use indexmap::IndexMap;

use crate::{util::IdSpan, Span};

use super::{Edge, NegPrim, PolarType, PosPrim, TypeCk, VarId, VarState};

// NOTE: see the invariant comment in TypeCk::postproc

#[derive(Copy, Clone, Debug)]
pub enum AnyId<T: PolarPrimitive> {
    Same(IdSpan<T>),
    Inverse(IdSpan<T::Inverse>),
    Var(VarId),
}

#[derive(Copy, Clone, Debug)]
pub enum AnyIdRef<'a, T: PolarPrimitive> {
    Same(&'a IdSpan<T>),
    Inverse(&'a IdSpan<T::Inverse>),
    Var(&'a VarId),
}

impl<T: PolarPrimitive> AnyIdRef<'_, T> {
    pub fn into_owned(self) -> AnyId<T> {
        match self {
            Self::Same(x) => AnyId::Same(*x),
            Self::Inverse(x) => AnyId::Inverse(*x),
            Self::Var(x) => AnyId::Var(*x),
        }
    }
}

#[derive(Debug)]
pub enum AnyIdMut<'a, T: PolarPrimitive> {
    Same(&'a mut IdSpan<T>),
    Inverse(&'a mut IdSpan<T::Inverse>),
    Var(&'a mut VarId),
}

pub trait PolarPrimitive: Clone + Eq + Hash {
    type Inverse: PolarPrimitive<Inverse = Self>;
    const POSITIVE: bool;
    fn typeck_data_mut(ck: &mut TypeCk) -> (&mut Vec<PolarType<Self>>, &mut Vec<Span>);
    fn typeck_data(ck: &TypeCk) -> &Vec<PolarType<Self>>;
    fn var_data_mut(var: &mut VarState) -> &mut IndexMap<IdSpan<Self>, Edge>;
    fn var_data(var: &VarState) -> &IndexMap<IdSpan<Self>, Edge>;
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
    fn var_data_mut(var: &mut VarState) -> &mut IndexMap<IdSpan<Self>, Edge> {
        &mut var.union
    }
    fn var_data(var: &VarState) -> &IndexMap<IdSpan<Self>, Edge> {
        &var.union
    }
    fn ids(&self) -> impl '_ + Iterator<Item = AnyIdRef<Self>> {
        enum Iter<'a, T: PolarPrimitive> {
            Default,
            Record(std::collections::btree_map::Values<'a, String, IdSpan<T>>),
        }
        impl<'a, T: PolarPrimitive> Iterator for Iter<'a, T> {
            type Item = AnyIdRef<'a, T>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Record(x) => x.next().map(AnyIdRef::Same),
                }
            }
        }
        match self {
            Self::Record(x) => Iter::Record(x.values()),
            Self::Void
            | Self::Bool
            | Self::Int { .. }
            | Self::Float { .. }
            | Self::IntLiteral { .. }
            | Self::FloatLiteral => Iter::Default,
        }
    }
    fn ids_mut(&mut self) -> impl '_ + Iterator<Item = AnyIdMut<Self>> {
        enum Iter<'a, T: PolarPrimitive> {
            Default,
            Record(std::collections::btree_map::ValuesMut<'a, String, IdSpan<T>>),
        }
        impl<'a, T: PolarPrimitive> Iterator for Iter<'a, T> {
            type Item = AnyIdMut<'a, T>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Record(x) => x.next().map(AnyIdMut::Same),
                }
            }
        }
        match self {
            Self::Record(x) => Iter::Record(x.values_mut()),
            Self::Void
            | Self::Bool
            | Self::Int { .. }
            | Self::Float { .. }
            | Self::IntLiteral { .. }
            | Self::FloatLiteral => Iter::Default,
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
    fn var_data_mut(var: &mut VarState) -> &mut IndexMap<IdSpan<Self>, Edge> {
        &mut var.inter
    }
    fn var_data(var: &VarState) -> &IndexMap<IdSpan<Self>, Edge> {
        &var.inter
    }
    fn ids(&self) -> impl '_ + Iterator<Item = AnyIdRef<Self>> {
        enum Iter<'a, T: PolarPrimitive> {
            Default,
            Record(std::option::IntoIter<&'a IdSpan<T>>),
        }
        impl<'a, T: PolarPrimitive> Iterator for Iter<'a, T> {
            type Item = AnyIdRef<'a, T>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Record(x) => x.next().map(AnyIdRef::Same),
                }
            }
        }
        match self {
            Self::Record(_, x) => Iter::Record(Some(x).into_iter()),
            Self::Void | Self::Bool | Self::Int { .. } | Self::Float { .. } => Iter::Default,
        }
    }
    fn ids_mut(&mut self) -> impl '_ + Iterator<Item = AnyIdMut<Self>> {
        enum Iter<'a, T: PolarPrimitive> {
            Default,
            Record(std::option::IntoIter<&'a mut IdSpan<T>>),
        }
        impl<'a, T: PolarPrimitive> Iterator for Iter<'a, T> {
            type Item = AnyIdMut<'a, T>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Record(x) => x.next().map(AnyIdMut::Same),
                }
            }
        }
        match self {
            Self::Record(_, x) => Iter::Record(Some(x).into_iter()),
            Self::Void | Self::Bool | Self::Int { .. } | Self::Float { .. } => Iter::Default,
        }
    }
}

impl<T: PolarPrimitive> PolarType<T> {
    pub fn ids(&self) -> impl '_ + Iterator<Item = AnyIdRef<T>> {
        enum Iter<'a, T: PolarPrimitive, I: Iterator<Item = AnyIdRef<'a, T>>> {
            Var(std::option::IntoIter<&'a VarId>),
            Func(std::array::IntoIter<AnyIdRef<'a, T>, 2>),
            Prim(I),
        }
        impl<'a, T: PolarPrimitive, I: Iterator<Item = AnyIdRef<'a, T>>> Iterator for Iter<'a, T, I> {
            type Item = AnyIdRef<'a, T>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Var(x) => x.size_hint(),
                    Self::Func(x) => x.size_hint(),
                    Self::Prim(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Var(x) => x.next().map(AnyIdRef::Var),
                    Self::Func(x) => x.next(),
                    Self::Prim(x) => x.next(),
                }
            }
        }
        match self {
            Self::Prim(x) => Iter::Prim(x.ids()),
            Self::Var(v) => Iter::Var(Some(v).into_iter()),
            Self::Func(a, b) => Iter::Func([AnyIdRef::Inverse(a), AnyIdRef::Same(b)].into_iter()),
        }
    }
    pub fn ids_mut(&mut self) -> impl '_ + Iterator<Item = AnyIdMut<T>> {
        enum Iter<'a, T: PolarPrimitive, I: Iterator<Item = AnyIdMut<'a, T>>> {
            Var(std::option::IntoIter<&'a mut VarId>),
            Func(std::array::IntoIter<AnyIdMut<'a, T>, 2>),
            Prim(I),
        }
        impl<'a, T: PolarPrimitive, I: Iterator<Item = AnyIdMut<'a, T>>> Iterator for Iter<'a, T, I> {
            type Item = AnyIdMut<'a, T>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Var(x) => x.size_hint(),
                    Self::Func(x) => x.size_hint(),
                    Self::Prim(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Var(x) => x.next().map(AnyIdMut::Var),
                    Self::Func(x) => x.next(),
                    Self::Prim(x) => x.next(),
                }
            }
        }
        match self {
            Self::Prim(x) => Iter::Prim(x.ids_mut()),
            Self::Var(v) => Iter::Var(Some(v).into_iter()),
            Self::Func(a, b) => Iter::Func([AnyIdMut::Inverse(a), AnyIdMut::Same(b)].into_iter()),
        }
    }
}
