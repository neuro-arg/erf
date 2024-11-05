//! This module has the most boring code I ever wrote in my life
use std::hash::Hash;

use indexmap::IndexSet;

use crate::{typeck::LabelId, util::IdSpan, Span};

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
        enum Iter<'a> {
            Default,
            Single(std::option::IntoIter<&'a IdSpan<PosPrim>>),
            Record(std::collections::btree_map::Values<'a, String, IdSpan<PosPrim>>),
        }
        impl<'a> Iterator for Iter<'a> {
            type Item = AnyIdRef<'a, PosPrim>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Single(x) => x.size_hint(),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Single(x) => x.next().map(AnyIdRef::Same),
                    Self::Record(x) => x.next().map(AnyIdRef::Same),
                }
            }
        }
        match self {
            Self::Label(_, id) => Iter::Single(Some(id).into_iter()),
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
        enum Iter<'a> {
            Default,
            Single(std::option::IntoIter<&'a mut IdSpan<PosPrim>>),
            Record(std::collections::btree_map::ValuesMut<'a, String, IdSpan<PosPrim>>),
        }
        impl<'a> Iterator for Iter<'a> {
            type Item = AnyIdMut<'a, PosPrim>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Single(x) => x.size_hint(),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Single(x) => x.next().map(AnyIdMut::Same),
                    Self::Record(x) => x.next().map(AnyIdMut::Same),
                }
            }
        }
        match self {
            Self::Label(_, id) => Iter::Single(Some(id).into_iter()),
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
    fn var_data_mut(var: &mut VarState) -> &mut IndexSet<IdSpan<Self>> {
        &mut var.inter
    }
    fn var_data(var: &VarState) -> &IndexSet<IdSpan<Self>> {
        &var.inter
    }
    fn ids(&self) -> impl '_ + Iterator<Item = AnyIdRef<Self>> {
        enum Iter<'a, F: FnMut(&'a (IdSpan<NegPrim>, bool)) -> &'a IdSpan<NegPrim>> {
            Default,
            Label(
                std::iter::Chain<
                    std::iter::Map<
                        std::collections::btree_map::Values<'a, LabelId, (IdSpan<NegPrim>, bool)>,
                        F,
                    >,
                    std::option::IntoIter<&'a IdSpan<NegPrim>>,
                >,
            ),
            Record(std::option::IntoIter<&'a IdSpan<NegPrim>>),
        }
        impl<'a, F: FnMut(&'a (IdSpan<NegPrim>, bool)) -> &'a IdSpan<NegPrim>> Iterator for Iter<'a, F> {
            type Item = AnyIdRef<'a, NegPrim>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Label(x) => x.size_hint(),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Label(x) => x.next().map(AnyIdRef::Same),
                    Self::Record(x) => x.next().map(AnyIdRef::Same),
                }
            }
        }
        match self {
            Self::Label { cases, fallthrough } => Iter::Label(
                cases
                    .values()
                    .map({
                        fn fst(x: &(IdSpan<NegPrim>, bool)) -> &IdSpan<NegPrim> {
                            &x.0
                        }
                        fst
                    })
                    .chain(fallthrough.as_ref()),
            ),
            Self::Record(_, x) => Iter::Record(Some(x).into_iter()),
            Self::Void | Self::Bool | Self::Int { .. } | Self::Float { .. } => Iter::Default,
        }
    }
    fn ids_mut(&mut self) -> impl '_ + Iterator<Item = AnyIdMut<Self>> {
        enum Iter<'a, F: FnMut(&'a mut (IdSpan<NegPrim>, bool)) -> &'a mut IdSpan<NegPrim>> {
            Default,
            Label(
                std::iter::Chain<
                    std::iter::Map<
                        std::collections::btree_map::ValuesMut<
                            'a,
                            LabelId,
                            (IdSpan<NegPrim>, bool),
                        >,
                        F,
                    >,
                    std::option::IntoIter<&'a mut IdSpan<NegPrim>>,
                >,
            ),
            Record(std::option::IntoIter<&'a mut IdSpan<NegPrim>>),
        }
        impl<'a, F: FnMut(&'a mut (IdSpan<NegPrim>, bool)) -> &'a mut IdSpan<NegPrim>> Iterator
            for Iter<'a, F>
        {
            type Item = AnyIdMut<'a, NegPrim>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Label(x) => x.size_hint(),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Label(x) => x.next().map(AnyIdMut::Same),
                    Self::Record(x) => x.next().map(AnyIdMut::Same),
                }
            }
        }
        match self {
            Self::Label { cases, fallthrough } => Iter::Label(
                cases
                    .values_mut()
                    .map({
                        fn fst(x: &mut (IdSpan<NegPrim>, bool)) -> &mut IdSpan<NegPrim> {
                            &mut x.0
                        }
                        fst
                    })
                    .chain(fallthrough.as_mut()),
            ),
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
