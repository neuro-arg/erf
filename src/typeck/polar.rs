//! This module has the most boring code I ever wrote in my life
use std::hash::Hash;

use crate::{
    util::{Either3, IdSpan, OrderedSet},
    Span,
};

use super::{NegPrim, PolarType, PosPrim, TypeCk, VarId, VarState};

pub trait PolarPrimitive: Clone + Eq + Hash {
    type Inverse: PolarPrimitive<Inverse = Self>;
    const POSITIVE: bool;
    fn typeck_data_mut(ck: &mut TypeCk) -> (&mut Vec<PolarType<Self>>, &mut Vec<Span>);
    fn typeck_data(ck: &TypeCk) -> &Vec<PolarType<Self>>;
    fn var_data_mut(var: &mut VarState) -> &mut OrderedSet<IdSpan<Self>>;
    fn var_data(var: &VarState) -> &OrderedSet<IdSpan<Self>>;
    fn ids(
        &self,
    ) -> impl '_ + Iterator<Item = Either3<&IdSpan<Self>, &IdSpan<Self::Inverse>, &VarId>>;
    fn ids_mut(
        &mut self,
    ) -> impl '_ + Iterator<Item = Either3<&mut IdSpan<Self>, &mut IdSpan<Self::Inverse>, &mut VarId>>;
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
    fn var_data_mut(var: &mut VarState) -> &mut OrderedSet<IdSpan<Self>> {
        &mut var.union
    }
    fn var_data(var: &VarState) -> &OrderedSet<IdSpan<Self>> {
        &var.union
    }
    fn ids(
        &self,
    ) -> impl '_ + Iterator<Item = Either3<&IdSpan<Self>, &IdSpan<Self::Inverse>, &VarId>> {
        enum Iter<'a, T: PolarPrimitive> {
            Default,
            Record(std::collections::btree_map::Values<'a, String, IdSpan<T>>),
        }
        impl<'a, T: PolarPrimitive> Iterator for Iter<'a, T> {
            type Item = Either3<&'a IdSpan<T>, &'a IdSpan<T::Inverse>, &'a VarId>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Record(x) => x.next().map(Either3::A),
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
    fn ids_mut(
        &mut self,
    ) -> impl '_ + Iterator<Item = Either3<&mut IdSpan<Self>, &mut IdSpan<Self::Inverse>, &mut VarId>>
    {
        enum Iter<'a, T: PolarPrimitive> {
            Default,
            Record(std::collections::btree_map::ValuesMut<'a, String, IdSpan<T>>),
        }
        impl<'a, T: PolarPrimitive> Iterator for Iter<'a, T> {
            type Item = Either3<&'a mut IdSpan<T>, &'a mut IdSpan<T::Inverse>, &'a mut VarId>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Record(x) => x.next().map(Either3::A),
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
    fn var_data_mut(var: &mut VarState) -> &mut OrderedSet<IdSpan<Self>> {
        &mut var.inter
    }
    fn var_data(var: &VarState) -> &OrderedSet<IdSpan<Self>> {
        &var.inter
    }
    fn ids(
        &self,
    ) -> impl '_ + Iterator<Item = Either3<&IdSpan<Self>, &IdSpan<Self::Inverse>, &VarId>> {
        enum Iter<'a, T: PolarPrimitive> {
            Default,
            Record(std::option::IntoIter<&'a IdSpan<T>>),
        }
        impl<'a, T: PolarPrimitive> Iterator for Iter<'a, T> {
            type Item = Either3<&'a IdSpan<T>, &'a IdSpan<T::Inverse>, &'a VarId>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Record(x) => x.next().map(Either3::A),
                }
            }
        }
        match self {
            Self::Record(_, x) => Iter::Record(Some(x).into_iter()),
            Self::Void | Self::Bool | Self::Int { .. } | Self::Float { .. } => Iter::Default,
        }
    }
    fn ids_mut(
        &mut self,
    ) -> impl '_ + Iterator<Item = Either3<&mut IdSpan<Self>, &mut IdSpan<Self::Inverse>, &mut VarId>>
    {
        enum Iter<'a, T: PolarPrimitive> {
            Default,
            Record(std::option::IntoIter<&'a mut IdSpan<T>>),
        }
        impl<'a, T: PolarPrimitive> Iterator for Iter<'a, T> {
            type Item = Either3<&'a mut IdSpan<T>, &'a mut IdSpan<T::Inverse>, &'a mut VarId>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Default => (0, Some(0)),
                    Self::Record(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Default => None,
                    Self::Record(x) => x.next().map(Either3::A),
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
    pub fn ids(
        &self,
    ) -> impl '_ + Iterator<Item = Either3<&IdSpan<T>, &IdSpan<T::Inverse>, &VarId>> {
        enum Iter<
            'a,
            T: PolarPrimitive,
            I: Iterator<Item = Either3<&'a IdSpan<T>, &'a IdSpan<T::Inverse>, &'a VarId>>,
        > {
            Var(std::option::IntoIter<&'a VarId>),
            Func(
                std::array::IntoIter<Either3<&'a IdSpan<T>, &'a IdSpan<T::Inverse>, &'a VarId>, 2>,
            ),
            Prim(I),
        }
        impl<
                'a,
                T: PolarPrimitive,
                I: Iterator<Item = Either3<&'a IdSpan<T>, &'a IdSpan<T::Inverse>, &'a VarId>>,
            > Iterator for Iter<'a, T, I>
        {
            type Item = Either3<&'a IdSpan<T>, &'a IdSpan<T::Inverse>, &'a VarId>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Var(x) => x.size_hint(),
                    Self::Func(x) => x.size_hint(),
                    Self::Prim(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Var(x) => x.next().map(Either3::C),
                    Self::Func(x) => x.next(),
                    Self::Prim(x) => x.next(),
                }
            }
        }
        match self {
            Self::Prim(x) => Iter::Prim(x.ids()),
            Self::Var(v) => Iter::Var(Some(v).into_iter()),
            Self::Func(a, b) => Iter::Func([Either3::B(a), Either3::A(b)].into_iter()),
        }
    }
    pub fn ids_mut(
        &mut self,
    ) -> impl '_ + Iterator<Item = Either3<&mut IdSpan<T>, &mut IdSpan<T::Inverse>, &mut VarId>>
    {
        enum Iter<
            'a,
            T: PolarPrimitive,
            I: Iterator<Item = Either3<&'a mut IdSpan<T>, &'a mut IdSpan<T::Inverse>, &'a mut VarId>>,
        > {
            Var(std::option::IntoIter<&'a mut VarId>),
            Func(
                std::array::IntoIter<
                    Either3<&'a mut IdSpan<T>, &'a mut IdSpan<T::Inverse>, &'a mut VarId>,
                    2,
                >,
            ),
            Prim(I),
        }
        impl<
                'a,
                T: PolarPrimitive,
                I: Iterator<
                    Item = Either3<&'a mut IdSpan<T>, &'a mut IdSpan<T::Inverse>, &'a mut VarId>,
                >,
            > Iterator for Iter<'a, T, I>
        {
            type Item = Either3<&'a mut IdSpan<T>, &'a mut IdSpan<T::Inverse>, &'a mut VarId>;
            fn size_hint(&self) -> (usize, Option<usize>) {
                match self {
                    Self::Var(x) => x.size_hint(),
                    Self::Func(x) => x.size_hint(),
                    Self::Prim(x) => x.size_hint(),
                }
            }
            fn next(&mut self) -> Option<Self::Item> {
                match self {
                    Self::Var(x) => x.next().map(Either3::C),
                    Self::Func(x) => x.next(),
                    Self::Prim(x) => x.next(),
                }
            }
        }
        match self {
            Self::Prim(x) => Iter::Prim(x.ids_mut()),
            Self::Var(v) => Iter::Var(Some(v).into_iter()),
            Self::Func(a, b) => Iter::Func([Either3::B(a), Either3::A(b)].into_iter()),
        }
    }
}
