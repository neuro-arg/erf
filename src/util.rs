use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use crate::Span;

pub struct Id<T>(usize, PhantomData<T>);
impl<T> Id<T> {
    pub fn new(id: usize) -> Self {
        Self(id, PhantomData)
    }
    pub fn id(&self) -> usize {
        self.0
    }
}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Id<T> {}
impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T> Eq for Id<T> {}
impl<T> Hash for Id<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl<T> Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Id").field(&self.0).finish()
    }
}

pub struct IdSpan<T>(Id<T>, Span);
impl<T> IdSpan<T> {
    pub fn new(id: Id<T>, span: Span) -> Self {
        Self(id, span)
    }
    pub fn id(&self) -> Id<T> {
        self.0
    }
    pub fn span(&self) -> Span {
        self.1
    }
}
impl<T> From<(Id<T>, Span)> for IdSpan<T> {
    fn from((a, b): (Id<T>, Span)) -> Self {
        Self::new(a, b)
    }
}

impl<T> Clone for IdSpan<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for IdSpan<T> {}
impl<T> Ord for IdSpan<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.0.cmp(&other.0) {
            std::cmp::Ordering::Equal => self.1.cmp(&other.1),
            x => x,
        }
    }
}
impl<T> PartialOrd for IdSpan<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<T> PartialEq for IdSpan<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0) && self.1.eq(&other.1)
    }
}
impl<T> Eq for IdSpan<T> {}
impl<T> Hash for IdSpan<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}
impl<T> Debug for IdSpan<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("IdSpan")
            .field(&self.0)
            .field(&self.1)
            .finish()
    }
}

pub enum IterEither<A, B> {
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
