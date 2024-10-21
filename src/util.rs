use std::{
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use crate::Span;

#[derive(Clone, Debug)]
pub struct OrderedSet<T> {
    added: HashMap<T, usize>,
    vals: Vec<T>,
}

impl<T> Default for OrderedSet<T> {
    fn default() -> Self {
        Self::new()
    }
}
impl<T> OrderedSet<T> {
    pub fn new() -> Self {
        Self {
            added: HashMap::default(),
            vals: vec![],
        }
    }
}
impl<T: Clone + Hash + Eq> OrderedSet<T> {
    pub fn insert(&mut self, val: T) -> (bool, usize) {
        let x = *self.added.entry(val.clone()).or_insert_with(|| {
            self.vals.push(val);
            self.vals.len() - 1
        });
        (x == self.vals.len() - 1, x)
    }
}

impl<'a, T> IntoIterator for &'a OrderedSet<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.vals.iter()
    }
}

impl<T> Index<usize> for OrderedSet<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.vals[index]
    }
}

impl<T> IndexMut<usize> for OrderedSet<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.vals[index]
    }
}

#[derive(Clone, Debug)]
pub struct OrderedMap<T, Y> {
    added: HashMap<T, usize>,
    vals: Vec<(T, Y)>,
}

impl<T, Y> Default for OrderedMap<T, Y> {
    fn default() -> Self {
        Self {
            added: HashMap::default(),
            vals: Vec::default(),
        }
    }
}

impl<T: Clone + Eq + Hash, Y> OrderedMap<T, Y> {
    pub fn insert(&mut self, key: T, val: Y) -> (bool, usize) {
        let x = *self
            .added
            .entry(key.clone())
            .or_insert_with(|| self.vals.len());
        if x == self.vals.len() {
            self.vals.push((key, val));
        } else {
            self.vals[x].1 = val;
        }
        (x == self.vals.len() - 1, x)
    }
}

impl<T, Y> IntoIterator for OrderedMap<T, Y> {
    type Item = (T, Y);
    type IntoIter = std::vec::IntoIter<(T, Y)>;
    fn into_iter(self) -> Self::IntoIter {
        self.vals.into_iter()
    }
}

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

#[derive(Copy, Clone, Debug)]
pub enum Either3<T, U, V> {
    A(T),
    B(U),
    C(V),
}
