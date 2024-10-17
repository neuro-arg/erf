use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Index, IndexMut},
};

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
