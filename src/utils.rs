use std::collections::HashMap;

pub type Name = String;
pub type P<T> = Box<T>;
/// Shorter alias of Box
pub fn P<T>(t: T) -> Box<T> {
    Box::new(t)
}

#[derive(Clone, PartialEq, Debug)]
pub struct SymTable<'a, K, T: 'a>
    where K: 'a + ::std::cmp::Eq + ::std::hash::Hash {
    vars: HashMap<K, T>,
    parent: Option<&'a SymTable<'a, K, T>>,
}

impl<'a: 'b, 'b, K:'a, T: 'a> SymTable<'a, K, T>
    where K: 'a + ::std::cmp::Eq + ::std::hash::Hash {
    pub fn new() -> SymTable<'a, K, T> {
        SymTable {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn from_iter<I>(it: I) -> SymTable<'a, K, T>
        where I: IntoIterator<Item = (K, T)>
    {
        let mut env = SymTable::new();
        env.vars.extend(it);
        env
    }

    pub fn sub_env(&'a self) -> SymTable<'b, K, T> {
        let mut it = SymTable::new();
        it.parent = Some(self);
        it
    }

    pub fn exist(&self, k: &K) -> bool {
        self.vars.contains_key(k)
    }

    pub fn extend(&'a self, name: K, ty: T) -> SymTable<'b, K, T> {
        let mut it = self.sub_env();
        it.vars.insert(name, ty);
        return it;
    }

    pub fn extend_n<I>(&'a self, terms: I) -> SymTable<'b, K, T>
        where I: IntoIterator<Item = (K, T)>
    {

        let mut it = self.sub_env();
        it.vars.extend(terms);
        return it;
    }
    pub fn remove(&mut self, k: &K) -> Option<T> {
        self.vars.remove(k)
    }
    pub fn insert(&mut self, k: K, v: T) -> Option<T> {
        self.vars.insert(k, v)
    }

    pub fn lookup(&self, name: &K) -> Option<&T> {
        match self.vars.get(name) {
            None => {
                if let Some(p) = self.parent {
                    p.lookup(name)
                } else {
                    None
                }
            }
            r => r,

        }
    }

    pub fn unwrap(self) -> HashMap<K, T> {
        self.vars
    }
}
