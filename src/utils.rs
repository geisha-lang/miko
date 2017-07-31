use std::collections::HashMap;

pub type Name = String;
pub type P<T> = Box<T>;
/// Shorter alias of Box
pub fn P<T>(t: T) -> Box<T> {
    Box::new(t)
}

#[derive(Clone, PartialEq, Debug)]
pub struct SymTable<'a, T: 'a> {
    vars: HashMap<&'a str, T>,
    parent: Option<&'a SymTable<'a, T>>,
}

impl<'a: 'b, 'b, T: 'a> SymTable<'a, T> {
    pub fn new() -> SymTable<'a, T> {
        SymTable {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn from_iter<I>(it: I) -> SymTable<'a, T>
        where I: IntoIterator<Item = (&'a str, T)>
    {
        let mut env = SymTable::new();
        env.vars.extend(it);
        env
    }

    fn sub_env(&'a self) -> SymTable<'b, T> {
        let mut it = SymTable::new();
        it.parent = Some(self);
        it
    }

    pub fn exist(&self, k: &str) -> bool {
        self.vars.contains_key(k)
    }

    pub fn extend(&'a self, name: &'b str, ty: T) -> SymTable<'b, T> {
        let mut it = self.sub_env();
        it.vars.insert(name, ty);
        return it;
    }

    pub fn extend_n<I>(&'a self, terms: I) -> SymTable<'b, T>
        where I: IntoIterator<Item = (&'b str, T)>
    {

        let mut it = self.sub_env();
        it.vars.extend(terms);
        return it;
    }

    pub fn lookup(&self, name: &'a str) -> Option<&T> {
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
}
