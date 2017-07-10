
use std::collections::HashMap;

use utils::*;
use syntax::*;


#[derive(Clone, PartialEq, Debug)]
pub struct TypeEnv<'a> {
    vars: HashMap<&'a str, &'a Scheme>,
    parent: Option<&'a TypeEnv<'a>>,
}

impl<'a: 'b, 'b> TypeEnv<'a> {
    pub fn new() -> TypeEnv<'a> {
        TypeEnv {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn from_iter<T>(it: T) -> TypeEnv<'a>
        where T: IntoIterator<Item = (&'a str, &'a Scheme)>
    {
        let mut env = TypeEnv::new();
        env.vars.extend(it);
        env
    }

    fn sub_env(&'a self) -> TypeEnv<'b> {
        let mut it = TypeEnv::new();
        it.parent = Some(self);
        it
    }

    pub fn exist(&self, k: &str) -> bool {
        self.vars.contains_key(k)
    }

    pub fn extend(&'a self, name: &'b str, ty: &'b Scheme) -> TypeEnv<'b> {
        let mut it = self.sub_env();
        it.vars.insert(name, ty);
        return it;
    }

    pub fn extend_n<T>(&'a self, terms: T) -> TypeEnv<'b>
        where T: IntoIterator<Item = (&'b str, &'b Scheme)>
    {

        let mut it = self.sub_env();
        it.vars.extend(terms);
        return it;
    }

    pub fn lookup(&self, name: &'a str) -> Option<&&'a Scheme> {
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
