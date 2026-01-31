use std::collections::HashMap;
use std::collections::HashSet;

use crate::types::*;
use crate::utils::*;

use super::subst::*;
use super::error::*;

#[derive(Clone, PartialEq, Debug)]
pub struct Constraint(pub Type, pub Type);

impl Constraint {
    pub fn unify<'a>(self) -> Result<Subst, TypeError> {
        use self::Type::*;
        match self {
            Constraint(Var(n), t) |
            Constraint(t, Var(n)) => {
                let mut s = HashMap::new();
                s.insert(n, t);
                Ok(s)
            },
            Constraint(Arr(l1, l2), Arr(r1, r2)) |
            Constraint(Prod(l1, l2), Prod(r1, r2)) |
            Constraint(Comp(l1, l2), Comp(r1, r2)) => {
                let mut u1 = Constraint(*l1, *r1).unify()?;
                let u2 = Constraint(*l2, *r2).apply(&u1).unify()?;
                u1.apply_mut(&u2);
                u1.extend(u2);
                Ok(u1)
            },
            Constraint(el, er) => {
                if el == er {
                    Ok(HashMap::new())
                } else {
                    Err(TypeError::MisMatch(el, er))
                }
            }
        }
    }
}


impl Substituable for Constraint {
    fn apply(self, sub: &Subst) -> Self {
        Constraint(self.0.apply(sub), self.1.apply(sub))
    }
    fn ftv(&self) -> HashSet<Name> {
        let mut r = self.0.ftv();
        r.extend(self.1.ftv());
        r
    }
}

impl SubstMut for Constraint {
    fn apply_mut(&mut self, sub: &Subst) {
        self.0 = self.0.clone().apply(sub);
        self.1 = self.1.clone().apply(sub);
    }
}


