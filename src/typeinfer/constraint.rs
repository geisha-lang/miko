use std::collections::{HashMap, HashSet};

use crate::types::*;
use crate::syntax::form::Span;
use crate::utils::*;

use super::subst::*;
use super::error::*;

#[derive(Clone, PartialEq, Debug)]
pub struct Constraint(pub Type, pub Type, pub Span);

fn bind_var(n: Name, t: Type, span: Span) -> Result<Subst, TypeError> {
    if let Type::Var(ref m) = t {
        if &n == m {
            return Ok(HashMap::new());
        }
        // Prefer keeping user-written rigid type variable names (e.g. `k`, `a`) over generated `.N` temporaries!
        if !n.starts_with('.') && m.starts_with('.') {
            let mut s = HashMap::new();
            s.insert(m.clone(), Type::Var(n));
            return Ok(s);
        }
    }
    if t.ftv().contains(&n) {
        return Err(TypeError::InfiniteType(n, t, span));
    }
    let mut s = HashMap::new();
    s.insert(n, t);
    Ok(s)
}

fn unify_types(lhs: Type, rhs: Type, span: Span) -> Result<Subst, TypeError> {
    use self::Type::*;
    match (lhs, rhs) {
        (Var(n), t) => bind_var(n, t, span),
        (t, Var(n)) => bind_var(n, t, span),
        (Arr(ps1, r1), Arr(ps2, r2)) => {
            if ps1.len() != ps2.len() {
                return Err(TypeError::ArityMismatch {
                    expected: ps1.len(),
                    actual: ps2.len(),
                    span,
                });
            }
            let mut sub = HashMap::new();
            for (p1, p2) in ps1.into_iter().zip(ps2.into_iter()) {
                let u = unify_types(p1.apply(&sub), p2.apply(&sub), span)?;
                sub.apply_mut(&u);
                sub.extend(u);
            }
            let ur = unify_types((*r1).apply(&sub), (*r2).apply(&sub), span)?;
            sub.apply_mut(&ur);
            sub.extend(ur);
            Ok(sub)
        }
        (Tuple(es1), Tuple(es2)) => {
            if es1.len() != es2.len() {
                return Err(TypeError::MisMatch(Tuple(es1), Tuple(es2), span));
            }
            let mut sub = HashMap::new();
            for (e1, e2) in es1.into_iter().zip(es2.into_iter()) {
                let u = unify_types(e1.apply(&sub), e2.apply(&sub), span)?;
                sub.apply_mut(&u);
                sub.extend(u);
            }
            Ok(sub)
        }
        (Comp(l1, l2), Comp(r1, r2)) => {
            let mut u1 = unify_types(*l1, *r1, span)?;
            let u2 = unify_types((*l2).apply(&u1), (*r2).apply(&u1), span)?;
            u1.apply_mut(&u2);
            u1.extend(u2);
            Ok(u1)
        }
        (el, er) => {
            if el == er {
                Ok(HashMap::new())
            } else {
                Err(TypeError::MisMatch(el, er, span))
            }
        }
    }
}

impl Constraint {
    pub fn unify(self) -> Result<Subst, TypeError> {
        unify_types(self.0, self.1, self.2)
    }
}

impl Substituable for Constraint {
    fn apply(self, sub: &Subst) -> Self {
        Constraint(self.0.apply(sub), self.1.apply(sub), self.2)
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
