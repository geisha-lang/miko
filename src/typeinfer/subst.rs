use std::collections::HashSet;
use std::collections::HashMap;

use std::ops::DerefMut;

use syntax::form::*;
use internal::*;
use utils::*;
use types::*;

pub type Subst = HashMap<Name, Type>;
pub trait Substituable {
    fn apply(self, sub: &Subst) -> Self;
    // fn apply_mut(&mut self, sub: &Subst);
    fn ftv(&self) -> HashSet<Name>;
}

pub trait SubstMut {
    fn apply_mut(&mut self, sub: &Subst);
}

impl SubstMut for Subst {
    fn apply_mut(&mut self, sub: &Subst) {
        for target in self.values_mut() {
            *target = target.to_owned().apply(sub);
        }
    }
}

impl Substituable for Type {
    fn apply(self, sub: &Subst) -> Self {
        use self::Type::*;
        match self {
            Arr(left, right) => Arr(P((*left).apply(sub)), P((*right).apply(sub))),
            Prod(left, right) => Prod(P((*left).apply(sub)), P((*right).apply(sub))),
            Comp(left, right) => Comp(P((*left).apply(sub)), P((*right).apply(sub))),
            Var(n) => {
                if let Some(ref t) = sub.get(&n) {
                    (*t).clone()
                } else {
                    Var(n)
                }
            }
            t => t,
        }
    }
    fn ftv(&self) -> HashSet<Name> {
        use self::Type::*;
        match *self {
            Var(ref n) => {
                let mut it = HashSet::new();
                it.insert(n.clone());
                it
            }
            Arr(ref left, ref right) |
            Comp(ref left, ref right) |
            Prod(ref left, ref right) => {
                let mut it = left.ftv();
                it.extend(right.ftv());
                it
            }
            _ => HashSet::new(),
        }
    }
}


impl Substituable for Scheme {
    // fn apply_mut(&mut self, sub: &Subst) {}
    fn apply(self, sub: &Subst) -> Self {
        use self::Scheme::*;
        match self {
            Mono(ty) => Mono(ty.apply(sub)),
            Poly(bounds, ty) => {
                let mut s_ = sub.clone();
                for ref n in &bounds {
                    s_.remove(n.clone());
                }
                Poly(bounds, ty.apply(&s_))
            }
            t => t,
        }
    }

    fn ftv(&self) -> HashSet<Name> {
        use self::Scheme::*;
        match *self {
            Mono(ref ty) => (*ty).ftv(),
            Poly(ref bounds, ref ty) => {
                let mut res = ty.ftv();
                for ref n in bounds {
                    res.remove(n.clone());
                }
                return res;
            }
            Slot => HashSet::new(),
        }
    }
}

impl SubstMut for Scheme {
    fn apply_mut(&mut self, sub: &Subst) {
        use self::Scheme::*;
        match self {
            &mut Mono(ref mut ty) |
            &mut Poly(_, ref mut ty) => *ty = ty.clone().apply(sub),
            _ => {}
        }
    }
}

impl<T: SubstMut> SubstMut for Vec<T> {
    fn apply_mut(&mut self, sub: &Subst) {
        for v in self.iter_mut() {
            v.apply_mut(sub);
        }
    }
}

impl<T: SubstMut> SubstMut for Box<T> {
    fn apply_mut(&mut self, sub: &Subst) {
        self.deref_mut().apply_mut(sub);
    }
}

impl Substituable for Form {
    // fn apply_mut(&mut self, sub: &Subst) {}
    fn apply(mut self, sub: &Subst) -> Self {
        self.tag.ty = self.tag.ty.apply(sub);
        self
    }
    fn ftv(&self) -> HashSet<Name> {
        self.tag.ty.ftv()
    }
}

impl SubstMut for Form {
    fn apply_mut(&mut self, sub: &Subst) {
        use self::Expr::*;
        self.tag.ty.apply_mut(sub);
        match self.node {
            List(ref mut es) |
            Block(ref mut es) => {
                es.apply_mut(sub);
            }
            Apply(ref mut callee, ref mut args) => {
                callee.apply_mut(sub);
                args.apply_mut(sub);
            }
            Abs(ref mut fun) => {
                fun.param.apply_mut(sub);
                fun.body.apply_mut(sub);
            }
            Binary(_, ref mut l, ref mut r) => {
                l.apply_mut(sub);
                r.apply_mut(sub);
            }
            Unary(_, ref mut f) => {
                f.apply_mut(sub);
            }
            Let(ref mut var, ref mut val, ref mut exp) => {
                var.apply_mut(sub);
                val.apply_mut(sub);
                exp.apply_mut(sub);
            }
            If(ref mut cond, ref mut tr, ref mut fl) => {
                cond.apply_mut(sub);
                tr.apply_mut(sub);
                fl.apply_mut(sub);
            }
            // Lit(Lit),
            // Var(Name),
            _ => {}
        }
    }
}

impl SubstMut for VarDecl {
    fn apply_mut(&mut self, sub: &Subst) {
        self.1.apply_mut(sub);
    }
}
