use std::collections::{HashMap, HashSet};
use std::ops::DerefMut;

use crate::syntax::form::*;
use crate::internal::*;
use crate::utils::*;
use crate::types::*;

pub type Subst = HashMap<Name, Type>;

pub trait Substituable {
    fn apply(self, sub: &Subst) -> Self;
    fn ftv(&self) -> HashSet<Name>;
}

pub trait SubstMut {
    fn apply_mut(&mut self, sub: &Subst);
}

impl SubstMut for Subst {
    fn apply_mut(&mut self, sub: &Subst) {
        for target in self.values_mut() {
            *target = target.clone().apply(sub);
        }
    }
}

impl Substituable for Type {
    fn apply(self, sub: &Subst) -> Self {
        use self::Type::*;
        match self {
            Arr(params, ret) => Arr(
                params.into_iter().map(|p| p.apply(sub)).collect(),
                P((*ret).apply(sub)),
            ),
            Tuple(elems) => Tuple(elems.into_iter().map(|e| e.apply(sub)).collect()),
            Comp(left, right) => Comp(P((*left).apply(sub)), P((*right).apply(sub))),
            Var(n) => {
                if let Some(t) = sub.get(&n) {
                    t.clone()
                } else {
                    Var(n)
                }
            }
            t => t,
        }
    }

    fn ftv(&self) -> HashSet<Name> {
        use self::Type::*;
        match self {
            Var(n) => {
                let mut it = HashSet::new();
                it.insert(n.clone());
                it
            }
            Arr(params, ret) => {
                let mut it = ret.ftv();
                for p in params {
                    it.extend(p.ftv());
                }
                it
            }
            Tuple(elems) => {
                let mut it = HashSet::new();
                for e in elems {
                    it.extend(e.ftv());
                }
                it
            }
            Comp(left, right) => {
                let mut it = left.ftv();
                it.extend(right.ftv());
                it
            }
            Void | Con(_) => HashSet::new(),
        }
    }
}

impl Substituable for Scheme {
    fn apply(self, sub: &Subst) -> Self {
        use self::Scheme::*;
        match self {
            Mono(ty) => Mono(ty.apply(sub)),
            Poly(bounds, ty) => {
                let mut s_ = sub.clone();
                for n in &bounds {
                    s_.remove(n);
                }
                Poly(bounds, ty.apply(&s_))
            }
            PolyConstrained(bounds, constraints, ty) => {
                let mut s_ = sub.clone();
                for n in &bounds {
                    s_.remove(n);
                }
                PolyConstrained(bounds, constraints, ty.apply(&s_))
            }
            Slot => Slot,
        }
    }

    fn ftv(&self) -> HashSet<Name> {
        use self::Scheme::*;
        match self {
            Mono(ty) => ty.ftv(),
            Poly(bounds, ty) | PolyConstrained(bounds, _, ty) => {
                let mut res = ty.ftv();
                for n in bounds {
                    res.remove(n);
                }
                res
            }
            Slot => HashSet::new(),
        }
    }
}

impl SubstMut for Scheme {
    fn apply_mut(&mut self, sub: &Subst) {
        use self::Scheme::*;
        match self {
            Mono(ref mut ty) => *ty = ty.clone().apply(sub),
            Poly(ref bounds, ref mut ty) | PolyConstrained(ref bounds, _, ref mut ty) => {
                let mut filtered = sub.clone();
                for b in bounds {
                    filtered.remove(b);
                }
                *ty = ty.clone().apply(&filtered);
            }
            Slot => {}
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
            Tuple(ref mut es) | List(ref mut es) | Block(ref mut es) => {
                es.apply_mut(sub);
            }
            StructInit(_, ref mut fields) => {
                for (_, val) in fields.iter_mut() {
                    val.apply_mut(sub);
                }
            }
            Apply(ref mut callee, ref mut args) => {
                callee.apply_mut(sub);
                args.apply_mut(sub);
            }
            Abs(ref mut fun) => {
                fun.param.apply_mut(sub);
                if let Some(ref mut rt) = fun.ret_ty {
                    *rt = rt.clone().apply(sub);
                }
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
            While(ref mut cond, ref mut body) => {
                cond.apply_mut(sub);
                body.apply_mut(sub);
            }
            Match(ref mut scrutinee, ref mut arms) => {
                scrutinee.apply_mut(sub);
                for arm in arms.iter_mut() {
                    if let Some(ref mut guard) = arm.guard {
                        guard.apply_mut(sub);
                    }
                    arm.body.apply_mut(sub);
                }
            }
            Lit(_) | Var(_) | QualifiedVar(_) => {}
        }
    }
}

impl SubstMut for VarDecl {
    fn apply_mut(&mut self, sub: &Subst) {
        self.1.apply_mut(sub);
    }
}
