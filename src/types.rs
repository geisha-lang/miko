use std::string::*;
use utils::*;

use std::ops::Deref;
use std::ops::DerefMut;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

/// Type scheme
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Scheme {
    /// A monomorphism type
    /// `Int * Int -> Double`
    Mono(P<Type>),

    /// Polymorphism type
    /// `forall a. a * a -> a`
    Poly(Vec<Name>, P<Type>),

    /// Unknown type
    Slot,
}

impl Scheme {
    pub fn slot() -> Scheme {
        Scheme::Slot
    }

    pub fn con<S: ToString>(name: S) -> Scheme {
        Scheme::Mono(P(Type::Con(name.to_string())))
    }

    pub fn body(&self) -> &Type {
        match *self {
            Scheme::Mono(ref t) |
            Scheme::Poly(_, ref t) => (*t).deref(),
            Scheme::Slot => unreachable!(),
        }
    }

    pub fn arrow(from: Type, to: Type) -> Scheme {
        Scheme::Mono(P(Type::Arr(P(from), P(to))))
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Void,
    /// Type variable
    Var(Name),
    /// Constant type name
    Con(Name),
    /// Arrow (->) type
    Arr(P<Type>, P<Type>),
    /// Product (*) type
    Prod(P<Type>, P<Type>),
    /// Composite type
    Comp(P<Type>, P<Type>),
}

impl Type {
    pub fn product(left: Type, right: Type) -> Type {
        Type::Prod(P(left), P(right))
        // Type::Comp(P(Type::Con("->".to_string())))
    }
    pub fn compose(callee: Type, arg: Type) -> Type {
        Type::Comp(P(callee), P(arg))
    }

    pub fn product_n<'a, I>(ts: I) -> Type
        where I: IntoIterator<Item = Type>,
              <I as IntoIterator>::IntoIter: DoubleEndedIterator
    {

        let mut it = ts.into_iter().rev();
        let last = it.next().unwrap();
        let mut res = last;
        for ty in it {
            res = Type::Prod(P(ty), P(res));
        }
        res
    }
}