use std::string::*;

use std::ops::Deref;
use std::ops::DerefMut;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

use std::collections::HashMap;

use utils::*;


/// Represents a variant of `data` type
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Variant {
    pub name: Name,
    pub body: VariantBody,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum VariantBody {
    Struct(Vec<Field>),
    Tuple(Vec<Field>),
    Unit,
}

/// A field definition of struct in `data`
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Field {
//    pub pos: Pos,
    pub name: Option<Name>,
    pub ty: P<Type>,
}


/// Type scheme
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Scheme {
    /// A monomorphism type
    /// `Int * Int -> Double`
    Mono(Type),

    /// Polymorphism type
    /// `forall a. a * a -> a`
    Poly(Vec<Name>, Type),

    /// Unknown type
    Slot,
}

impl Scheme {
    pub fn slot() -> Scheme {
        Scheme::Slot
    }

    pub fn con<S: ToString>(name: S) -> Scheme {
        Scheme::Mono(Type::Con(name.to_string()))
    }

    pub fn body(&self) -> &Type {
        match *self {
            Scheme::Mono(ref t) |
            Scheme::Poly(_, ref t) => t,
            Scheme::Slot => unreachable!(),
        }
    }

    pub fn arrow(from: Type, to: Type) -> Scheme {
        Scheme::Mono(Type::Arr(P(from), P(to)))
    }
    pub fn is_fn(&self) -> bool {
        match self {
            &Scheme::Mono(Type::Arr(..)) |
            &Scheme::Poly(_, Type::Arr(..)) => true,
            _ => false
        }
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


pub type TypeEnv<'a> = SymTable<'a, String, Scheme>;

