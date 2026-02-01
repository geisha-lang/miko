use std::string::*;

use std::ops::Deref;
use std::ops::DerefMut;

use std::iter::IntoIterator;
use std::iter::DoubleEndedIterator;

use std::collections::HashMap;

use crate::utils::*;


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


/// A type-level constraint for concepts (used in type schemes)
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SchemeConstraint {
    /// Name of the concept (e.g., "Eq")
    pub concept: Name,
    /// Type variable or type (e.g., "a" in "Eq a")
    pub type_arg: Name,
}

/// Type scheme
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Scheme {
    /// A monomorphism type
    /// `Int * Int -> Double`
    Mono(Type),

    /// Polymorphism type without constraints
    /// `forall a. a * a -> a`
    Poly(Vec<Name>, Type),

    /// Polymorphism type with constraints
    /// `forall (Eq a). a * a -> Bool`
    PolyConstrained(Vec<Name>, Vec<SchemeConstraint>, Type),

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
            Scheme::Poly(_, ref t) |
            Scheme::PolyConstrained(_, _, ref t) => t,
            Scheme::Slot => unreachable!(),
        }
    }

    pub fn arrow(from: Type, to: Type) -> Scheme {
        Scheme::Mono(Type::Arr(P(from), P(to)))
    }

    pub fn is_fn(&self) -> bool {
        match self {
            Scheme::Mono(Type::Arr(..)) |
            Scheme::Poly(_, Type::Arr(..)) |
            Scheme::PolyConstrained(_, _, Type::Arr(..)) => true,
            _ => false
        }
    }

    /// Get type parameters
    pub fn type_params(&self) -> &[Name] {
        match self {
            Scheme::Poly(vars, _) | Scheme::PolyConstrained(vars, _, _) => vars,
            _ => &[],
        }
    }

    /// Get constraints (if any)
    pub fn constraints(&self) -> &[SchemeConstraint] {
        match self {
            Scheme::PolyConstrained(_, constraints, _) => constraints,
            _ => &[],
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
    }
    pub fn compose(callee: Type, arg: Type) -> Type {
        Type::Comp(P(callee), P(arg))
    }

    pub fn product_n<I>(ts: I) -> Type
        where I: IntoIterator<Item = Type>,
              <I as IntoIterator>::IntoIter: DoubleEndedIterator
    {

        let mut it = ts.into_iter().rev();
        let last = it.next();
        match last {
            Some(last) => {
                let mut res = last;
                for ty in it {
                    res = Type::Prod(P(ty), P(res));
                }
                res
            }
            _ => panic!("Empty vec to product")
        }
    }
    pub fn compose_n<I>(ts: I) -> Type
        where I: IntoIterator<Item = Type>
    {

        let mut it = ts.into_iter();
        let last = it.next().unwrap();
        let mut res = last;
        for ty in it {
            res = Type::Comp(Box::new(res), Box::new(ty));
        }
        res
    }

    pub fn prod_to_vec(&self) -> Vec<&Type> {
        use self::Type::*;
        let mut v = vec![];
        let mut r: &Type = self;
        loop {
            if let Prod(t, rest) = r {
                v.push(t.as_ref());
                r = rest.as_ref();
            } else {
                v.push(r);
                break;
            }
        }
        v
    }
}

impl ToString for Type {
    fn to_string(&self) -> String {
        use self::Type::*;
        match self {
            Void => String::from("Void"),
            Var(n) | Con(n) => n.clone(),
            Arr(l, r) => l.to_string() + "->" + r.to_string().as_str(),
            Prod(l, r) => l.to_string() + "*" + r.to_string().as_str(),
            Comp(l, r) => l.to_string() + "+" + r.to_string().as_str(),
        }
    }
}


pub type TypeEnv<'a> = SymTable<'a, Id, Scheme>;

/// Information about an ADT (Algebraic Data Type)
#[derive(Clone, Debug)]
pub struct AdtInfo {
    /// Name of the ADT (e.g., "List", "Option")
    pub name: Name,
    /// Type parameters (e.g., ["a"] for List a)
    pub type_params: Vec<Name>,
    /// Variants with their constructor info
    pub variants: Vec<VariantInfo>,
}

/// Information about a single variant/constructor
#[derive(Clone, Debug)]
pub struct VariantInfo {
    /// Name of the constructor (e.g., "Cons", "Nil")
    pub name: Name,
    /// Tag value for runtime discrimination
    pub tag: usize,
    /// Field types (empty for unit variants)
    pub field_types: Vec<Type>,
    /// The constructor's type scheme
    pub scheme: Scheme,
}

impl AdtInfo {
    /// Build the result type for this ADT applied to its type parameters
    /// e.g., for `data List a`, returns `List a`
    pub fn result_type(&self) -> Type {
        if self.type_params.is_empty() {
            Type::Con(self.name.clone())
        } else {
            let base = Type::Con(self.name.clone());
            let params: Vec<Type> = self.type_params.iter()
                .map(|p| Type::Var(p.clone()))
                .collect();
            Type::compose_n(std::iter::once(base).chain(params))
        }
    }

    /// Generate constructor type schemes for all variants
    pub fn generate_constructors(&self) -> Vec<(Name, Scheme)> {
        self.variants.iter().map(|v| {
            (v.name.clone(), v.scheme.clone())
        }).collect()
    }

    /// Check if this ADT is recursive (self-referential)
    /// Recursive ADTs like List or Tree can grow unboundedly and must always be heap-allocated
    pub fn is_recursive(&self) -> bool {
        self.variants.iter().any(|v| {
            v.field_types.iter().any(|ft| type_mentions(&self.name, ft))
        })
    }
}

/// Check if a type mentions a given ADT name (used for recursive ADT detection)
pub fn type_mentions(adt_name: &str, ty: &Type) -> bool {
    match ty {
        Type::Con(name) => name == adt_name,
        Type::Comp(base, arg) => type_mentions(adt_name, base) || type_mentions(adt_name, arg),
        Type::Arr(p, r) | Type::Prod(p, r) => type_mentions(adt_name, p) || type_mentions(adt_name, r),
        Type::Var(_) | Type::Void => false,
    }
}

/// Registry of all ADT definitions
pub type AdtRegistry = HashMap<Name, AdtInfo>;

