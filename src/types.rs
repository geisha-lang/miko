use std::collections::{HashMap, HashSet};
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

/// A field definition of struct or tuple variant in `data`
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Field {
    pub name: Option<Name>,
    pub ty: P<Type>,
}

/// A type-level constraint for concepts (used in type schemes)
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SchemeConstraint {
    /// Name of the concept (e.g., "Eq")
    pub concept: Name,
    /// Constrained type (usually Type::Var("a"), or applied type during solving)
    pub type_arg: Name,
}

/// Type scheme
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Scheme {
    /// A monomorphic type: `(Int, Int) -> Bool`
    Mono(Type),

    /// Polymorphic type without constraints: `forall a b. (a, b) -> a`
    Poly(Vec<Name>, Type),

    /// Polymorphic type with concept constraints: `forall a. (Eq a) => (a, a) -> Bool`
    PolyConstrained(Vec<Name>, Vec<SchemeConstraint>, Type),

    /// Unknown type placeholder before inference
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
            Scheme::Mono(ref t)
            | Scheme::Poly(_, ref t)
            | Scheme::PolyConstrained(_, _, ref t) => t,
            Scheme::Slot => panic!("Attempted to unwrap Scheme::Slot"),
        }
    }

    pub fn arrow(params: Vec<Type>, to: Type) -> Scheme {
        Scheme::Mono(Type::Arr(params, P(to)))
    }

    pub fn arrow1(from: Type, to: Type) -> Scheme {
        Scheme::Mono(Type::Arr(vec![from], P(to)))
    }

    pub fn is_fn(&self) -> bool {
        matches!(
            self,
            Scheme::Mono(Type::Arr(..))
                | Scheme::Poly(_, Type::Arr(..))
                | Scheme::PolyConstrained(_, _, Type::Arr(..))
        )
    }

    /// Get quantified type parameters
    pub fn type_params(&self) -> &[Name] {
        match self {
            Scheme::Poly(vars, _) | Scheme::PolyConstrained(vars, _, _) => vars,
            _ => &[],
        }
    }

    /// Get concept constraints (if any)
    pub fn constraints(&self) -> &[SchemeConstraint] {
        match self {
            Scheme::PolyConstrained(_, constraints, _) => constraints,
            _ => &[],
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    /// Unit type (`Void` / `()`)
    Void,
    /// Type variable (`a`, `.1`)
    Var(Name),
    /// Concrete type constructor (`Int`, `Bool`, `List`, `Maybe`)
    Con(Name),
    /// Function type with explicit parameter arity: `(T1, T2, ...) -> R`
    /// `vec![]` represents `() -> R`, `vec![A]` represents `A -> R`.
    Arr(Vec<Type>, P<Type>),
    /// First-class tuple type: `(T1, T2, ...)` (length >= 2)
    Tuple(Vec<Type>),
    /// Type constructor application: `List a`, `Either Int String`
    Comp(P<Type>, P<Type>),
}

impl Type {
    pub fn compose(callee: Type, arg: Type) -> Type {
        Type::Comp(P(callee), P(arg))
    }

    pub fn compose_n<I>(ts: I) -> Type
    where
        I: IntoIterator<Item = Type>,
    {
        let mut it = ts.into_iter();
        let first = it.next().expect("Empty iterator in Type::compose_n");
        let mut res = first;
        for ty in it {
            res = Type::Comp(Box::new(res), Box::new(ty));
        }
        res
    }

    /// Deterministic left-to-right free type variables (preserving first-seen order)
    pub fn ftv_ordered(&self) -> Vec<Name> {
        let mut out = Vec::new();
        let mut seen = HashSet::new();
        self.collect_ftv_ordered(&mut seen, &mut out);
        out
    }

    fn collect_ftv_ordered(&self, seen: &mut HashSet<Name>, out: &mut Vec<Name>) {
        match self {
            Type::Void | Type::Con(_) => {}
            Type::Var(n) => {
                if seen.insert(n.clone()) {
                    out.push(n.clone());
                }
            }
            Type::Arr(params, ret) => {
                for p in params {
                    p.collect_ftv_ordered(seen, out);
                }
                ret.collect_ftv_ordered(seen, out);
            }
            Type::Tuple(elems) => {
                for e in elems {
                    e.collect_ftv_ordered(seen, out);
                }
            }
            Type::Comp(l, r) => {
                l.collect_ftv_ordered(seen, out);
                r.collect_ftv_ordered(seen, out);
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Void => write!(f, "Void"),
            Type::Var(n) | Type::Con(n) => write!(f, "{}", n),
            Type::Arr(params, ret) => {
                if params.is_empty() {
                    write!(f, "() -> {}", ret)
                } else if params.len() == 1 {
                    match &params[0] {
                        Type::Arr(..) | Type::Tuple(..) => write!(f, "({}) -> {}", params[0], ret),
                        p => write!(f, "{} -> {}", p, ret),
                    }
                } else {
                    let ps: Vec<String> = params.iter().map(|p| p.to_string()).collect();
                    write!(f, "({}) -> {}", ps.join(", "), ret)
                }
            }
            Type::Tuple(elems) => {
                let es: Vec<String> = elems.iter().map(|e| e.to_string()).collect();
                write!(f, "({})", es.join(", "))
            }
            Type::Comp(l, r) => match r.as_ref() {
                Type::Comp(..) | Type::Arr(..) => write!(f, "{} ({})", l, r),
                _ => write!(f, "{} {}", l, r),
            },
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
    /// Optional field names (populated for struct variants)
    pub field_names: Option<Vec<Name>>,
    /// Field types (empty for unit variants)
    pub field_types: Vec<Type>,
    /// The constructor's type scheme
    pub scheme: Scheme,
}

impl AdtInfo {
    /// Build the result type for this ADT applied to its type parameters
    pub fn result_type(&self) -> Type {
        if self.type_params.is_empty() {
            Type::Con(self.name.clone())
        } else {
            let base = Type::Con(self.name.clone());
            let params: Vec<Type> = self
                .type_params
                .iter()
                .map(|p| Type::Var(p.clone()))
                .collect();
            Type::compose_n(std::iter::once(base).chain(params))
        }
    }

    /// Generate constructor type schemes for all variants
    pub fn generate_constructors(&self) -> Vec<(Name, Scheme)> {
        self.variants
            .iter()
            .map(|v| (v.name.clone(), v.scheme.clone()))
            .collect()
    }

    /// Direct self-reference check; see `compute_recursive_adts` for full mutual-recursion detection.
    pub fn is_recursive(&self) -> bool {
        self.variants
            .iter()
            .any(|v| v.field_types.iter().any(|ft| type_mentions(&self.name, ft)))
    }
}

/// Collect all ADT constructor names referenced inside a type
pub fn collect_mentioned_types(ty: &Type, out: &mut HashSet<Name>) {
    match ty {
        Type::Con(name) => {
            out.insert(name.clone());
        }
        Type::Comp(base, arg) => {
            collect_mentioned_types(base, out);
            collect_mentioned_types(arg, out);
        }
        Type::Arr(params, ret) => {
            for p in params {
                collect_mentioned_types(p, out);
            }
            collect_mentioned_types(ret, out);
        }
        Type::Tuple(elems) => {
            for e in elems {
                collect_mentioned_types(e, out);
            }
        }
        Type::Var(_) | Type::Void => {}
    }
}

/// Check if a type directly mentions a given ADT name
pub fn type_mentions(adt_name: &str, ty: &Type) -> bool {
    let mut mentioned = HashSet::new();
    collect_mentioned_types(ty, &mut mentioned);
    mentioned.contains(adt_name)
}

/// Compute all self-recursive and mutually-recursive ADTs via graph reachability.
/// An ADT `A` is recursive iff `A` can reach `A` in one or more field-type edges.
pub fn compute_recursive_adts(registry: &AdtRegistry) -> HashSet<Name> {
    let mut edges: HashMap<Name, HashSet<Name>> = HashMap::new();
    for (name, info) in registry {
        let mut targets = HashSet::new();
        for variant in &info.variants {
            for ft in &variant.field_types {
                let mut mentioned = HashSet::new();
                collect_mentioned_types(ft, &mut mentioned);
                for m in mentioned {
                    if registry.contains_key(&m) {
                        targets.insert(m);
                    }
                }
            }
        }
        edges.insert(name.clone(), targets);
    }

    let mut recursive = HashSet::new();
    for start in registry.keys() {
        let mut visited = HashSet::new();
        let mut stack: Vec<Name> = edges
            .get(start)
            .map(|s| s.iter().cloned().collect())
            .unwrap_or_default();
        while let Some(curr) = stack.pop() {
            if &curr == start {
                recursive.insert(start.clone());
                break;
            }
            if visited.insert(curr.clone()) {
                if let Some(nexts) = edges.get(&curr) {
                    stack.extend(nexts.iter().cloned());
                }
            }
        }
    }
    recursive
}

/// Registry of all ADT definitions
pub type AdtRegistry = HashMap<Name, AdtInfo>;

/// Concept definition metadata
#[derive(Clone, Debug)]
pub struct ConceptInfo {
    pub name: Name,
    pub type_params: Vec<Name>,
    pub superclasses: Vec<SchemeConstraint>,
    /// Method name -> polymorphic method scheme (quantified with `PolyConstrained`)
    pub methods: Vec<(Name, Scheme)>,
}

/// Registered concept instance metadata
#[derive(Clone, Debug)]
pub struct InstanceInfo {
    pub concept_name: Name,
    /// Free type variables in `target_types` (empty for concrete instances like `Eq Int`)
    pub type_params: Vec<Name>,
    /// Prerequisite constraints (e.g., `[Eq a]` for `instance (Eq a) => Eq (List a)`)
    pub constraints: Vec<SchemeConstraint>,
    /// Target type arguments (e.g., `[List a]` or `[Int]`)
    pub target_types: Vec<Type>,
    /// Method name -> mangled global function name implementing it
    pub method_impls: HashMap<Name, Name>,
}

pub type ConceptRegistry = HashMap<Name, ConceptInfo>;
pub type InstanceRegistry = Vec<InstanceInfo>;
