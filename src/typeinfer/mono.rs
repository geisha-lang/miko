/// Monomorphization support for polymorphic functions
///
/// This module provides data structures for tracking and generating
/// specialized versions of polymorphic functions.

use std::collections::{HashMap, HashSet};
use crate::types::{Type, Scheme};
use crate::utils::Name;

/// Represents a single instantiation of a polymorphic function
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Instantiation {
    /// The original function name
    pub function: Name,
    /// Concrete type arguments for each type parameter
    pub type_args: Vec<Type>,
}

impl Instantiation {
    pub fn new(function: Name, type_args: Vec<Type>) -> Self {
        Instantiation { function, type_args }
    }

    /// Generate a mangled name for this instantiation
    /// E.g., "identity" with [Int] becomes "identity_Int"
    pub fn mangled_name(&self) -> Name {
        if self.type_args.is_empty() {
            self.function.clone()
        } else {
            let args_str = self.type_args.iter()
                .map(|t| mangle_type(t))
                .collect::<Vec<_>>()
                .join("_");
            format!("{}_{}", self.function, args_str)
        }
    }
}

/// Mangle a type into a valid identifier suffix
fn mangle_type(ty: &Type) -> String {
    match ty {
        Type::Con(name) => name.clone(),
        Type::Var(name) => format!("T{}", name),
        Type::Arr(p, r) => format!("Fn{}To{}", mangle_type(p), mangle_type(r)),
        Type::Prod(l, r) => format!("Pair{}And{}", mangle_type(l), mangle_type(r)),
        Type::Comp(base, arg) => format!("{}Of{}", mangle_type(base), mangle_type(arg)),
        Type::Void => "Void".to_string(),
    }
}

/// Registry tracking all instantiations needed for monomorphization
#[derive(Clone, Debug, Default)]
pub struct InstantiationRegistry {
    /// Set of all instantiations discovered during type inference
    pub instantiations: HashSet<Instantiation>,
    /// Map from function name to its polymorphic scheme (for functions that need specialization)
    pub polymorphic_functions: HashMap<Name, Scheme>,
    /// Context tracking: maps fresh type variables to (function_name, param_index)
    /// Used during solve() to connect resolved types back to call sites
    instantiation_context: HashMap<Name, (Name, usize)>,
}

impl InstantiationRegistry {
    pub fn new() -> Self {
        InstantiationRegistry {
            instantiations: HashSet::new(),
            polymorphic_functions: HashMap::new(),
            instantiation_context: HashMap::new(),
        }
    }

    /// Register a polymorphic function
    pub fn register_polymorphic(&mut self, name: Name, scheme: Scheme) {
        if let Scheme::Poly(ref tvs, _) | Scheme::PolyConstrained(ref tvs, _, _) = scheme {
            if !tvs.is_empty() {
                self.polymorphic_functions.insert(name, scheme);
            }
        }
    }

    /// Record context for a fresh type variable during instantiation
    pub fn record_context(&mut self, fresh_var: Name, function: Name, param_index: usize) {
        self.instantiation_context.insert(fresh_var, (function, param_index));
    }

    /// Get the context for a fresh type variable
    pub fn get_context(&self, fresh_var: &Name) -> Option<&(Name, usize)> {
        self.instantiation_context.get(fresh_var)
    }

    /// Add an instantiation
    pub fn add_instantiation(&mut self, inst: Instantiation) {
        self.instantiations.insert(inst);
    }

    /// Get all instantiations for a specific function
    pub fn get_instantiations(&self, function: &Name) -> Vec<&Instantiation> {
        self.instantiations.iter()
            .filter(|inst| &inst.function == function)
            .collect()
    }

    /// Check if a function is polymorphic
    pub fn is_polymorphic(&self, function: &Name) -> bool {
        self.polymorphic_functions.contains_key(function)
    }

    /// Get the scheme for a polymorphic function
    pub fn get_scheme(&self, function: &Name) -> Option<&Scheme> {
        self.polymorphic_functions.get(function)
    }
}

/// Information about a pending instantiation call site
#[derive(Clone, Debug)]
pub struct CallSite {
    /// The function being called
    pub function: Name,
    /// Fresh type variables introduced at this call site
    pub fresh_vars: Vec<Name>,
}

impl CallSite {
    pub fn new(function: Name, fresh_vars: Vec<Name>) -> Self {
        CallSite { function, fresh_vars }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mangle_type() {
        assert_eq!(mangle_type(&Type::Con("Int".to_string())), "Int");
        assert_eq!(mangle_type(&Type::Con("Bool".to_string())), "Bool");

        let arr_ty = Type::Arr(
            Box::new(Type::Con("Int".to_string())),
            Box::new(Type::Con("Bool".to_string()))
        );
        assert_eq!(mangle_type(&arr_ty), "FnIntToBool");
    }

    #[test]
    fn test_instantiation_mangled_name() {
        let inst = Instantiation::new(
            "identity".to_string(),
            vec![Type::Con("Int".to_string())]
        );
        assert_eq!(inst.mangled_name(), "identity_Int");

        let inst2 = Instantiation::new(
            "pair".to_string(),
            vec![Type::Con("Int".to_string()), Type::Con("Bool".to_string())]
        );
        assert_eq!(inst2.mangled_name(), "pair_Int_Bool");
    }

    #[test]
    fn test_registry_basics() {
        let mut registry = InstantiationRegistry::new();

        let scheme = Scheme::Poly(
            vec!["a".to_string()],
            Type::Arr(
                Box::new(Type::Var("a".to_string())),
                Box::new(Type::Var("a".to_string()))
            )
        );

        registry.register_polymorphic("identity".to_string(), scheme);
        assert!(registry.is_polymorphic(&"identity".to_string()));
        assert!(!registry.is_polymorphic(&"foo".to_string()));
    }
}
