/// Monomorphization support for polymorphic functions and concept methods.

use std::collections::{HashMap, HashSet};
use crate::types::{Type, Scheme};
use crate::utils::Name;

/// Represents a single instantiation of a polymorphic function
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Instantiation {
    pub function: Name,
    pub type_args: Vec<Type>,
}

impl Instantiation {
    pub fn new(function: Name, type_args: Vec<Type>) -> Self {
        Instantiation { function, type_args }
    }

    pub fn mangled_name(&self) -> Name {
        if self.type_args.is_empty() {
            self.function.clone()
        } else {
            let args_str = self
                .type_args
                .iter()
                .map(mangle_type)
                .collect::<Vec<_>>()
                .join("_");
            format!("{}_{}", self.function, args_str)
        }
    }
}

/// Mangle a type into a valid, injective identifier suffix
pub fn mangle_type(ty: &Type) -> String {
    match ty {
        Type::Con(name) => name.replace('.', "$"),
        Type::Var(name) => format!("T{}", name.trim_start_matches('.')),
        Type::Arr(params, r) => {
            if params.is_empty() {
                format!("FnUnitTo{}", mangle_type(r))
            } else {
                let ps: Vec<String> = params.iter().map(mangle_type).collect();
                format!("Fn{}To{}", ps.join("And"), mangle_type(r))
            }
        }
        Type::Tuple(elems) => {
            let es: Vec<String> = elems.iter().map(mangle_type).collect();
            format!("Tup{}", es.join("And"))
        }
        Type::Comp(base, arg) => format!("{}Of{}", mangle_type(base), mangle_type(arg)),
        Type::Void => "Void".to_string(),
    }
}

/// Registry tracking polymorphic definitions and initial instantiations
#[derive(Clone, Debug, Default)]
pub struct InstantiationRegistry {
    pub instantiations: HashSet<Instantiation>,
    pub polymorphic_functions: HashMap<Name, Scheme>,
    instantiation_context: HashMap<Name, (Name, usize)>,
}

impl InstantiationRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register_polymorphic(&mut self, name: Name, scheme: Scheme) {
        if !scheme.type_params().is_empty() {
            self.polymorphic_functions.insert(name, scheme);
        }
    }

    pub fn record_context(&mut self, fresh_var: Name, function: Name, param_index: usize) {
        self.instantiation_context.insert(fresh_var, (function, param_index));
    }

    pub fn get_context(&self, fresh_var: &Name) -> Option<&(Name, usize)> {
        self.instantiation_context.get(fresh_var)
    }

    pub fn add_instantiation(&mut self, inst: Instantiation) {
        self.instantiations.insert(inst);
    }

    pub fn get_instantiations(&self, function: &Name) -> Vec<&Instantiation> {
        self.instantiations
            .iter()
            .filter(|inst| &inst.function == function)
            .collect()
    }

    pub fn is_polymorphic(&self, function: &Name) -> bool {
        self.polymorphic_functions.contains_key(function)
    }

    pub fn get_scheme(&self, function: &Name) -> Option<&Scheme> {
        self.polymorphic_functions.get(function)
    }
}

#[derive(Clone, Debug)]
pub struct CallSite {
    pub function: Name,
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
        let arr_ty = Type::Arr(
            vec![Type::Con("Int".to_string())],
            Box::new(Type::Con("Bool".to_string())),
        );
        assert_eq!(mangle_type(&arr_ty), "FnIntToBool");
    }

    #[test]
    fn test_instantiation_mangled_name() {
        let inst = Instantiation::new("identity".to_string(), vec![Type::Con("Int".to_string())]);
        assert_eq!(inst.mangled_name(), "identity_Int");
    }
}
