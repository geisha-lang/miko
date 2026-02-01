//! Import resolution for `use` statements.
//!
//! This module handles resolving `use` statements to bring names into scope.
//! It creates a mapping from imported names to their fully qualified paths.

use std::collections::HashMap;

use crate::syntax::form::{ModulePath, UseItem, Visibility};
use crate::utils::{Id, Interner};

/// Information about an available name for import resolution.
#[derive(Debug, Clone)]
pub struct NameInfo {
    /// The qualified Id for this name
    pub qualified_id: Id,
    /// Whether this name is public (can be imported from other modules)
    pub is_public: bool,
}

/// Represents the resolved imports for a module.
#[derive(Debug, Clone, Default)]
pub struct ImportMap {
    /// Maps local name -> qualified name
    /// e.g., "add" -> "math.add" from `use math.{add}`
    mappings: HashMap<Id, Id>,
}

impl ImportMap {
    pub fn new() -> Self {
        ImportMap {
            mappings: HashMap::new(),
        }
    }

    /// Add an import mapping: local_name -> qualified_name
    pub fn add(&mut self, local: Id, qualified: Id) {
        self.mappings.insert(local, qualified);
    }

    /// Look up the qualified name for an imported local name
    pub fn resolve(&self, local: &Id) -> Option<&Id> {
        self.mappings.get(local)
    }

    /// Check if a name is imported
    pub fn contains(&self, local: &Id) -> bool {
        self.mappings.contains_key(local)
    }

    /// Get all import mappings
    pub fn iter(&self) -> impl Iterator<Item = (&Id, &Id)> {
        self.mappings.iter()
    }
}

/// Errors that can occur during import resolution.
#[derive(Debug, Clone)]
pub enum ImportError {
    /// Trying to import a private item
    PrivateItem {
        name: String,
        module: String,
    },
    /// Item not found in module
    NotFound {
        name: String,
        module: String,
    },
}

/// Resolve imports from a list of use statements.
///
/// # Arguments
/// * `imports` - List of (visibility, UseItem) from the module
/// * `module_path` - The current module's path (for relative resolution)
/// * `available_names` - Set of names that are actually available for import (without visibility info)
/// * `interner` - String interner for creating new IDs
///
/// # Returns
/// An ImportMap containing the resolved import mappings
pub fn resolve_imports(
    imports: &[(Visibility, UseItem)],
    _module_path: &str,
    available_names: &HashMap<String, Id>,
    interner: &mut Interner,
) -> ImportMap {
    let mut import_map = ImportMap::new();

    for (_visibility, use_item) in imports {
        match use_item {
            UseItem::Single(path) => {
                // `use foo.bar` - imports `bar` from module `foo`
                let full_path = path.to_string_with(interner);
                if let Some(name) = path.name() {
                    // Check if this name is available
                    if let Some(&qualified_id) = available_names.get(&full_path) {
                        import_map.add(name, qualified_id);
                    }
                }
            }

            UseItem::Multiple(base_path, specs) => {
                // `use foo.{bar, baz}` - imports specific items from module `foo`
                let base_str = base_path.to_string_with(interner);

                for spec in specs {
                    let item_name = interner.trace(spec.name).to_string();
                    let full_path = if base_str.is_empty() {
                        item_name.clone()
                    } else {
                        format!("{}.{}", base_str, item_name)
                    };

                    if let Some(&qualified_id) = available_names.get(&full_path) {
                        // Use alias if provided, otherwise use original name
                        let local_name = spec.alias.unwrap_or(spec.name);
                        import_map.add(local_name, qualified_id);
                    }
                }
            }

            UseItem::Glob(base_path) => {
                // `use foo.*` (via `open foo`) - imports all public items from module `foo`
                let base_str = base_path.to_string_with(interner);
                let prefix = if base_str.is_empty() {
                    String::new()
                } else {
                    format!("{}.", base_str)
                };

                // Find all names that start with this prefix
                for (full_name, &qualified_id) in available_names {
                    if full_name.starts_with(&prefix) {
                        let rest = &full_name[prefix.len()..];
                        // Only import direct children, not nested items
                        if !rest.contains('.') && !rest.is_empty() {
                            let local_id = interner.intern(rest);
                            import_map.add(local_id, qualified_id);
                        }
                    }
                }
            }

            UseItem::Alias(path, alias) => {
                // `use foo.bar as baz` - imports with alias
                let full_path = path.to_string_with(interner);
                if let Some(&qualified_id) = available_names.get(&full_path) {
                    import_map.add(*alias, qualified_id);
                }
            }
        }
    }

    import_map
}

/// Resolve imports with visibility checking.
///
/// This version checks that imported items are public and reports errors.
///
/// # Arguments
/// * `imports` - List of (visibility, UseItem) from the module
/// * `module_path` - The current module's path (for relative resolution)
/// * `available_names` - Map of names to their info (including visibility)
/// * `interner` - String interner for creating new IDs
///
/// # Returns
/// A tuple of (ImportMap, Vec<ImportError>)
pub fn resolve_imports_with_visibility(
    imports: &[(Visibility, UseItem)],
    _module_path: &str,
    available_names: &HashMap<String, NameInfo>,
    interner: &mut Interner,
) -> (ImportMap, Vec<ImportError>) {
    let mut import_map = ImportMap::new();
    let mut errors = Vec::new();

    for (_visibility, use_item) in imports {
        match use_item {
            UseItem::Single(path) => {
                // `use foo.bar` - imports `bar` from module `foo`
                let full_path = path.to_string_with(interner);
                if let Some(name) = path.name() {
                    match available_names.get(&full_path) {
                        Some(info) if info.is_public => {
                            import_map.add(name, info.qualified_id);
                        }
                        Some(_) => {
                            // Private item
                            let module = path.module_path();
                            let module_str = module.iter()
                                .map(|id| interner.trace(*id).to_string())
                                .collect::<Vec<_>>()
                                .join(".");
                            errors.push(ImportError::PrivateItem {
                                name: interner.trace(name).to_string(),
                                module: module_str,
                            });
                        }
                        None => {
                            // Item not found - silently ignore (may be defined locally)
                        }
                    }
                }
            }

            UseItem::Multiple(base_path, specs) => {
                // `use foo.{bar, baz}` - imports specific items from module `foo`
                let base_str = base_path.to_string_with(interner);

                for spec in specs {
                    let item_name = interner.trace(spec.name).to_string();
                    let full_path = if base_str.is_empty() {
                        item_name.clone()
                    } else {
                        format!("{}.{}", base_str, item_name)
                    };

                    match available_names.get(&full_path) {
                        Some(info) if info.is_public => {
                            let local_name = spec.alias.unwrap_or(spec.name);
                            import_map.add(local_name, info.qualified_id);
                        }
                        Some(_) => {
                            errors.push(ImportError::PrivateItem {
                                name: item_name,
                                module: base_str.clone(),
                            });
                        }
                        None => {
                            // Item not found - silently ignore
                        }
                    }
                }
            }

            UseItem::Glob(base_path) => {
                // `use foo.*` (via `open foo`) - imports all PUBLIC items from module `foo`
                let base_str = base_path.to_string_with(interner);
                let prefix = if base_str.is_empty() {
                    String::new()
                } else {
                    format!("{}.", base_str)
                };

                // Find all PUBLIC names that start with this prefix
                for (full_name, info) in available_names {
                    if info.is_public && full_name.starts_with(&prefix) {
                        let rest = &full_name[prefix.len()..];
                        // Only import direct children, not nested items
                        if !rest.contains('.') && !rest.is_empty() {
                            let local_id = interner.intern(rest);
                            import_map.add(local_id, info.qualified_id);
                        }
                    }
                }
            }

            UseItem::Alias(path, alias) => {
                // `use foo.bar as baz` - imports with alias
                let full_path = path.to_string_with(interner);
                match available_names.get(&full_path) {
                    Some(info) if info.is_public => {
                        import_map.add(*alias, info.qualified_id);
                    }
                    Some(_) => {
                        let module = path.module_path();
                        let module_str = module.iter()
                            .map(|id| interner.trace(*id).to_string())
                            .collect::<Vec<_>>()
                            .join(".");
                        if let Some(name) = path.name() {
                            errors.push(ImportError::PrivateItem {
                                name: interner.trace(name).to_string(),
                                module: module_str,
                            });
                        }
                    }
                    None => {
                        // Item not found - silently ignore
                    }
                }
            }
        }
    }

    (import_map, errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_import() {
        let mut interner = Interner::new();
        let math_id = interner.intern("math");
        let add_id = interner.intern("add");
        let math_add_id = interner.intern("math.add");

        let mut available = HashMap::new();
        available.insert("math.add".to_string(), math_add_id);

        let path = ModulePath::new(vec![math_id, add_id]);
        let imports = vec![(Visibility::Private, UseItem::Single(path))];

        let import_map = resolve_imports(&imports, "", &available, &mut interner);

        assert!(import_map.contains(&add_id));
        assert_eq!(import_map.resolve(&add_id), Some(&math_add_id));
    }

    #[test]
    fn test_alias_import() {
        let mut interner = Interner::new();
        let math_id = interner.intern("math");
        let add_id = interner.intern("add");
        let plus_id = interner.intern("plus");
        let math_add_id = interner.intern("math.add");

        let mut available = HashMap::new();
        available.insert("math.add".to_string(), math_add_id);

        let path = ModulePath::new(vec![math_id, add_id]);
        let imports = vec![(Visibility::Private, UseItem::Alias(path, plus_id))];

        let import_map = resolve_imports(&imports, "", &available, &mut interner);

        assert!(!import_map.contains(&add_id));
        assert!(import_map.contains(&plus_id));
        assert_eq!(import_map.resolve(&plus_id), Some(&math_add_id));
    }
}
