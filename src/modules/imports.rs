//! Import resolution for `use` and `open` statements with visibility checking.

use std::collections::HashMap;

use crate::syntax::form::{UseItem, Visibility};
use crate::utils::{Id, Interner};

#[derive(Debug, Clone)]
pub struct NameInfo {
    pub qualified_id: Id,
    pub is_public: bool,
    pub defined_in_module: String,
}

#[derive(Debug, Clone, Default)]
pub struct ImportMap {
    mappings: HashMap<Id, Id>,
}

impl ImportMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, local: Id, qualified: Id) {
        self.mappings.insert(local, qualified);
    }

    pub fn resolve(&self, local: &Id) -> Option<&Id> {
        self.mappings.get(local)
    }

    pub fn contains(&self, local: &Id) -> bool {
        self.mappings.contains_key(local)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Id, &Id)> {
        self.mappings.iter()
    }
}

#[derive(Debug, Clone)]
pub enum ImportError {
    PrivateItem { name: String, module: String },
    NotFound { name: String, module: String },
}

fn resolve_candidate<'a>(
    raw_target: &str,
    current_module: &str,
    available_names: &'a HashMap<String, NameInfo>,
) -> Option<&'a NameInfo> {
    // 1. Relative to current module (e.g. `math.add` inside `main` -> `main.math.add`, or `b.foo` inside `a` -> `a.b.foo`)
    if !current_module.is_empty() {
        let rel = format!("{}.{}", current_module, raw_target);
        if let Some(info) = available_names.get(&rel) {
            return Some(info);
        }
    }
    // 2. Root-relative (e.g. sibling module `math.add` from another submodule)
    available_names.get(raw_target)
}

pub fn resolve_imports(
    imports: &[(Visibility, UseItem)],
    module_path: &str,
    available_names: &HashMap<String, Id>,
    interner: &mut Interner,
) -> ImportMap {
    let info_map: HashMap<String, NameInfo> = available_names
        .iter()
        .map(|(k, &id)| {
            let mod_part = k.rsplit_once('.').map(|(m, _)| m.to_string()).unwrap_or_default();
            (
                k.clone(),
                NameInfo {
                    qualified_id: id,
                    is_public: true,
                    defined_in_module: mod_part,
                },
            )
        })
        .collect();
    let (imap, _) = resolve_imports_with_visibility(imports, module_path, &info_map, interner);
    imap
}

pub fn resolve_imports_with_visibility(
    imports: &[(Visibility, UseItem)],
    current_module: &str,
    available_names: &HashMap<String, NameInfo>,
    interner: &mut Interner,
) -> (ImportMap, Vec<ImportError>) {
    let mut import_map = ImportMap::new();
    let mut errors = Vec::new();

    for (_visibility, use_item) in imports {
        match use_item {
            UseItem::Single(path) => {
                let full_path = path.to_string_with(interner);
                if let Some(name) = path.name() {
                    let module_str = path
                        .module_path()
                        .iter()
                        .map(|id| interner.trace(*id).to_string())
                        .collect::<Vec<_>>()
                        .join(".");
                    match resolve_candidate(&full_path, current_module, available_names) {
                        Some(info)
                            if info.is_public || info.defined_in_module == current_module =>
                        {
                            import_map.add(name, info.qualified_id);
                        }
                        Some(_) => {
                            errors.push(ImportError::PrivateItem {
                                name: interner.trace(name).to_string(),
                                module: module_str,
                            });
                        }
                        None => {
                            errors.push(ImportError::NotFound {
                                name: interner.trace(name).to_string(),
                                module: module_str,
                            });
                        }
                    }
                }
            }

            UseItem::Multiple(base_path, specs) => {
                let base_str = base_path.to_string_with(interner);
                for spec in specs {
                    let item_name = interner.trace(spec.name).to_string();
                    let full_path = if base_str.is_empty() {
                        item_name.clone()
                    } else {
                        format!("{}.{}", base_str, item_name)
                    };

                    match resolve_candidate(&full_path, current_module, available_names) {
                        Some(info)
                            if info.is_public || info.defined_in_module == current_module =>
                        {
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
                            errors.push(ImportError::NotFound {
                                name: item_name,
                                module: base_str.clone(),
                            });
                        }
                    }
                }
            }

            UseItem::Glob(base_path) => {
                let base_str = base_path.to_string_with(interner);
                let mut prefixes = Vec::new();
                if !current_module.is_empty() {
                    prefixes.push(format!("{}.{}.", current_module, base_str));
                }
                prefixes.push(if base_str.is_empty() {
                    String::new()
                } else {
                    format!("{}.", base_str)
                });

                for prefix in prefixes {
                    let mut matched_any = false;
                    for (full_name, info) in available_names {
                        if info.is_public && full_name.starts_with(&prefix) {
                            let rest = &full_name[prefix.len()..];
                            if !rest.contains('.') && !rest.is_empty() {
                                let local_id = interner.intern(rest);
                                import_map.add(local_id, info.qualified_id);
                                matched_any = true;
                            }
                        }
                    }
                    if matched_any {
                        break;
                    }
                }
            }

            UseItem::Alias(path, alias) => {
                let full_path = path.to_string_with(interner);
                let module_str = path
                    .module_path()
                    .iter()
                    .map(|id| interner.trace(*id).to_string())
                    .collect::<Vec<_>>()
                    .join(".");
                match resolve_candidate(&full_path, current_module, available_names) {
                    Some(info)
                        if info.is_public || info.defined_in_module == current_module =>
                    {
                        import_map.add(*alias, info.qualified_id);
                    }
                    Some(_) => {
                        if let Some(name) = path.name() {
                            errors.push(ImportError::PrivateItem {
                                name: interner.trace(name).to_string(),
                                module: module_str,
                            });
                        }
                    }
                    None => {
                        if let Some(name) = path.name() {
                            errors.push(ImportError::NotFound {
                                name: interner.trace(name).to_string(),
                                module: module_str,
                            });
                        }
                    }
                }
            }
        }
    }

    (import_map, errors)
}
