use std::collections::HashMap;

pub type P<T> = Box<T>;
/// Shorter alias of Box
pub fn P<T>(t: T) -> Box<T> {
    Box::new(t)
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Id(usize);

pub type Name = String;

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct Interner {
    forward: HashMap<String, Id>,
    backward: Vec<String>
}

impl Interner {
    pub fn new() -> Interner {
        Interner { forward: HashMap::new(), backward: Vec::new() }
    }

    pub fn intern(&mut self, s: &str) -> Id {
        if let Some(&id) = self.forward.get(s) { id } else {
            let id = Id(self.backward.len());
            self.backward.push(s.to_owned());
            self.forward.insert(s.to_owned(), id);
            id
        }
    }

    pub fn trace(&self, i: Id) -> &str {
        self.backward[i.0].as_str()
    }

    pub fn trace_string(&self, i: Id) -> String {
        self.backward[i.0].clone()
    }

    /// Look up an Id without interning if it doesn't exist
    pub fn lookup(&self, s: &str) -> Option<Id> {
        self.forward.get(s).copied()
    }
}



#[derive(Clone, PartialEq, Debug)]
pub struct SymTable<'a, K, T: 'a>
    where K: 'a + ::std::cmp::Eq + ::std::hash::Hash
{
    vars: HashMap<K, T>,
    parent: Option<&'a SymTable<'a, K, T>>,
}

impl<'a: 'b, 'b, K: 'a, T: 'a> SymTable<'a, K, T>
    where K: ::std::cmp::Eq + ::std::hash::Hash + Clone
{
    pub fn with_var<F, R>(&mut self, var: K, val: T, mut cb: F) -> R
        where F: FnMut(&mut Self) -> R
    {
        let old = self.insert(var.to_owned(), val);
        let res = cb(self);
        if let Some(o) = old {
            self.insert(var, o);
        } else {
            self.remove(&var);
        }
        res
    }
}

impl<'a: 'b, 'b, K: 'a, T: 'a> SymTable<'a, K, T>
    where K: 'a + ::std::cmp::Eq + ::std::hash::Hash
{
    pub fn new() -> SymTable<'a, K, T> {
        SymTable {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn from_iter<I>(it: I) -> SymTable<'a, K, T>
        where I: IntoIterator<Item = (K, T)>
    {
        let mut env = SymTable::new();
        env.vars.extend(it);
        env
    }

    pub fn sub_env(&'a self) -> SymTable<'b, K, T> {
        let mut it = SymTable::new();
        it.parent = Some(self);
        it
    }

    pub fn exist(&self, k: &K) -> bool {
        self.vars.contains_key(k)
    }

    pub fn extend(&'a self, name: K, ty: T) -> SymTable<'b, K, T> {
        let mut it = self.sub_env();
        it.vars.insert(name, ty);
        return it;
    }

    pub fn extend_n<I>(&'a self, terms: I) -> SymTable<'b, K, T>
        where I: IntoIterator<Item = (K, T)>
    {

        let mut it = self.sub_env();
        it.vars.extend(terms);
        return it;
    }
    pub fn remove(&mut self, k: &K) -> Option<T> {
        self.vars.remove(k)
    }
    pub fn insert(&mut self, k: K, v: T) -> Option<T> {
        self.vars.insert(k, v)
    }

    pub fn lookup(&self, name: &K) -> Option<&T> {
        match self.vars.get(name) {
            None => {
                if let Some(p) = self.parent {
                    p.lookup(name)
                } else {
                    None
                }
            }
            r => r,

        }
    }

    pub fn unwrap(self) -> HashMap<K, T> {
        self.vars
    }
}

// ============================================================================
// Module-aware Symbol Table
// ============================================================================

/// An import entry mapping a local name to its original module and name
#[derive(Clone, Debug, PartialEq)]
pub struct ImportEntry {
    /// The module path where this symbol is defined
    pub module_path: Vec<Id>,
    /// The original name in that module
    pub original_name: Id,
    /// Whether this import is re-exported (pub use)
    pub is_reexport: bool,
}

/// Visibility information for a symbol
#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub enum SymbolVisibility {
    #[default]
    Private,
    Public,
}

impl SymbolVisibility {
    pub fn is_public(&self) -> bool {
        matches!(self, SymbolVisibility::Public)
    }
}

/// A symbol entry with visibility information
#[derive(Clone, Debug)]
pub struct ModuleSymbol<T> {
    pub value: T,
    pub visibility: SymbolVisibility,
    /// The module this symbol is defined in (empty for root)
    pub defined_in: Vec<Id>,
}

impl<T> ModuleSymbol<T> {
    pub fn new(value: T, visibility: SymbolVisibility, defined_in: Vec<Id>) -> Self {
        ModuleSymbol { value, visibility, defined_in }
    }

    pub fn public(value: T, defined_in: Vec<Id>) -> Self {
        ModuleSymbol::new(value, SymbolVisibility::Public, defined_in)
    }

    pub fn private(value: T, defined_in: Vec<Id>) -> Self {
        ModuleSymbol::new(value, SymbolVisibility::Private, defined_in)
    }
}

/// Module-aware symbol table supporting:
/// - Local scope lookup
/// - Import resolution
/// - Qualified name lookup (module.name)
/// - Visibility checking
#[derive(Clone, Debug)]
pub struct ModuleSymTable<'a, T: Clone> {
    /// Local symbols in current scope
    local: SymTable<'a, Id, ModuleSymbol<T>>,
    /// Symbols from submodules (module path -> symbols)
    modules: HashMap<Vec<Id>, HashMap<Id, ModuleSymbol<T>>>,
    /// Import mappings (local name -> import entry)
    imports: HashMap<Id, ImportEntry>,
    /// Glob imports (module paths whose public symbols are in scope)
    glob_imports: Vec<Vec<Id>>,
    /// Current module path
    current_path: Vec<Id>,
}

impl<'a, T: Clone> ModuleSymTable<'a, T> {
    /// Create a new module-aware symbol table
    pub fn new() -> Self {
        ModuleSymTable {
            local: SymTable::new(),
            modules: HashMap::new(),
            imports: HashMap::new(),
            glob_imports: Vec::new(),
            current_path: Vec::new(),
        }
    }

    /// Create a new scope within a module
    pub fn with_module_path(current_path: Vec<Id>) -> Self {
        ModuleSymTable {
            local: SymTable::new(),
            modules: HashMap::new(),
            imports: HashMap::new(),
            glob_imports: Vec::new(),
            current_path,
        }
    }

    /// Get the current module path
    pub fn current_path(&self) -> &[Id] {
        &self.current_path
    }

    /// Enter a submodule
    pub fn enter_module(&mut self, name: Id) {
        self.current_path.push(name);
    }

    /// Exit the current module (go back to parent)
    pub fn exit_module(&mut self) {
        self.current_path.pop();
    }

    /// Insert a symbol with visibility into the current scope
    pub fn insert_with_visibility(&mut self, name: Id, value: T, visibility: SymbolVisibility) {
        let symbol = ModuleSymbol::new(value, visibility, self.current_path.clone());
        self.local.insert(name, symbol);
    }

    /// Insert a public symbol
    pub fn insert_public(&mut self, name: Id, value: T) {
        self.insert_with_visibility(name, value, SymbolVisibility::Public);
    }

    /// Insert a private symbol
    pub fn insert_private(&mut self, name: Id, value: T) {
        self.insert_with_visibility(name, value, SymbolVisibility::Private);
    }

    /// Add an import mapping
    pub fn add_import(&mut self, local_name: Id, module_path: Vec<Id>, original_name: Id, is_reexport: bool) {
        self.imports.insert(local_name, ImportEntry {
            module_path,
            original_name,
            is_reexport,
        });
    }

    /// Add a glob import (open)
    pub fn add_glob_import(&mut self, module_path: Vec<Id>) {
        if !self.glob_imports.contains(&module_path) {
            self.glob_imports.push(module_path);
        }
    }

    /// Register a module's symbols
    pub fn register_module(&mut self, module_path: Vec<Id>, symbols: HashMap<Id, ModuleSymbol<T>>) {
        self.modules.insert(module_path, symbols);
    }

    /// Look up a simple name (not qualified)
    /// Order: local scope -> imports -> glob imports
    pub fn lookup(&self, name: &Id) -> Option<&T> {
        // 1. Local scope
        if let Some(sym) = self.local.lookup(name) {
            return Some(&sym.value);
        }

        // 2. Direct imports
        if let Some(import) = self.imports.get(name) {
            if let Some(module_syms) = self.modules.get(&import.module_path) {
                if let Some(sym) = module_syms.get(&import.original_name) {
                    // Check visibility (imported symbols must be public in their module)
                    if sym.visibility.is_public() {
                        return Some(&sym.value);
                    }
                }
            }
        }

        // 3. Glob imports (look in each glob-imported module)
        for glob_path in &self.glob_imports {
            if let Some(module_syms) = self.modules.get(glob_path) {
                if let Some(sym) = module_syms.get(name) {
                    if sym.visibility.is_public() {
                        return Some(&sym.value);
                    }
                }
            }
        }

        None
    }

    /// Look up a qualified name (e.g., module.submodule.name)
    /// The path segments represent the module path, and the last segment is the name
    pub fn lookup_qualified(&self, path: &[Id]) -> Option<&T> {
        if path.is_empty() {
            return None;
        }

        if path.len() == 1 {
            // Simple lookup
            return self.lookup(&path[0]);
        }

        // Split into module path and name
        let (module_path, name) = path.split_at(path.len() - 1);
        let module_path = module_path.to_vec();
        let name = &name[0];

        // Look up in the specified module
        if let Some(module_syms) = self.modules.get(&module_path) {
            if let Some(sym) = module_syms.get(name) {
                // Check visibility for cross-module access
                // Private symbols are only accessible from the same module
                if sym.visibility.is_public() || self.is_same_or_child_module(&sym.defined_in) {
                    return Some(&sym.value);
                }
            }
        }

        None
    }

    /// Check if the current module is the same as or a child of the given module
    fn is_same_or_child_module(&self, module: &[Id]) -> bool {
        if module.len() > self.current_path.len() {
            return false;
        }
        self.current_path.starts_with(module)
    }

    /// Look up with visibility check
    /// Returns Some only if the symbol is accessible from the current module
    pub fn lookup_with_visibility(&self, name: &Id) -> Option<(&T, SymbolVisibility)> {
        if let Some(sym) = self.local.lookup(name) {
            // Check if we're in the same module or if it's public
            if sym.visibility.is_public() || self.is_same_or_child_module(&sym.defined_in) {
                return Some((&sym.value, sym.visibility));
            }
        }

        // Check imports (imports are always accessible locally)
        if let Some(import) = self.imports.get(name) {
            if let Some(module_syms) = self.modules.get(&import.module_path) {
                if let Some(sym) = module_syms.get(&import.original_name) {
                    if sym.visibility.is_public() {
                        return Some((&sym.value, sym.visibility));
                    }
                }
            }
        }

        None
    }

    /// Create a sub-scope that inherits the current module context
    pub fn sub_scope(&'a self) -> ModuleSymTable<'a, T> {
        ModuleSymTable {
            local: self.local.sub_env(),
            modules: self.modules.clone(),
            imports: self.imports.clone(),
            glob_imports: self.glob_imports.clone(),
            current_path: self.current_path.clone(),
        }
    }
}

impl<'a, T: Clone> Default for ModuleSymTable<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interner_basic() {
        let mut interner = Interner::new();
        let id1 = interner.intern("foo");
        let id2 = interner.intern("bar");
        let id3 = interner.intern("foo"); // Same as id1

        assert_eq!(id1, id3);
        assert_ne!(id1, id2);
        assert_eq!(interner.trace(id1), "foo");
        assert_eq!(interner.trace(id2), "bar");
    }

    #[test]
    fn test_symtable_basic() {
        let mut table: SymTable<i32, String> = SymTable::new();
        table.insert(1, "one".to_string());
        table.insert(2, "two".to_string());

        assert_eq!(table.lookup(&1), Some(&"one".to_string()));
        assert_eq!(table.lookup(&2), Some(&"two".to_string()));
        assert_eq!(table.lookup(&3), None);
    }

    #[test]
    fn test_symtable_scoping() {
        let table: SymTable<i32, String> = SymTable::from_iter([(1, "one".to_string())]);
        let sub_table = table.extend(2, "two".to_string());

        // Sub-table should see both
        assert_eq!(sub_table.lookup(&1), Some(&"one".to_string()));
        assert_eq!(sub_table.lookup(&2), Some(&"two".to_string()));

        // Parent table should only see original
        assert_eq!(table.lookup(&1), Some(&"one".to_string()));
        assert_eq!(table.lookup(&2), None);
    }

    #[test]
    fn test_module_symtable_local_lookup() {
        let mut interner = Interner::new();
        let mut table: ModuleSymTable<String> = ModuleSymTable::new();

        let foo_id = interner.intern("foo");
        table.insert_public(foo_id, "foo_value".to_string());

        assert_eq!(table.lookup(&foo_id), Some(&"foo_value".to_string()));
    }

    #[test]
    fn test_module_symtable_visibility() {
        let mut interner = Interner::new();
        let mut table: ModuleSymTable<String> = ModuleSymTable::new();

        let pub_id = interner.intern("public");
        let priv_id = interner.intern("private");

        table.insert_public(pub_id, "pub_val".to_string());
        table.insert_private(priv_id, "priv_val".to_string());

        // Both should be accessible from the same module
        assert_eq!(table.lookup(&pub_id), Some(&"pub_val".to_string()));
        assert_eq!(table.lookup(&priv_id), Some(&"priv_val".to_string()));

        // Check visibility info
        let (_, vis) = table.lookup_with_visibility(&pub_id).unwrap();
        assert!(vis.is_public());

        let (_, vis) = table.lookup_with_visibility(&priv_id).unwrap();
        assert!(!vis.is_public());
    }

    #[test]
    fn test_module_symtable_imports() {
        let mut interner = Interner::new();
        let mut table: ModuleSymTable<String> = ModuleSymTable::new();

        // Set up a module with a public symbol
        let utils_id = interner.intern("utils");
        let clamp_id = interner.intern("clamp");
        let clamp_alias_id = interner.intern("myClamp");

        let mut utils_symbols: HashMap<Id, ModuleSymbol<String>> = HashMap::new();
        utils_symbols.insert(clamp_id, ModuleSymbol::public("clamp_fn".to_string(), vec![utils_id]));
        table.register_module(vec![utils_id], utils_symbols);

        // Add import: use utils.clamp as myClamp
        table.add_import(clamp_alias_id, vec![utils_id], clamp_id, false);

        // Should be able to look up via alias
        assert_eq!(table.lookup(&clamp_alias_id), Some(&"clamp_fn".to_string()));
    }

    #[test]
    fn test_module_symtable_qualified_lookup() {
        let mut interner = Interner::new();
        let mut table: ModuleSymTable<String> = ModuleSymTable::new();

        // Set up collections.list module with a public symbol
        let collections_id = interner.intern("collections");
        let list_id = interner.intern("list");
        let length_id = interner.intern("length");

        let mut list_symbols: HashMap<Id, ModuleSymbol<String>> = HashMap::new();
        list_symbols.insert(
            length_id,
            ModuleSymbol::public("length_fn".to_string(), vec![collections_id, list_id])
        );
        table.register_module(vec![collections_id, list_id], list_symbols);

        // Should be able to look up via qualified path
        let path = vec![collections_id, list_id, length_id];
        assert_eq!(table.lookup_qualified(&path), Some(&"length_fn".to_string()));
    }

    #[test]
    fn test_module_symtable_glob_import() {
        let mut interner = Interner::new();
        let mut table: ModuleSymTable<String> = ModuleSymTable::new();

        // Set up utils module with public symbols
        let utils_id = interner.intern("utils");
        let clamp_id = interner.intern("clamp");
        let helper_id = interner.intern("helper");

        let mut utils_symbols: HashMap<Id, ModuleSymbol<String>> = HashMap::new();
        utils_symbols.insert(clamp_id, ModuleSymbol::public("clamp_fn".to_string(), vec![utils_id]));
        utils_symbols.insert(helper_id, ModuleSymbol::private("helper_fn".to_string(), vec![utils_id]));
        table.register_module(vec![utils_id], utils_symbols);

        // Add glob import: open utils
        table.add_glob_import(vec![utils_id]);

        // Should be able to look up public symbol directly
        assert_eq!(table.lookup(&clamp_id), Some(&"clamp_fn".to_string()));

        // Private symbol should not be accessible
        assert_eq!(table.lookup(&helper_id), None);
    }

    #[test]
    fn test_module_symtable_module_path_tracking() {
        let mut interner = Interner::new();
        let mut table: ModuleSymTable<String> = ModuleSymTable::with_module_path(vec![]);

        let collections_id = interner.intern("collections");
        let list_id = interner.intern("list");

        assert!(table.current_path().is_empty());

        table.enter_module(collections_id);
        assert_eq!(table.current_path(), &[collections_id]);

        table.enter_module(list_id);
        assert_eq!(table.current_path(), &[collections_id, list_id]);

        table.exit_module();
        assert_eq!(table.current_path(), &[collections_id]);

        table.exit_module();
        assert!(table.current_path().is_empty());
    }
}
