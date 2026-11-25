//! Module loader for finding and loading `.gs` source files and inline submodules.

use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

use crate::syntax::form::{Def, FileModule, ModuleDef, ModuleItem, ModulePath, UseItem, Visibility};
use crate::syntax::parser;
use crate::utils::{Id, Interner};

/// Built-in standard library (`std.*`) and system library (`sys.*`) modules embedded in `miko`.
pub fn builtin_module_source(module_key: &str) -> Option<&'static str> {
    match module_key {
        "std.option" => Some(include_str!("../../stdlib/std/option.gs")),
        "std.result" => Some(include_str!("../../stdlib/std/result.gs")),
        "std.list" => Some(include_str!("../../stdlib/std/list.gs")),
        "std.char" => Some(include_str!("../../stdlib/std/char.gs")),
        "std.buf" => Some(include_str!("../../stdlib/std/buf.gs")),
        "std.str" => Some(include_str!("../../stdlib/std/str.gs")),
        "std.vec" => Some(include_str!("../../stdlib/std/vec.gs")),
        "std.map" => Some(include_str!("../../stdlib/std/map.gs")),
        "sys.io" => Some(include_str!("../../stdlib/sys/io.gs")),
        "sys.fs" => Some(include_str!("../../stdlib/sys/fs.gs")),
        "sys.env" => Some(include_str!("../../stdlib/sys/env.gs")),
        "sys.proc" => Some(include_str!("../../stdlib/sys/proc.gs")),
        _ => None,
    }
}

fn import_target_module_key(use_item: &UseItem, interner: &Interner) -> String {
    match use_item {
        UseItem::Single(p) | UseItem::Alias(p, _) => p
            .module_path()
            .iter()
            .map(|id| interner.trace(*id))
            .collect::<Vec<_>>()
            .join("."),
        UseItem::Multiple(p, _) | UseItem::Glob(p) => p.to_string_with(interner),
    }
}

#[derive(Debug, Clone)]
pub enum ModuleError {
    ModuleNotFound {
        path: String,
        tried: Vec<PathBuf>,
    },
    IoError {
        path: PathBuf,
        message: String,
    },
    ParseError {
        path: PathBuf,
        message: String,
    },
    CircularDependency {
        cycle: Vec<String>,
    },
}

impl std::fmt::Display for ModuleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleError::ModuleNotFound { path, tried } => {
                writeln!(f, "Module '{}' not found. Tried:", path)?;
                for p in tried {
                    writeln!(f, "  - {}", p.display())?;
                }
                Ok(())
            }
            ModuleError::IoError { path, message } => {
                write!(f, "IO error reading '{}': {}", path.display(), message)
            }
            ModuleError::ParseError { path, message } => {
                write!(f, "Parse error in '{}': {}", path.display(), message)
            }
            ModuleError::CircularDependency { cycle } => {
                write!(f, "Circular dependency detected: {}", cycle.join(" -> "))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct LoadedModule {
    pub path: ModulePath,
    pub file_path: PathBuf,
    pub defs: Vec<(Visibility, Def)>,
    pub imports: Vec<(Visibility, UseItem)>,
    pub submodules: Vec<(Visibility, Id, String)>,
}

impl LoadedModule {
    pub fn public_defs(&self) -> impl Iterator<Item = &Def> {
        self.defs
            .iter()
            .filter(|(vis, _)| vis.is_public())
            .map(|(_, def)| def)
    }

    pub fn all_defs(&self) -> impl Iterator<Item = &Def> {
        self.defs.iter().map(|(_, def)| def)
    }

    pub fn defs_vec(&self) -> Vec<Def> {
        self.defs.iter().map(|(_, def)| def.clone()).collect()
    }
}

pub struct ModuleLoader {
    source_root: PathBuf,
    cache: HashMap<String, LoadedModule>,
    loading: Vec<String>,
}

impl ModuleLoader {
    pub fn new(source_root: PathBuf) -> Self {
        ModuleLoader {
            source_root,
            cache: HashMap::new(),
            loading: Vec::new(),
        }
    }

    pub fn resolve_path(&self, module_path: &[&str]) -> Option<PathBuf> {
        if module_path.is_empty() {
            return None;
        }
        let mut base_path = self.source_root.clone();
        for segment in module_path {
            base_path.push(segment);
        }

        let file_path = base_path.with_extension("gs");
        if file_path.exists() {
            return Some(file_path);
        }

        let mod_path = base_path.join("mod.gs");
        if mod_path.exists() {
            return Some(mod_path);
        }

        None
    }

    pub fn resolve_relative(&self, parent_path: &[&str], child_name: &str) -> Option<PathBuf> {
        let mut full_path = parent_path.to_vec();
        full_path.push(child_name);
        self.resolve_path(&full_path)
    }

    fn tried_paths(&self, module_path: &[&str]) -> Vec<PathBuf> {
        let mut base_path = self.source_root.clone();
        for segment in module_path {
            base_path.push(segment);
        }
        vec![base_path.with_extension("gs"), base_path.join("mod.gs")]
    }

    pub fn load(
        &mut self,
        module_path: &[&str],
        interner: &RefCell<Interner>,
    ) -> Result<(), ModuleError> {
        let path_key = module_path.join(".");

        if self.cache.contains_key(&path_key) {
            return Ok(());
        }

        if self.loading.contains(&path_key) {
            let mut cycle = self.loading.clone();
            cycle.push(path_key.clone());
            let start = cycle.iter().position(|p| p == &path_key).unwrap();
            return Err(ModuleError::CircularDependency {
                cycle: cycle[start..].to_vec(),
            });
        }

        self.loading.push(path_key.clone());

        let file_path = self.resolve_path(module_path).ok_or_else(|| {
            ModuleError::ModuleNotFound {
                path: path_key.clone(),
                tried: self.tried_paths(module_path),
            }
        })?;

        let source = self.read_file(&file_path)?;
        let mod_path = {
            let mut inter = interner.borrow_mut();
            let segments: Vec<Id> = module_path.iter().map(|s| inter.intern(s)).collect();
            ModulePath::new(segments)
        };

        let file_module = self.parse_file_module(&file_path, &source, &mod_path, interner)?;
        let mut inline_modules = Vec::new();
        let (defs, imports, submodules) =
            self.extract_items(&file_module, &file_path, interner, &mut inline_modules);

        let loaded = LoadedModule {
            path: mod_path,
            file_path: file_path.clone(),
            defs,
            imports,
            submodules: submodules.clone(),
        };

        self.loading.pop();
        self.cache.insert(path_key, loaded);

        // Register any inline `mod Foo { ... }` blocks as synthetic LoadedModules
        for inline_mod in inline_modules {
            let key = inline_mod.path.to_string_with(&interner.borrow());
            self.cache.insert(key, inline_mod);
        }

        // Load file-based submodules
        for (_, _, child_path_key) in submodules {
            if !self.cache.contains_key(&child_path_key) {
                let child_segments: Vec<&str> = child_path_key.split('.').collect();
                self.load(&child_segments, interner)?;
            }
        }

        Ok(())
    }

    /// Parse a single standalone source string into a root `LoadedModule` plus any inline submodules.
    pub fn load_single_source(
        &mut self,
        file_name: &Path,
        source: &str,
        interner: &RefCell<Interner>,
    ) -> Result<(), ModuleError> {
        let root_path = ModulePath::new(vec![]);
        let file_module = self.parse_file_module(file_name, source, &root_path, interner)?;
        let mut inline_modules = Vec::new();
        let (defs, imports, submodules) =
            self.extract_items(&file_module, file_name, interner, &mut inline_modules);

        let root_loaded = LoadedModule {
            path: root_path,
            file_path: file_name.to_path_buf(),
            defs,
            imports,
            submodules,
        };
        self.cache.insert(String::new(), root_loaded);
        for inline_mod in inline_modules {
            let key = inline_mod.path.to_string_with(&interner.borrow());
            self.cache.insert(key, inline_mod);
        }
        self.load_builtin_imports(interner)?;
        Ok(())
    }

    pub fn load_entry(
        &mut self,
        file_path: &Path,
        interner: &RefCell<Interner>,
    ) -> Result<(), ModuleError> {
        let module_segments = self.path_to_segments(file_path)?;
        let segments_refs: Vec<&str> = module_segments.iter().map(|s| s.as_str()).collect();
        self.load(&segments_refs, interner)?;
        self.load_builtin_imports(interner)?;
        Ok(())
    }

    /// Transitively load any embedded `std.*` / `sys.*` modules referenced by `use` or `open` statements.
    pub fn load_builtin_imports(
        &mut self,
        interner: &RefCell<Interner>,
    ) -> Result<(), ModuleError> {
        loop {
            let mut needed: Vec<String> = Vec::new();
            {
                let inter = interner.borrow();
                for m in self.cache.values() {
                    for (_, use_item) in &m.imports {
                        let target = import_target_module_key(use_item, &inter);
                        if !target.is_empty()
                            && !self.cache.contains_key(&target)
                            && builtin_module_source(&target).is_some()
                            && !needed.contains(&target)
                        {
                            needed.push(target);
                        }
                    }
                }
            }

            if needed.is_empty() {
                break;
            }

            needed.sort();
            for key in needed {
                let src = builtin_module_source(&key).unwrap();
                let virt_path = PathBuf::from(format!("<stdlib>/{}.gs", key.replace('.', "/")));
                let mod_path = {
                    let mut inter = interner.borrow_mut();
                    let segs: Vec<Id> = key.split('.').map(|s| inter.intern(s)).collect();
                    ModulePath::new(segs)
                };
                let fm = self.parse_file_module(&virt_path, src, &mod_path, interner)?;
                let mut inline_mods = Vec::new();
                let (defs, imports, submodules) =
                    self.extract_items(&fm, &virt_path, interner, &mut inline_mods);
                self.cache.insert(
                    key,
                    LoadedModule {
                        path: mod_path,
                        file_path: virt_path,
                        defs,
                        imports,
                        submodules,
                    },
                );
                for im in inline_mods {
                    let ikey = im.path.to_string_with(&interner.borrow());
                    self.cache.insert(ikey, im);
                }
            }
        }
        Ok(())
    }

    pub fn get(&self, path_key: &str) -> Option<&LoadedModule> {
        self.cache.get(path_key)
    }

    pub fn loaded_modules(&self) -> Vec<&LoadedModule> {
        let mut modules: Vec<_> = self.cache.values().collect();
        modules.sort_by(|a, b| a.file_path.cmp(&b.file_path));
        modules
    }

    pub fn is_loaded(&self, path_key: &str) -> bool {
        self.cache.contains_key(path_key)
    }

    pub fn module_paths(&self) -> Vec<String> {
        self.cache.keys().cloned().collect()
    }

    pub fn path_to_segments(&self, file_path: &Path) -> Result<Vec<String>, ModuleError> {
        let relative = file_path
            .strip_prefix(&self.source_root)
            .unwrap_or(file_path);

        let stem = relative.with_extension("");
        let final_path = if stem.file_name().map(|n| n == "mod").unwrap_or(false) {
            stem.parent().unwrap_or(&stem).to_path_buf()
        } else {
            stem
        };

        let segments: Vec<String> = final_path
            .components()
            .filter_map(|c| c.as_os_str().to_str().map(String::from))
            .collect();

        if segments.is_empty() {
            let name = file_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("main");
            Ok(vec![name.to_string()])
        } else {
            Ok(segments)
        }
    }

    fn read_file(&self, path: &Path) -> Result<String, ModuleError> {
        let mut file = fs::File::open(path).map_err(|e| ModuleError::IoError {
            path: path.to_path_buf(),
            message: e.to_string(),
        })?;
        let mut source = String::new();
        file.read_to_string(&mut source)
            .map_err(|e| ModuleError::IoError {
                path: path.to_path_buf(),
                message: e.to_string(),
            })?;
        Ok(source)
    }

    fn parse_file_module(
        &self,
        path: &Path,
        source: &str,
        mod_path: &ModulePath,
        interner: &RefCell<Interner>,
    ) -> Result<FileModule, ModuleError> {
        parser::parse_file_module(source, mod_path, interner).map_err(|e| {
            ModuleError::ParseError {
                path: path.to_path_buf(),
                message: e.to_string(),
            }
        })
    }

    fn extract_inline_module(
        &self,
        parent_segments: &[Id],
        mod_def: &ModuleDef,
        file_path: &Path,
        interner: &RefCell<Interner>,
        inline_out: &mut Vec<LoadedModule>,
    ) -> String {
        let mut segs = parent_segments.to_vec();
        segs.push(mod_def.name);
        let inline_path = ModulePath::new(segs);
        let inline_fm = FileModule {
            path: inline_path.clone(),
            items: mod_def.items.clone(),
        };
        let (defs, imports, submodules) =
            self.extract_items(&inline_fm, file_path, interner, inline_out);
        let path_str = inline_path.to_string_with(&interner.borrow());
        inline_out.push(LoadedModule {
            path: inline_path,
            file_path: file_path.to_path_buf(),
            defs,
            imports,
            submodules,
        });
        path_str
    }

    fn extract_items(
        &self,
        file_module: &FileModule,
        file_path: &Path,
        interner: &RefCell<Interner>,
        inline_out: &mut Vec<LoadedModule>,
    ) -> (
        Vec<(Visibility, Def)>,
        Vec<(Visibility, UseItem)>,
        Vec<(Visibility, Id, String)>,
    ) {
        let mut defs = Vec::new();
        let mut imports = Vec::new();
        let mut submodules = Vec::new();

        let parent_path = file_module.path.to_string_with(&interner.borrow());

        for item in &file_module.items {
            match item {
                ModuleItem::Def(vis, def) => {
                    defs.push((*vis, def.clone()));
                }
                ModuleItem::Use(vis, use_item) => {
                    imports.push((*vis, use_item.clone()));
                }
                ModuleItem::SubModule(vis, mod_def) => {
                    let child_path = self.extract_inline_module(
                        &file_module.path.segments,
                        mod_def,
                        file_path,
                        interner,
                        inline_out,
                    );
                    submodules.push((*vis, mod_def.name, child_path));
                }
                ModuleItem::ModDecl(vis, name) => {
                    let child_path = if parent_path.is_empty() {
                        interner.borrow().trace(*name).to_string()
                    } else {
                        format!("{}.{}", parent_path, interner.borrow().trace(*name))
                    };
                    submodules.push((*vis, *name, child_path));
                }
            }
        }

        (defs, imports, submodules)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn create_temp_project() -> (tempfile::TempDir, PathBuf) {
        let dir = tempfile::tempdir().unwrap();
        let src_root = dir.path().join("src");
        fs::create_dir_all(&src_root).unwrap();
        (dir, src_root)
    }

    fn write_file(path: &Path, content: &str) {
        let parent = path.parent().unwrap();
        fs::create_dir_all(parent).unwrap();
        let mut f = fs::File::create(path).unwrap();
        f.write_all(content.as_bytes()).unwrap();
    }

    #[test]
    fn test_load_module_with_submodule() {
        let (_dir, src_root) = create_temp_project();
        let mut loader = ModuleLoader::new(src_root.clone());
        let interner = RefCell::new(Interner::new());

        write_file(&src_root.join("utils/mod.gs"), "pub mod math");
        write_file(&src_root.join("utils/math.gs"), "pub def add(x, y) = x + y");

        loader.load(&["utils"], &interner).unwrap();
        assert!(loader.is_loaded("utils"));
        assert!(loader.is_loaded("utils.math"));
    }

    #[test]
    fn test_inline_submodule_extraction() {
        let (_dir, src_root) = create_temp_project();
        let mut loader = ModuleLoader::new(src_root.clone());
        let interner = RefCell::new(Interner::new());
        let src = "
mod Math {
    pub def abs(x) = if (x < 0) -x else x
}
def main() = Math.abs(-5)
";
        loader
            .load_single_source(Path::new("test.gs"), src, &interner)
            .unwrap();
        assert!(loader.is_loaded("Math"));
        assert!(loader.is_loaded(""));
    }
}
