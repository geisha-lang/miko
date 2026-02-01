//! Module system infrastructure for multi-file compilation.
//!
//! This module provides:
//! - `ModuleLoader`: Finds and loads `.gs` source files based on module paths
//! - `DependencyGraph`: Resolves compilation order and detects circular dependencies
//! - `ImportMap`: Resolves `use` statements to bring names into scope

mod loader;
mod deps;
mod imports;

pub use loader::{ModuleLoader, ModuleError, LoadedModule};
pub use deps::{DependencyGraph, CycleError};
pub use imports::{ImportMap, NameInfo, ImportError, resolve_imports, resolve_imports_with_visibility};
