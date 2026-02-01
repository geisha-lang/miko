# TODO: Module System Phase 2 - File-Based Modules

## Overview

Phase 1 of the module system (core syntax) is complete. Phase 2 implements file-based modules for multi-file project compilation.

## Current State

Phase 1 (Complete):
- Visibility modifiers (`pub` for public, default private)
- Inline submodule syntax (`mod Name { ... }`)
- Import statements (`use`, `open`, `pub use`)
- Qualified name access (`module.name`)
- Module-aware symbol table (`ModuleSymTable`)
- Name mangling for LLVM symbols

## Phase 2 Requirements

### 1. Module Loader

**Location:** New file `src/modules/loader.rs` or in `src/main.rs`

**Responsibilities:**
- Given a module path (e.g., `collections.list`), find the corresponding `.gs` file
- Handle both `foo.gs` and `foo/mod.gs` patterns
- Cache parsed modules to avoid reparsing
- Track which modules have been loaded

**File Resolution Rules:**
```
collections.list ->
  1. Try: src/collections/list.gs
  2. Try: src/collections/list/mod.gs
  3. Error if neither exists
```

**Interface:**
```rust
pub struct ModuleLoader {
    source_root: PathBuf,
    loaded: HashMap<ModulePath, FileModule>,
}

impl ModuleLoader {
    pub fn load(&mut self, path: &ModulePath) -> Result<&FileModule, ModuleError>;
    pub fn load_all(&mut self) -> Result<Vec<&FileModule>, ModuleError>;
}
```

### 2. Module Declaration Parsing

**Status:** Parser rule exists (`mod_declaration`)

**Remaining work:**
- When encountering `mod foo`, trigger file loading
- Link the loaded module to the current module's namespace
- Handle `pub mod` for re-exporting submodules

### 3. Dependency Resolution

**Goal:** Determine correct compilation order

**Algorithm:**
1. Start from entry point (e.g., `main.gs`)
2. Parse to find all `use` and `mod` declarations
3. Build dependency graph
4. Topological sort to get compilation order
5. Detect and report circular dependencies

**Data Structures:**
```rust
pub struct DependencyGraph {
    nodes: HashMap<ModulePath, ModuleInfo>,
    edges: HashMap<ModulePath, Vec<ModulePath>>,  // depends_on
}

impl DependencyGraph {
    pub fn add_module(&mut self, path: ModulePath, deps: Vec<ModulePath>);
    pub fn topological_sort(&self) -> Result<Vec<ModulePath>, CycleError>;
}
```

### 4. Cross-File Visibility Checking

**Current state:** `ModuleSymTable` has visibility infrastructure

**Remaining work:**
- When resolving a qualified name across files, check visibility
- Private items should not be accessible from other files
- Error messages should indicate visibility violations

**Example error:**
```
Error: `helper` is private in module `collections.list`
  --> src/main.gs:5:10
   |
5  |   list.helper(x)
   |        ^^^^^^ private function
```

### 5. Integration with Type Inference

**Current state:** Single-file type inference works

**Remaining work:**
- Pass module context to type inference
- Resolve types from imported modules
- Handle type visibility (pub data types vs private)

### 6. Integration with Codegen

**Current state:** Name mangling implemented

**Remaining work:**
- Emit LLVM IR for all modules in correct order
- Handle cross-module function calls
- Ensure symbols are properly exported/imported

### 7. CLI Changes

**Current:** Single file input

**New options:**
```bash
# Compile project from entry point
./miko -o output src/main.gs

# Specify source root (for module resolution)
./miko --src-root ./src -o output main.gs

# Compile specific module
./miko --module collections.list -o list.o src/collections/list.gs
```

## Implementation Order

1. **Module Loader** - Core infrastructure for finding/loading files
2. **Dependency Resolution** - Build dependency graph, detect cycles
3. **Multi-file Parsing** - Parse all required modules
4. **Cross-file Type Inference** - Type check with module context
5. **Cross-file Codegen** - Generate code for all modules
6. **Visibility Enforcement** - Check visibility across boundaries
7. **CLI Integration** - Command-line support for projects

## Test Cases

### Basic Multi-File

```
// src/main.gs
use utils.math

def main() = putNumber(math.add(1, 2))

// src/utils/math.gs
pub def add(x, y) = x + y
```

### Visibility Enforcement

```
// src/main.gs
use internal  // should work

def main() = internal.helper(1)  // ERROR: helper is private

// src/internal.gs
def helper(x) = x  // private
pub def api(x) = helper(x)  // public
```

### Circular Dependency Detection

```
// src/a.gs
use b
pub def fromA() = b.fromB()

// src/b.gs
use a  // ERROR: circular dependency
pub def fromB() = a.fromA()
```

## Files to Modify

| File | Changes |
|------|---------|
| `src/main.rs` | Add module loader, multi-file compilation |
| `src/syntax/parser/mod.rs` | Connect `mod name` to file loading |
| `src/typeinfer/infer.rs` | Cross-module type resolution |
| `src/codegen/emit.rs` | Multi-module code generation |
| New: `src/modules/` | Module loader, dependency resolution |

## Notes

- Keep single-file compilation working (no breaking changes)
- Module loader should be optional - only used when `mod` declarations exist
- Consider incremental compilation later (check file timestamps)
- May want a project manifest file (like Cargo.toml) eventually
