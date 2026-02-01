# Module System

The Geisha module system provides namespacing, visibility control, and import mechanisms inspired by Rust.

## Overview

The module system has two phases of implementation:
- **Phase 1 (Complete)**: Core module syntax - visibility modifiers, inline modules, qualified names
- **Phase 2 (Planned)**: File-based modules - multi-file compilation, module loader

## Syntax

### Visibility Modifiers

Items are private by default. Use `pub` to make them public:

```
pub def add(x, y) = x + y       // public function
def helper(x) = x * 2           // private function (default)

pub data Color { Red, Green, Blue }   // public ADT
data Internal { Secret(Int) }         // private ADT

pub concept Show a { ... }      // public concept
concept Helper a { ... }        // private concept
```

### Inline Submodules

For small modules defined within a file:

```
mod Utils {
    pub def clamp(x, lo, hi) =
        if (x < lo) lo
        else if (x > hi) hi
        else x
}
```

### Module Paths

Qualified access to module members:

```
collections.list.length(myList)
Utils.clamp(value, 0, 100)
```

### Import Statements

```
use collections.list                    // qualified access: list.length
use collections.list.{Nil, Cons, map}   // import specific items
use collections.list as L               // alias: L.length
open collections.list                   // bring all public items into scope
pub use collections.list.{Nil, Cons}    // re-export
```

## AST Types

The following types are defined in `src/syntax/form.rs`:

### ModulePath

Represents a dotted path like `collections.list.length`:

```rust
pub struct ModulePath {
    pub segments: Vec<Id>,
}
```

### Visibility

```rust
pub enum Visibility {
    Private,   // default
    Public,
}
```

### UseItem

Import specification:

```rust
pub enum UseItem {
    Single(ModulePath),                    // use Foo.bar
    Multiple(ModulePath, Vec<UseSpec>),    // use Foo.{bar, baz}
    Glob(ModulePath),                      // open Foo (use Foo.*)
    Alias(ModulePath, Id),                 // use Foo.bar as B
}
```

### ModuleDef

Inline module definition:

```rust
pub struct ModuleDef {
    pub name: Id,
    pub items: Vec<ModuleItem>,
    pub pos: Span,
}
```

### ModuleItem

Items that can appear in a module:

```rust
pub enum ModuleItem {
    Def(Visibility, Def),              // function/value definition
    Use(Visibility, UseItem),          // import statement
    SubModule(Visibility, Box<ModuleDef>),  // inline submodule
    ModDecl(Visibility, Id),           // module declaration (Phase 2)
}
```

### Expr Extension

Qualified variable access:

```rust
pub enum Expr {
    // ... existing variants ...
    QualifiedVar(ModulePath),  // module.name access
}
```

## Symbol Resolution

`ModuleSymTable` in `src/utils.rs` provides module-aware symbol resolution:

```rust
pub struct ModuleSymTable<'a, T: Clone> {
    local: SymTable<'a, Id, ModuleSymbol<T>>,
    modules: HashMap<Vec<Id>, HashMap<Id, ModuleSymbol<T>>>,
    imports: HashMap<Id, ImportEntry>,
    glob_imports: Vec<Vec<Id>>,
    current_path: Vec<Id>,
}
```

### Lookup Order

1. Local scope
2. Imported names (via `use`)
3. Glob imports (via `open`)
4. Qualified paths (explicit module.name)

### Visibility Checking

- Private items are only visible within the same module
- Public items are visible from any module
- Re-exports (`pub use`) make imported items public

## Name Mangling

Module-qualified names are mangled for LLVM symbols:

```
collections.list.length : List Int -> Int
=> "_G11Mcollections4Mlist6Nlength_7ListInt"
```

Format: `_G{len}M{segment}...{len}N{name}[_{type_suffix}]`

Functions in `src/codegen/llvm.rs`:
- `mangle_module_name(module_path, name)` - basic mangling
- `mangle_module_name_with_types(module_path, name, type_args)` - with type suffix
- `demangle_module_name(mangled)` - reverse operation

## Implementation Status

### Phase 1: Core Module Syntax (Complete)

| Component | Status | File |
|-----------|--------|------|
| AST types (ModulePath, Visibility, etc.) | Done | `src/syntax/form.rs` |
| Parser for `pub` modifier | Done | `src/syntax/parser/mod.rs` |
| Parser for `use`/`open` statements | Done | `src/syntax/parser/mod.rs` |
| Parser for qualified names | Done | `src/syntax/parser/mod.rs` |
| Parser for inline `mod` | Done | `src/syntax/parser/mod.rs` |
| ModuleSymTable | Done | `src/utils.rs` |
| Type inference for QualifiedVar | Done | `src/typeinfer/infer.rs` |
| Core conversion for QualifiedVar | Done | `src/core/convert.rs` |
| Module name mangling | Done | `src/codegen/llvm.rs` |
| Unit tests | Done | Various test modules |

### Phase 2: File-Based Modules (Planned)

| Component | Status | Notes |
|-----------|--------|-------|
| Module loader | Not started | Load `.gs` files based on module path |
| `mod filename` syntax | Parser done | Links to file-based modules |
| File path resolution | Not started | `foo.gs` or `foo/mod.gs` |
| Dependency resolution | Not started | Topological sort for compilation order |
| Cross-file visibility | Not started | Check visibility across file boundaries |
| Circular import detection | Not started | Prevent circular dependencies |

## File Organization (Phase 2 Design)

File path maps directly to module path:

```
src/
  main.gs              # entry point (root module)
  collections.gs       # module collections
  collections/
    mod.gs             # module collections (alternative)
    list.gs            # module collections.list
    map.gs             # module collections.map
```

**Mapping:** `path/to/file.gs` -> module `path.to.file`

**Parent module declares submodules** (`collections/mod.gs`):
```
pub mod list    // loads list.gs as collections.list
pub mod map     // loads map.gs as collections.map

// Re-exports for convenience
pub use list.{List, Nil, Cons}
```

## Examples

### Basic Visibility

```
// math.gs
pub def add(x, y) = x + y
pub def multiply(x, y) = x * y
def square(x) = multiply(x, x)  // private helper

pub def cube(x) = multiply(square(x), x)
```

### ADT with Visibility

```
pub data Option a {
    None,
    Some(a)
}

pub def map(opt, f) = match opt {
    None -> None,
    Some(x) -> Some(f(x))
}
```

### Inline Module

```
mod Math {
    pub def abs(x) = if (x < 0) (0 - x) else x
    pub def max(a, b) = if (a > b) a else b
}

def main() = Math.max(Math.abs(0 - 5), 3)
```

## Testing

Run module-related tests:

```bash
cargo test module
cargo test visibility
cargo test qualified
```

Test programs:
```bash
# Compile with visibility modifiers
echo "pub def main() = putNumber(42)" > /tmp/test.gs
./target/debug/miko -o /tmp/test /tmp/test.gs
/tmp/test
```
