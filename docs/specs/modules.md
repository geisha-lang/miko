# Module System

The Geisha module system provides namespacing, visibility control, and import mechanisms inspired by Rust.

## Overview

The module system supports:
- **Visibility control**: `pub` modifier for public items, private by default
- **File-based modules**: Multi-file project compilation with `mod` declarations
- **Import mechanism**: `use` statements with multiple variants
- **Qualified names**: Direct access via `module.name` syntax

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

### Module Declarations

Declare submodules to load from separate files:

```
// main/mod.gs
mod math                        // loads math.gs as submodule
mod utils                       // loads utils.gs or utils/mod.gs

pub def main() = putNumber(math.add(2, 3))
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
math.add(2, 3)
```

### Import Statements

```
use math.add                            // single import
use math.{add, mul}                     // multiple imports
use math.add as plus                    // import with alias
open math                               // bring all public items into scope
pub use math.{add, mul}                 // re-export (make visible to other modules)
```

## Multi-File Compilation

### Project Structure

File path maps directly to module path:

```
project/
  src/
    main/
      mod.gs          # entry point: mod math, use math.{add, mul}
      math.gs         # pub def add, pub def mul
      utils/
        mod.gs        # submodule: pub def clamp
```

**Mapping:** `path/to/file.gs` → module `path.to.file`

### Usage

```bash
# Single-file compilation (traditional)
./miko -o output input.gs

# Multi-file project compilation
./miko --src-root ./src -o output src/main/mod.gs
```

### Example: Multi-File Project

**src/main/math.gs:**
```
pub def add(x, y) = x + y
pub def mul(x, y) = x * y
def secret(x) = x * x           // private - cannot be imported
```

**src/main/mod.gs:**
```
mod math
use math.{add, mul}

pub def main() = putNumber(mul(add(2, 3), 4))
```

**Compile and run:**
```bash
./miko --src-root src -o output src/main/mod.gs
./output
# Output: 20
```

## Visibility Checking

### Cross-Module Access

Private items cannot be imported from other modules:

```
// math.gs
pub def add(x, y) = x + y
def secret(x) = x * x            // private

// main.gs
use math.add                     // ✅ OK - add is public
use math.secret                  // ❌ Error: cannot import private item
```

**Error message:**
```
Compiling error:
Normal("cannot import private item 'secret' from module 'math'")
```

### Glob Import Respects Visibility

The `open` statement only imports public items:

```
// math.gs
pub def add(x, y) = x + y
def secret(x) = x * x

// main.gs
open math                        // imports only `add`, not `secret`
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
    ModDecl(Visibility, Id),           // module declaration
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
- Import resolution checks visibility and reports errors

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

### Phase 1: Core Module Syntax ✅ Complete

| Component | Status | File |
|-----------|--------|------|
| AST types (ModulePath, Visibility, etc.) | ✅ Done | `src/syntax/form.rs` |
| Parser for `pub` modifier | ✅ Done | `src/syntax/parser/mod.rs` |
| Parser for `use`/`open` statements | ✅ Done | `src/syntax/parser/mod.rs` |
| Parser for qualified names | ✅ Done | `src/syntax/parser/mod.rs` |
| Parser for inline `mod` | ✅ Done | `src/syntax/parser/mod.rs` |
| ModuleSymTable | ✅ Done | `src/utils.rs` |
| Type inference for QualifiedVar | ✅ Done | `src/typeinfer/infer.rs` |
| Core conversion for QualifiedVar | ✅ Done | `src/core/convert.rs` |
| Module name mangling | ✅ Done | `src/codegen/llvm.rs` |

### Phase 2: File-Based Modules ✅ Complete

| Component | Status | File |
|-----------|--------|------|
| Module loader | ✅ Done | `src/modules/loader.rs` |
| File path resolution | ✅ Done | Supports `foo.gs` or `foo/mod.gs` |
| Dependency graph | ✅ Done | `src/modules/deps.rs` |
| CLI --src-root | ✅ Done | `src/main.rs` |
| `mod` declaration linking | ✅ Done | Recursively loads submodules |
| `use` statement resolution | ✅ Done | `src/modules/imports.rs` |
| Multiple imports syntax | ✅ Done | `use foo.{a, b}` |
| Import aliases | ✅ Done | `use foo.bar as baz` |
| Glob imports | ✅ Done | `open foo` |
| Cross-file visibility | ✅ Done | Error on importing private items |

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

### Multi-File with Imports

**math.gs:**
```
pub def add(x, y) = x + y
pub def mul(x, y) = x * y
```

**mod.gs:**
```
mod math
use math.{add, mul}

pub def main() = putNumber(mul(add(2, 3), 4))
```

### Import with Alias

```
mod math
use math.add as plus

pub def main() = putNumber(plus(5, 10))
```

### Glob Import

```
mod math
open math

pub def main() = putNumber(mul(add(2, 3), 4))
```

## Testing

Run module-related tests:

```bash
cargo test module
cargo test visibility
cargo test qualified
cargo test use
```

Test multi-file compilation:
```bash
# Create project structure
mkdir -p /tmp/project/src/main
echo "pub def add(x, y) = x + y" > /tmp/project/src/main/math.gs
echo "mod math
use math.add
pub def main() = putNumber(add(2, 3))" > /tmp/project/src/main/mod.gs

# Compile and run
./target/debug/miko --src-root /tmp/project/src -o /tmp/project/out /tmp/project/src/main/mod.gs
/tmp/project/out
# Output: 5
```
