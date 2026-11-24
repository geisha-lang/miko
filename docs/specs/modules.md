# Module System

The Geisha module system provides namespacing, `pub` visibility control, inline submodules, and multi-file compilation with topological dependency ordering.

## Syntax

```
// Visibility (private by default)
pub def add(x, y) = x + y
def secret(x) = x * x

// Inline submodule (supported in both single-file and multi-file modes)
mod Math {
    pub def abs(x) = if (x < 0) -x else x
}

// File-based submodule declaration
mod utils

// Imports (supported in entry modules and submodules alike)
use math.add
use math.{add, mul}
use math.add as plus
open math
pub use math.{add, mul}
```

## Resolution & Visibility Rules

1. **Topological Ordering**: Multi-file compilation constructs a `DependencyGraph` across `mod` and `use` edges, detects circular module dependencies, and compiles modules in dependency order.
2. **Per-Module Scoped Imports**: Each module resolves its own `use`, `open`, and `pub use` re-exports independently without leaking local aliases into sibling modules.
3. **Visibility Enforcement**: Both `use`/`open` imports and direct qualified references (`math.secret(5)`) enforce `pub` visibility across module boundaries.
