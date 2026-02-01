# Compilation Pipeline

This document describes how Geisha source code is compiled to native executables.

## Pipeline Overview

```
Source Code (.gs)
       │
       ▼
   ┌──────────────┐
   │ Module Loader │  Load multi-file projects (optional)
   └──────────────┘
       │
       ▼
   ┌────────┐
   │ Parser │  PEG grammar → AST
   └────────┘
       │
       ▼
   ┌──────────────┐
   │ Type Inference │  Hindley-Milner → Typed AST
   └──────────────┘
       │
       ▼
   ┌──────────────┐
   │ K-Conversion │  Closure conversion → Core terms
   └──────────────┘
       │
       ▼
   ┌─────────────────┐
   │ Escape Analysis │  Determine allocation strategies
   └─────────────────┘
       │
       ▼
   ┌──────────────┐
   │ Code Generation │  → LLVM IR
   └──────────────┘
       │
       ▼
   ┌──────────────┐
   │ LLVM Backend │  → Object file (.o)
   └──────────────┘
       │
       ▼
   ┌────────┐
   │ Linker │  Link with runtime → Executable
   └────────┘
```

## Compilation Modes

### Single-File Compilation

```bash
./miko -o output input.gs
```

Compiles a single `.gs` file to an executable.

### Multi-File Project Compilation

```bash
./miko --src-root ./src -o output src/main/mod.gs
```

Compiles a multi-file project with module system support:
- `--src-root`: Base directory for module resolution
- Entry file: The main module to compile

## Stage 0: Module Loading (Multi-File)

For projects using `--src-root`, the module loader handles multi-file compilation.

### Implementation

Located in `src/modules/`:
- `loader.rs`: File discovery and loading
- `deps.rs`: Dependency graph and compilation order
- `imports.rs`: Import resolution and visibility checking

### Process

1. **Entry Module**: Load the entry file specified on command line
2. **Submodule Discovery**: Process `mod foo` declarations to find dependencies
3. **Recursive Loading**: Load submodules from `foo.gs` or `foo/mod.gs`
4. **Import Resolution**: Resolve `use` statements to qualified names
5. **Visibility Checking**: Verify imported items are public

### File Resolution

For `mod foo` in `src/main/mod.gs`, the loader tries:
1. `src/main/foo.gs`
2. `src/main/foo/mod.gs`

### Module Path Mapping

```
src/main/mod.gs       → module "main"
src/main/math.gs      → module "main.math"
src/main/utils/mod.gs → module "main.utils"
```

### Import Resolution

```
// main/mod.gs
mod math
use math.{add, mul}      // Resolves to main.math.add, main.math.mul

pub def main() = add(2, 3)
```

The import map tracks local name → qualified name mappings, passed through type inference and K-conversion.

## Stage 1: Parsing

The parser transforms source text into an Abstract Syntax Tree (AST).

### Parser Implementation

- Uses a PEG (Parsing Expression Grammar) via the `peg` crate
- Grammar defined inline in `src/syntax/parser/mod.rs`
- String interning via `RefCell<Interner>` for efficient symbol handling

### AST Structure

Key AST types from `src/syntax/form.rs`:

```rust
// Top-level definitions
enum Def {
    Value { name, params, body, ... },
    Data { name, variants, ... },
    Concept { name, methods, ... },
    Instance { concept, type, methods, ... },
}

// Expressions
enum Expr {
    Lit(Literal),
    Var(Symbol),
    QualifiedVar(ModulePath),  // module.name access
    App(func, args),
    Lambda(params, body),
    Let(name, value, body),
    If(cond, then, else),
    Match(expr, arms),
    Block(statements),
    ...
}

// Patterns
enum Pattern {
    Wildcard,
    Var(Symbol),
    Lit(Literal),
    Constructor(name, fields),
}

// Module items
enum ModuleItem {
    Def(Visibility, Def),
    Use(Visibility, UseItem),
    SubModule(Visibility, ModuleDef),
    ModDecl(Visibility, Id),
}
```

## Stage 2: Type Inference

The type inference phase assigns types to all expressions.

### Implementation

- Located in `src/typeinfer/`
- Implements Hindley-Milner algorithm with extensions

### Process

1. **Environment Setup**: Initialize with built-in types and functions
2. **Import Integration**: Add import aliases to type environment
3. **Constraint Generation**: Traverse AST, generate type constraints
4. **Unification**: Solve constraints using Robinson's algorithm
5. **Generalization**: Generalize function types to polymorphic schemes
6. **Instantiation Tracking**: Record polymorphic function instantiations for monomorphization

### Outputs

- Type annotations for all expressions
- ADT registry with constructor types
- Concept/instance registry
- Instantiation registry for monomorphization

## Stage 3: K-Conversion

K-conversion transforms the AST into a core intermediate representation with explicit closure handling.

### Purpose

- Make free variable capture explicit
- Distinguish between closure calls and direct calls
- Resolve import aliases to qualified names
- Prepare for efficient code generation

### Implementation

Located in `src/core/convert.rs`.

### Core Terms

```rust
enum Term {
    Lit(Literal),
    Var(Symbol),
    Let(name, value, body),
    If(cond, then, else),
    Binary(op, left, right),
    Match(expr, arms),

    // Closure operations
    MakeCls(name, closure, body),
    ApplyCls(func, args),
    ApplyDir(func, args),

    // ADT operations
    MakeData(name, tag, fields),
    GetTag(expr),
    GetField(expr, index),
}

struct Closure {
    entry: Symbol,      // Global function name
    free_vars: Vec<Symbol>, // Captured variables
}
```

### Transformation Example

Source:
```
def makeAdder(n) = (x) -> n + x
```

After K-conversion (conceptually):
```
// Global entry function
def _lambda_entry(x, fv) = fv.n + x

// Original function becomes:
def makeAdder(n) = MakeCls(closure, Closure(_lambda_entry, [n]))
```

## Stage 4: Escape Analysis

Escape analysis determines which values can be safely stack-allocated versus those that must be heap-allocated.

### Implementation

Located in `src/core/escape.rs`.

### Purpose

- Optimize memory allocation by using stack instead of GC heap when safe
- Reduce garbage collection pressure
- Improve performance for short-lived values

### Two-Phase Analysis

1. **Parameter Escape Analysis**: Bottom-up traversal of call graph to determine how function parameters escape:
   - `NoEscape`: Parameter is used locally only
   - `EscapesCapture`: Parameter is captured by a closure
   - `EscapesReturn`: Parameter is returned from the function
   - `EscapesStore`: Parameter is stored in a heap-allocated ADT

2. **Local Allocation Analysis**: For each function, determine which local bindings can be stack-allocated:
   - Closures (MakeCls) that don't escape → stack
   - ADT values (MakeData) that don't escape → stack
   - Recursive ADTs always → heap (can grow unboundedly)

### Escape Rules

A value must be heap-allocated if:
- Captured by a closure (pointer types)
- Returned from function (in tail position)
- Stored in an escaping or recursive ADT
- Passed to a callee's escaping parameter
- Is a recursive ADT (e.g., List, Tree)

### Output

`EscapeAnalysis` struct containing:
- Per-function allocation strategies for local bindings
- Set of recursive ADT names

## Stage 5: Code Generation

The code generator emits LLVM IR from core terms, using escape analysis results to choose allocation strategies.

### Implementation

- Located in `src/codegen/emit.rs`
- Uses `llvm-sys` crate for LLVM 14 bindings

### Type Mapping

| Geisha Type | LLVM Type |
|-------------|-----------|
| Int | i32 |
| Float | double |
| Bool | i1 |
| String | i8* |
| Void | void |
| Function | {fn_ptr, env_ptr}* |
| ADT | {i32, payload}* |

### Function Compilation

Each function compiles to an LLVM function:

```llvm
define i32 @add(i32 %x, i32 %y, i8* %fv) {
entry:
    %result = add i32 %x, %y
    ret i32 %result
}
```

The `fv` parameter holds the closure environment (free variables).

### Name Mangling

Module-qualified names are mangled for LLVM:

```
math.add : Int * Int -> Int
=> "_G4Mmath3Nadd_3Int3Int"
```

### Closure Representation

Closures are represented as structs containing captured values:

```llvm
; Closure for (x) -> n + x where n is captured
%closure = type { i32 }  ; Contains captured 'n'
```

### ADT Representation

ADTs use a tagged union:

```llvm
; data Maybe a { Nothing, Just(a) }
; Nothing: tag = 0, no payload
; Just(x): tag = 1, payload = x

%Maybe = type { i32, i8* }  ; {tag, payload_ptr}
```

## Stage 6: Object File Generation

LLVM compiles the IR to native object code.

### Process

1. LLVM optimizations (if enabled)
2. Target-specific code generation
3. Object file emission (.o)

### Command

```bash
./target/debug/miko -c -o output.o input.gs
```

## Stage 7: Linking

The object file is linked with the runtime library automatically.

### Runtime Library

The runtime (`src/lib/`) contains:
- `base.c`: I/O functions (`printLn`, `putNumber`, `putFloat`, `putChar`)
- `runtime/gc_bitmap.c`: Mark-and-sweep garbage collector

### Automatic Linking

When producing an executable (default), the compiler automatically links with the runtime:

```bash
./miko -o output input.gs  # Links automatically
```

For manual control:

```bash
./miko -c -o output.o input.gs          # Object file only
./miko --runtime /path/to/runtime.a ...  # Custom runtime path
```

## Complete Workflow

### Single File

```bash
# Build the compiler
cargo build

# Compile and link (one step)
./target/debug/miko -o program program.gs

# Run
./program
```

### Multi-File Project

```bash
# Project structure:
# src/main/mod.gs   - entry point with "mod math"
# src/main/math.gs  - math module with "pub def add"

# Compile
./target/debug/miko --src-root src -o program src/main/mod.gs

# Run
./program
```

## Debugging

### Emit LLVM IR

```bash
./target/debug/miko -e -o /dev/null program.gs
```

This prints the generated LLVM IR to stdout for inspection.

### Example IR Output

```llvm
; Function: factorial
define i32 @factorial(i32 %n, i8* %fv) {
entry:
    %cond = icmp eq i32 %n, 0
    br i1 %cond, label %then, label %else

then:
    ret i32 1

else:
    %n_minus_1 = sub i32 %n, 1
    %rec = call i32 @factorial(i32 %n_minus_1, i8* null)
    %result = mul i32 %n, %rec
    ret i32 %result
}
```

## Source File Organization

| File | Purpose |
|------|---------|
| `src/syntax/parser/mod.rs` | PEG grammar |
| `src/syntax/form.rs` | AST types |
| `src/types.rs` | Type definitions |
| `src/typeinfer/infer.rs` | Type inference |
| `src/typeinfer/constraint.rs` | Constraint solving |
| `src/core/convert.rs` | K-conversion |
| `src/core/term.rs` | Core IR types |
| `src/core/escape.rs` | Escape analysis |
| `src/codegen/emit.rs` | LLVM emission |
| `src/modules/loader.rs` | Module file loading |
| `src/modules/imports.rs` | Import resolution |
| `src/modules/deps.rs` | Dependency graph |
| `src/lib/base.c` | Runtime library (I/O) |
| `src/lib/runtime/gc_bitmap.c` | Garbage collector |
