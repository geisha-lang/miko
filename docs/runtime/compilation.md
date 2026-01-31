# Compilation Pipeline

This document describes how Geisha source code is compiled to native executables.

## Pipeline Overview

```
Source Code (.gs)
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
   │ Linker │  Link with base.o → Executable
   └────────┘
```

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
```

## Stage 2: Type Inference

The type inference phase assigns types to all expressions.

### Implementation

- Located in `src/typeinfer/`
- Implements Hindley-Milner algorithm with extensions

### Process

1. **Environment Setup**: Initialize with built-in types and functions
2. **Constraint Generation**: Traverse AST, generate type constraints
3. **Unification**: Solve constraints using Robinson's algorithm
4. **Generalization**: Generalize function types to polymorphic schemes

### Outputs

- Type annotations for all expressions
- ADT registry with constructor types
- Concept/instance registry

## Stage 3: K-Conversion

K-conversion transforms the AST into a core intermediate representation with explicit closure handling.

### Purpose

- Make free variable capture explicit
- Distinguish between closure calls and direct calls
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

## Stage 4: Code Generation

The code generator emits LLVM IR from core terms.

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

## Stage 5: Object File Generation

LLVM compiles the IR to native object code.

### Process

1. LLVM optimizations (if enabled)
2. Target-specific code generation
3. Object file emission (.o)

### Command

```bash
./target/debug/miko -o output.o input.gs
```

## Stage 6: Linking

The object file must be linked with the runtime library.

### Runtime Library

`base.o` contains:
- `printLn`: Print string with newline
- `putNumber`: Print integer
- `putFloat`: Print float
- `putChar`: Print character

### Linking Command

```bash
cc -o output base.o output.o
```

## Complete Workflow

```bash
# 1. Build the compiler
cargo build

# 2. Compile source to object file
./target/debug/miko -o program.o program.gs

# 3. Link with runtime
cc -o program base.o program.o

# 4. Run
./program
```

## Debugging

### Emit LLVM IR

```bash
./target/debug/miko -e program.gs
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
| `src/codegen/emit.rs` | LLVM emission |
| `src/lib/base.c` | Runtime library source |
