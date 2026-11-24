# Compilation Pipeline

```
Source (.gs)
  → Module Loader & DependencyGraph (multi-file & inline modules, per-module imports)
  → PEG Parser (AST with spans, comments, tuples, ADTs, concepts)
  → Type Inference (HM + occurs check + let-polymorphism + type aliases + concept constraints + exhaustiveness)
  → K-Conversion & Worklist Monomorphization (explicit closures, transitive specialization, static concept dispatch)
  → Fixed-Point Escape Analysis (stack vs GC heap allocation, SCC recursive ADT detection)
  → LLVM 20 IR Emission (opaque `ptr`, New PassManager optimizations)
  → Object File + Runtime Linker (`libgeisha_runtime.a`)
```

## Key Stages

1. **Module Resolution (`src/modules/`)**: Discovers submodules, topologically sorts dependencies via `DependencyGraph`, resolves `use`/`open`/`pub use` per module, and checks `pub` visibility.
2. **Parsing (`src/syntax/parser/mod.rs`)**: Inline `peg::parser!` supporting `//` and `/* */` comments, right-associative `->` function types `(T1, T2) -> R`, first-class tuple types/literals/patterns `(a, b)`, `forall a. (C a) => ...`, `;`/newline blocks, unary `!`/`-`, list literals `[...]`, and struct ADT variants.
3. **Type Inference (`src/typeinfer/`)**: Expands `type` aliases, checks HM constraints with occurs check, generalizes `let` and `def` bindings deterministically, verifies concept constraints against concrete and generic instances, and checks `match` exhaustiveness.
4. **K-Conversion & Monomorphization (`src/core/convert.rs`)**: Converts lambdas to `MakeCls` / `ApplyCls` / `ApplyDir`, lowers top-level value globals, and runs a worklist from `main` to transitively specialize polymorphic functions, nested closures, and concept method calls.
5. **Escape Analysis (`src/core/escape.rs`)**: Uses fixed-point propagation so non-escaping closures and non-recursive ADTs allocate on the stack while anything escaping (including fields stored in escaping ADTs) allocates via `gc_alloc`.
6. **LLVM 20 Backend (`src/codegen/`, `libllvm/`)**: Emits LLVM 20 IR using opaque pointers (`ptr`) with explicit `(LLVMValue, LLVMType)` slot tracking and runs New PassManager optimizations (`mem2reg`, `instcombine`, `reassociate`, `gvn`, `simplifycfg`, `tailcallelim`).
