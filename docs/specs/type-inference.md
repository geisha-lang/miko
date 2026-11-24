# Type Inference & Monomorphization

Geisha uses Hindley-Milner (HM) type inference with concept constraints, followed by demand-driven transitive monomorphization.

## Inference Pipeline

1. **Alias & ADT Registration**: Expand `type` aliases (detecting cycles), register `data` constructors, and register `concept` methods and `instance` implementations.
2. **Constraint Generation**: Traverse expressions, freshly instantiating polymorphic schemes at every variable, constructor, and operator site while recording equality constraints and wanted concept constraints.
3. **Unification (with Occurs Check)**: Solve equality constraints via Robinson's unification algorithm. Self-bindings `a ~ a` succeed trivially; cyclic bindings `a ~ a -> b` fail with an infinite-type error (`InfiniteType`).
4. **Concept Constraint Solving & Propagation**:
   - Concrete wanted constraints (e.g. `Eq Int`, `Eq (List Int)`, `Num Float`) are checked recursively against registered instances; missing instances fail at compile time (`NoInstance`).
   - Residual constraints on generalizable type variables propagate into the inferred `Scheme::PolyConstrained`.
5. **Deterministic Generalization**: Both top-level `def` and local `let` bindings generalize free type variables not free in the outer `TypeEnv`, preserving left-to-right variable order.

## Let-Polymorphism

Local `let` bindings are generalized so polymorphic helpers work across multiple types in the same scope:

```
def main() =
    let id = (x) -> x in {
        let n = id(42);
        let b = id(true);
        if (b) putNumber(n) else putNumber(0)
    }
```

## Demand-Driven Transitive Monomorphization

Geisha compiles all polymorphism—both unconstrained functions and `concept`-constrained functions—via **worklist monomorphization**:

1. Starting from `main` and top-level value initializers, the compiler inspects every call and function value reference whose types are now concrete.
2. **Polymorphic functions**: A call to `f` with concrete type arguments `[T1, T2, ...]` rewrites to `f_<mangled>` and enqueues `(f, [T1, T2, ...])` onto the specialization worklist. Because specialized bodies are themselves processed by the worklist, **transitive generic calls** (`def f(x) = g(x)`) and **nested closures inside generic functions** are specialized automatically.
3. **Concept method dispatch**: A call to concept method `m` (e.g., `eq(x, y)`) resolves the matching `instance` from the concrete argument/return types:
   - Monomorphic instances (`instance Eq Int`) rewrite directly to the concrete method function (`__inst_Eq_Int_eq`).
   - Generic/constrained instances (`instance (Eq a) => Eq (List a)`) rewrite to the specialized instance method (`__inst_Eq_List_eq_Int`) and enqueue it onto the worklist, recursively resolving inner constraints (`Eq Int`).
4. Once the worklist drains, unspecialized generic templates are discarded so LLVM only emits concrete native machine code with zero boxing or dictionary indirection.
