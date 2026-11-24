# COMPLETED: Type Class (Concepts) Static Monomorphization

## Status: COMPLETED

## Summary

`concept` and `instance` declarations (including generic/constrained instances like `instance (Eq a) => Eq (List a)`) are now fully type-checked and compiled via **demand-driven static monomorphization** rather than runtime dictionary passing:

1. **Type Inference (`src/typeinfer/infer.rs`)**:
   - Registers `ConceptInfo` and `InstanceInfo` metadata and synthesizes typed global functions for every instance method so method implementations are verified against their concept signature.
   - Emits and discharges `WantedConstraint`s across concrete types, generic instances, superclasses, and constrained `forall` annotations (`forall a. (Eq a) => ...`).
   - Built-in operators (`+`, `-`, `*`, `/`, `==`, `!=`, `<`, `<=`, `>`, `>=`, unary `-`) are constrained via built-in numeric/equality/ordering concepts so invalid operations like `"a" + "b"` fail at compile time with `NoInstance`.

2. **Worklist Monomorphization (`src/core/convert.rs`)**:
   - Resolves concept method calls statically from concrete argument and return types at each monomorphized call site, transitively specializing generic instance methods (`__inst_Eq_ListOfa_eq_Int`) with zero runtime dictionary overhead.

3. **Verification**:
   - `test/concept_eq.gs` compiles and outputs `42`.
   - End-to-end tests in `tests/e2e.rs` verify concrete instances, generic constrained instances (`Eq (Maybe a)`, `Eq (List a)`), constrained functions (`lookup`), and `NoInstance` compile-time rejection.
