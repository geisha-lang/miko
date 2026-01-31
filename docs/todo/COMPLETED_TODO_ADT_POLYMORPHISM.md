# ADT Polymorphism: Implementation Complete

This document previously tracked issues with ADT polymorphism. All issues have been **resolved** through the implementation of monomorphization.

## Resolved Issues

### ✅ Issue 1: Type Substitution Not Applied to AST

**Status**: Resolved

**Solution**: Monomorphization generates specialized versions of polymorphic functions with concrete types. The `SpecializeContext` struct tracks both type substitutions and function renames, ensuring all types in specialized function bodies are concrete.

---

### ✅ Issue 2: Polymorphic Return Value Coercion (i64 ↔ i32)

**Status**: Resolved

**Solution**: Polymorphic functions are now monomorphized, so there are no longer i64 values representing type variables at runtime. All functions operate on concrete types directly. The `create_result_phi` helper handles any remaining type mismatches in pattern matching.

---

### ✅ Issue 3: Void Phi Nodes in Pattern Matching

**Status**: Resolved

**Solution**: The `create_result_phi` helper in `emit.rs` checks for void types and returns a dummy value instead of creating an invalid phi node.

---

### ✅ Issue 4: Let Binding Alloca Type Mismatch

**Status**: Resolved

**Solution**: With monomorphization, function parameters and let bindings have concrete types. The Let case in codegen also uses the actual value's LLVM type for ADT allocations.

---

## Implementation Summary

### Key Changes

1. **`src/core/term.rs`**: Added `type_args` field to `MakeData` term

2. **`src/core/convert.rs`**:
   - Added `extract_type_args()` helper
   - Added `SpecializeContext` for tracking type substitutions and function renames
   - `specialize_function()` generates specialized function versions
   - `generate_specializations()` creates all needed specializations

3. **`src/codegen/emit.rs`**:
   - `gen_top_level()` skips polymorphic functions with type variables
   - `MakeData` uses `gen_instantiated_user_type()` for correct memory layout
   - `create_result_phi()` handles void types and type mismatches

4. **`src/typeinfer/infer.rs`**:
   - `collect_instantiations()` records concrete type arguments for polymorphic calls

### Test Coverage

All ADT tests pass:
- `adt_list.gs` - List with length function
- `adt_maybe.gs` - Maybe with fromMaybe
- `adt_pair.gs` - Pair with fst, snd, swap
- `adt_tree.gs` - Recursive Tree with sumTree
