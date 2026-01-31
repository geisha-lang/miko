# ADT Polymorphism: Known Issues and Workarounds

This document captures issues discovered during the ADT polymorphism implementation that require deeper fixes.

## Issue 1: Type Substitution Not Applied to AST

**Location**: Type inference → K-conversion → Codegen

**Problem**: Type inference correctly unifies type variables with concrete types (e.g., `a = Int` for `Cons(1, Nil)`), but the substitution is not applied back to update the types stored in the AST/Scheme. This causes the scrutinee type in pattern matching to appear as `Type::Var(".3")` instead of the concrete type like `Pair` or `List Int`.

**Example**:
```
match x {
    MkPair(a, b) -> ...   // scrutinee type is Var(".3") instead of Pair
}
```

**Current Workaround** (`src/codegen/emit.rs`):
- `get_variant_field_types_by_ctor()` - Falls back to looking up variant field types directly from the constructor name when scrutinee type is a type variable
- Pattern matching uses the constructor name to infer the ADT type

**Proper Fix**: Apply the type substitution to all AST nodes after type inference completes. This could be done in:
- `src/typeinfer/infer.rs` after constraint solving
- Or add a separate AST rewriting pass

---

## Issue 2: Polymorphic Return Value Coercion (i64 ↔ i32)

**Location**: `src/codegen/emit.rs` - function calls and argument passing

**Problem**: Polymorphic type variables use `i64` as a universal representation (can hold both pointers via inttoptr and primitives via zext). However, when a polymorphic function returns a value and it's passed to a function expecting `i32` (like `putNumber`), LLVM generates an invalid bitcast:

```llvm
%arg.cast = bitcast i64 %result to i32   ; INVALID - bitcast can't change sizes
```

**Example that triggers this**:
```
def fromMaybe(d, opt) = match opt { None -> d, Some(x) -> x }
def main() = putNumber(fromMaybe(0, Some(43)))  ; fromMaybe returns i64, putNumber expects i32
```

**Current Workaround**: None - the warning is printed but compilation continues. Sometimes works at runtime due to calling conventions.

**Proper Fix**: Add type coercion logic in `src/codegen/emit.rs`:
- When passing a value to a function, check if source type is i64 (polymorphic) and target is i32
- Use `trunc` instruction instead of `bitcast` for i64→i32
- Use `sext`/`zext` for i32→i64
- For pointers: use `inttoptr`/`ptrtoint` appropriately

Relevant location: `ApplyDir` case around line 224-250 and `ApplyCls` case.

---

## Issue 3: Void Phi Nodes in Pattern Matching

**Location**: `src/codegen/emit.rs` - Match expression codegen

**Problem**: When all match arms return void (e.g., calling `putNumber` which returns void), the phi node at the merge block has void type, which is invalid in LLVM:

```llvm
%match.result = phi void [ <badref>, %match.arm.0 ], [ <badref>, %match.arm.1 ]
```

**Current Workaround**: LLVM prints a warning but compilation succeeds.

**Proper Fix**: In the Match codegen (around line 433-500):
- Check if result type is void before creating the phi node
- If void, skip the phi node and just branch to continuation
- Return an undef or unit value instead

---

## Issue 4: Let Binding Alloca Type Mismatch

**Location**: `src/codegen/emit.rs` - Let expression codegen

**Problem**: When allocating space for an ADT variable, the declared scheme type may still have unresolved type variables (using i64 fallback), while the actual MakeData value has concrete instantiated types (e.g., i32 for Int).

**Example**:
```
let x = Some(42) in ...
; Declared type: Maybe a → struct { i32, { i64 } }
; Actual value:  Some(42) → struct { i32, { i32 } }
; Store fails due to type mismatch
```

**Current Workaround** (`src/codegen/emit.rs:181-195`):
- For ADT types, use the actual value's LLVM type for the alloca instead of the declared scheme type

**Proper Fix**: Same as Issue 1 - apply type substitution to update scheme types after inference.

---

## Priority Order for Fixes

1. **Issue 1** (Type Substitution) - Fixing this would resolve Issues 1 and 4, and partially help with Issue 2
2. **Issue 2** (i64/i32 Coercion) - Needed for polymorphic functions to work correctly with primitive types
3. **Issue 3** (Void Phi) - Minor, only affects match expressions that return void

---

## Files to Modify

| Issue | Primary Files |
|-------|--------------|
| 1 | `src/typeinfer/infer.rs`, possibly new `src/typeinfer/substitute.rs` |
| 2 | `src/codegen/emit.rs` (ApplyDir, ApplyCls, convert_to_field_type) |
| 3 | `src/codegen/emit.rs` (Match case) |
| 4 | Resolved by Issue 1, or keep current workaround |
