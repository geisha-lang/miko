# Type Class (Concepts) Implementation Issue

## Status: PENDING

## Problem

The `concept_eq.gs` test file fails to compile correctly. This is a pre-existing issue with the type class (concepts) implementation, unrelated to the ADT polymorphism monomorphization work.

## Test File

```geisha
concept Eq a {
    eq: a * a -> Bool
}

instance Eq Int {
    def eq(x, y) = x == y
}

def main() = if (eq(1, 1)) putNumber(42) else putNumber(0)
```

## Expected Output

```
42
```

## Current Behavior

The file fails during compilation or produces incorrect output.

## Root Cause Analysis

The concepts/type class system needs further implementation work:

1. **Dictionary passing**: Instance methods need to be passed as dictionaries to functions with constrained types
2. **Instance resolution**: The compiler needs to resolve which instance to use based on concrete types
3. **Method lookup**: Generated code needs to properly call instance methods through the dictionary

## Affected Features

- `concept` declarations
- `instance` implementations
- Constrained polymorphism (`forall a. (Eq a) => ...`)

## Workaround

Currently, avoid using concepts. Use concrete types or unconstrained polymorphism instead.

## Related Files

- `src/syntax/parser/mod.rs` - Concept/instance parsing
- `src/typeinfer/infer.rs` - Type class constraint handling
- `src/core/convert.rs` - Dictionary passing (not yet implemented)
- `src/codegen/emit.rs` - Instance method codegen

## Priority

Medium - Type classes are a valuable feature but the language is usable without them.
