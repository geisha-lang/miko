# Operators

This document describes all unary and binary operators in Geisha, their types, semantics, and precedence.

## Operator Precedence

Listed from highest to lowest precedence:

| Precedence | Operators | Associativity | Description |
|------------|-----------|---------------|-------------|
| Highest | `f(x)` | Left | Function application |
| 7 | `!`, `-` (unary) | Prefix | Logical NOT, arithmetic negation |
| 6 | `*`, `/`, `%` | Left | Multiplicative |
| 5 | `+`, `-` | Left | Additive |
| 4 | `<`, `<=`, `>`, `>=`, `==`, `!=` | Left | Comparison |
| 3 | `&&` | Left | Short-circuit logical AND |
| 2 | `\|\|` | Left | Short-circuit logical OR |

*(In type expressions, type application `List a` binds tighter than `->`, and `->` is **right-associative**.)*

## Unary Operators

- **`!x`** (`Bool -> Bool`): Logical negation (`!true == false`).
- **`-x`** (`forall a. (Num a) => a -> a`): Arithmetic negation for `Int` and `Float`.

## Arithmetic Operators

- **`+`, `-`, `*`, `/`**: `forall a. (Num a) => (a, a) -> a`
  - Supported for `Int` (`add`, `sub`, `mul`, signed `sdiv`) and `Float` (`fadd`, `fsub`, `fmul`, `fdiv`).
- **`%`**: `(Int, Int) -> Int` (signed integer remainder `srem`).

## Comparison Operators

- **`==`, `!=`**: `forall a. (EqOp a) => (a, a) -> Bool`
  - Supported for `Int`, `Float`, `Bool`, `Char`, and `String` (content equality via `strcmp`).
- **`<`, `<=`, `>`, `>=`**: `forall a. (OrdOp a) => (a, a) -> Bool`
  - Supported for `Int`, `Float`, `Char`, and `String` (lexicographical order).

## Short-Circuit Logical Operators

- **`&&`**: `(Bool, Bool) -> Bool` — evaluates right operand **only** if left operand is `true`.
- **`||`**: `(Bool, Bool) -> Bool` — evaluates right operand **only** if left operand is `false`.

```
def safeCheck(d: Int, n: Int): Bool =
    d != 0 && (n / d) > 1
```
