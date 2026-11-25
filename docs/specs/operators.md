# Operators

This document describes all unary and binary operators in Geisha, their types, semantics, and precedence.

## Operator Precedence & Associativity (Scala / Swift Leading-Character Rule)

Both built-in and user-defined symbolic operators follow a fixed precedence table determined by the operator's **first character**, and **right-associativity** iff the operator **ends with `:`** (e.g. `::`, `+:`, `~:`):

| Precedence | Leading Character / Form | Associativity | Examples |
|------------|--------------------------|---------------|----------|
| Highest | `f(x)` | Left | Function application |
| 11 | `!`, `-` (prefix) | Prefix | `!flag`, `-x` |
| 10 | `@`, `#`, `$`, `?`, `~`, `\` | Left (`:`-suffix: Right) | `~>`, `<~` (by first char) |
| 9 | `*`, `/`, `%` | Left (`:`-suffix: Right) | `*`, `/`, `%`, `**` |
| 8 | `+`, `-` | Left (`:`-suffix: Right) | `+`, `-`, `++`, `+:` (right-assoc!) |
| 7 | `:` | Left (`:`-suffix: Right) | `::` (right-assoc!) |
| 6 | `<`, `>` | Left (`:`-suffix: Right) | `<`, `<=`, `>`, `>=`, `<+>`, `<\|>`, `>>=` |
| 5 | `=`, `!` | Left (`:`-suffix: Right) | `==`, `!=`, `===`, `=/=` |
| 4 | `&&`, `&`-leading | Left (`:`-suffix: Right) | `&&` (short-circuit), `&&&` |
| 3 | `^` | Left (`:`-suffix: Right) | `^^` |
| 2 | `\|\|` | Left | `\|\|` (short-circuit) |
| Lowest (1) | `\|`-leading (except `\|\|`) | Left (`:`-suffix: Right) | `\|>` (pipeline: `a + b \|> f`) |

*(In type expressions, type application `List a` binds tighter than `->`, and `->` is **right-associative**.)*

## Concept-Based Operator Overloading

All built-in arithmetic and comparison operators are backed by open Concepts (`Add`, `Sub`, `Mul`, `Div`, `Rem`, `Neg`, `Eq`, `Ord`) resolved statically during monomorphization:

- **Primitive fast path**: `Int`, `Int64`, `Float`, `Bool`, `Char`, and `String` (including `"a" + "b"` string concatenation via `Add String`) compile directly to native LLVM instructions.
- **Custom ADT overloading**: Implementing `Add`, `Sub`, `Mul`, `Div`, `Rem`, `Neg`, `Eq`, or `Ord` for a user type enables `+`, `-`, `*`, `/`, `%`, unary `-`, `==`, `!=`, `<`, `<=`, `>`, `>=` on that type (including generic constrained instances such as `instance (Eq a) => Eq (Maybe a)`).
- **Custom symbolic operators & `(op)` sections**: Parenthesized operator names `(op)` can be used in `def (|>)(x, f) = f(x)`, `concept Semigroup a { (<+>): (a, a) -> a }`, `instance` implementations, `use` imports, and as first-class function values (`listFold(xs, 0, (+))`).

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
