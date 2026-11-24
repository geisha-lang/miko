# Pattern Matching

Pattern matching deconstructs ADTs, tuples, and primitive literals with full support for nested patterns, guards, and compile-time exhaustiveness checking.

## Match Syntax

```
match scrutinee {
    pattern1 -> expr1,
    pattern2 if (guard) -> expr2,
    _ -> fallback,
}
```

Arms are tested top-to-bottom; the first arm whose pattern matches **and** whose optional `if (guard)` evaluates to `true` produces the result.

## Supported Patterns

- **Wildcard**: `_`
- **Variable binding**: `x`
- **Literals**: `0`, `-1`, `3.14`, `'a'`, `"hello"`, `true`, `false`, `()`
- **Tuples**: `(x, y)`, `(0, (a, _))`
- **ADT Tuple/Unit Constructors**: `Nil`, `Cons(h, Cons(next, rest))` (in any order relative to `data` declaration)
- **ADT Struct Constructors**: `Circle { radius: r }`, `Rectangle { width, height }` (field punning supported)

## Guards

```
def classify(n: Int): String = match n {
    0 -> "zero",
    x if (x > 0) -> "positive",
    _ -> "negative",
}
```

If `x > 0` evaluates to `false`, execution falls through to the next arm (`_ -> "negative"`).

## Exhaustiveness Checking

Every `match` expression is checked for exhaustiveness at compile time:
- Matching an ADT must cover all variants with unguarded patterns, or include an unguarded wildcard/variable arm.
- Matching `Bool` must cover both `true` and `false` (or `_`).
- Matching `Int`, `Float`, `Char`, or `String` must include an unguarded wildcard or variable catch-all arm.
