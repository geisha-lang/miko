# Built-in Functions & Operators

## Standard I/O Functions (`src/lib/base.c`)

- **`printLn : String -> Void`** — Prints a UTF-8 string followed by a newline.
- **`putNumber : Int -> Void`** — Prints a signed 32-bit integer (`%d`).
- **`putFloat : Float -> Void`** — Prints a 64-bit floating-point number (`%g` / `%f`).
- **`putChar : Char -> Void`** — Prints a single character.

## Runtime String Helpers

- **`gs_str_eq(a, b)`** / **`gs_str_cmp(a, b)`** — Used by `==`, `!=`, `<`, `<=`, `>`, `>=` on `String` operands for content comparison.

## Built-in Prelude Types & Operators

- Primitive types: `Int`, `Float`, `Char`, `String`, `Bool`, `Void` (`()`)
- Standard `List a { Nil, Cons(a, List a) }` (available by default for `[e1, e2, ...]` list literals unless overridden by a user `data List` declaration)
- Arithmetic: `+`, `-`, `*`, `/` (`Num`: `Int`, `Float`), `%` (`Int`), unary `-` (`Num`)
- Comparison: `==`, `!=` (`EqOp`: `Int`, `Float`, `Bool`, `Char`, `String`), `<`, `<=`, `>`, `>=` (`OrdOp`: `Int`, `Float`, `Char`, `String`)
- Logical: `!` (`Bool -> Bool`), short-circuit `&&` and `||` (`(Bool, Bool) -> Bool`)
