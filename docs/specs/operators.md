# Operators

This document describes all unary, binary, and custom symbolic operators in Geisha, their open `Concept` contracts, precedence, and associativity rules.

## Operator Precedence & Associativity

Geisha combines Scala / Swift leading-character operator buckets with ergonomic Rust / Swift bitwise precedence (placing `&`, `^`, `|`, and shifts `<<`, `>>`, `>>>` **above** equality and relational comparisons so expressions like `flags & mask == 0`, `16 > x << 2`, and `a | b == c || d` parse naturally without extra parentheses). Any infix operator ending with `:` (such as `::` or `+:`) is **right-associative**; all other infix operators are **left-associative**.

| Level | Form / Leading Character | Associativity | Examples |
|-------|--------------------------|---------------|----------|
| Highest | `f(x)` | Left | Function application |
| 14 | `!`, `-`, `~` (prefix) | Prefix | `!flag`, `-x`, `~mask` |
| 13 | `@`, `#`, `$`, `?`, multi-char `~`, `\` | Left (`:`-suffix: Right) | `~>`, `~:` |
| 12 | `*`, `/`, `%` | Left (`:`-suffix: Right) | `*`, `/`, `%`, `**` |
| 11 | `+`, `-` | Left (`:`-suffix: Right) | `+`, `-`, `++`, `+:` (right-assoc) |
| 10 | `<<`, `>>`, `>>>` | Left | `1 << n + 1`, `-16 >> 2`, `-16 >>> 28` |
| 9 | `&`-leading (except `&&`) | Left (`:`-suffix: Right) | `&`, `&+`, `&&&` |
| 8 | `^`-leading | Left (`:`-suffix: Right) | `^`, `^^` |
| 7 | `\|` (single char) | Left | `a \| b` |
| 6 | `:`-leading | Left (`:`-suffix: Right) | `::` (right-assoc) |
| 5 | `<`, `>`-leading (except `<<`, `>>`, `>>>`) | Left (`:`-suffix: Right) | `<`, `<=`, `>`, `>=`, `<+>`, `<\|>`, `>>=` |
| 4 | `=`, `!`-leading (except `=`, `!`) | Left (`:`-suffix: Right) | `==`, `!=`, `===`, `=/=` |
| 3 | `&&` | Left | Short-circuit logical AND |
| 2 | `\|\|` | Left | Short-circuit logical OR |
| Lowest (1) | Multi-char `\|`-leading (except `\|\|`) | Left (`:`-suffix: Right) | `\|>` (`a + b \|> f`) |

*(In type expressions, type application `List a` binds tighter than `->`, and `->` is **right-associative**.)*

## Open Operator Concepts & Static Dispatch

All built-in arithmetic, bitwise, and comparison operators are backed by open `Concept`s resolved via **static monomorphization** (zero runtime dictionary overhead):

| Operator(s) | Open `Concept` | Signature | Accepted `instance` Method Name(s) | Built-in Primitive Instances |
|-------------|----------------|-----------|------------------------------------|------------------------------|
| `+` | `Add a` | `(a, a) -> a` | `(+)` or `add` | `Int`, `Int64`, `Float`, `String` |
| `-` | `Sub a` | `(a, a) -> a` | `(-)` or `sub` | `Int`, `Int64`, `Float` |
| `*` | `Mul a` | `(a, a) -> a` | `(*)` or `mul` | `Int`, `Int64`, `Float` |
| `/` | `Div a` | `(a, a) -> a` | `(/)` or `div` | `Int`, `Int64`, `Float` |
| `%` | `Rem a` | `(a, a) -> a` | `(%)` or `rem` | `Int`, `Int64` |
| `-x` (unary) | `Neg a` | `a -> a` | `neg` or `(-)` | `Int`, `Int64`, `Float` |
| `&` | `BitAnd a` | `(a, a) -> a` | `(&)` or `bitAnd` | `Int`, `Int64`, `Bool` |
| `\|` | `BitOr a` | `(a, a) -> a` | `(\|)` or `bitOr` | `Int`, `Int64`, `Bool` |
| `^` | `BitXor a` | `(a, a) -> a` | `(^)` or `bitXor` | `Int`, `Int64`, `Bool` |
| `~x` (unary) | `BitNot a` | `a -> a` | `(~)` or `bitNot` | `Int`, `Int64` |
| `<<` | `Shl a` | `(a, Int) -> a` | `(<<)`, `shl`, or `bitShl` | `Int`, `Int64` |
| `>>` | `Shr a` | `(a, Int) -> a` | `(>>)`, `shr`, or `bitShr` | `Int`, `Int64` (arithmetic `ashr`) |
| `>>>` | `Ushr a` | `(a, Int) -> a` | `(>>>)`, `ushr`, or `bitUshr` | `Int`, `Int64` (logical `lshr`) |
| `==`, `!=` | `Eq a` | `(a, a) -> Bool` | `(==)` or `eq` | `Int`, `Int64`, `Float`, `Bool`, `Char`, `String` |
| `<`, `<=`, `>`, `>=` | `Ord a` | `(a, a) -> Int` | `compare` | `Int`, `Int64`, `Float`, `Char`, `String` |

- **Primitive fast path**: Built-in primitive operands emit direct LLVM instructions (`add`, `and`, `or`, `xor`, `shl`, `ashr`, `lshr`, `icmp`, `fcmp`, etc.).
- **Operator sections `(op)`**: Parenthesized operators such as `(+)`, `(&)`, `(|)`, `(<<)`, `(~)`, `(|>)`, and `(<+>)` can be used in `def`, `concept`, `instance`, `use` imports, and as first-class function values (`listFold(xs, 0, (|))`).

## Low-Level Bitwise Intrinsics & `std.bits`

In addition to the operator Concepts above, the prelude exposes direct intrinsics for `Int` (`bitAnd`, `bitOr`, `bitXor`, `bitNot`, `bitShl`, `bitShr`, `bitUshr`) and `Int64` (`bitAnd64`, `bitOr64`, `bitXor64`, `bitNot64`, `bitShl64`, `bitShr64`, `bitUshr64`), and `std.bits` provides `hasFlag`, `setFlag`, `clearFlag`, `toggleFlag`, `popCount`, `popCount64`, and `trailingZeros`.
