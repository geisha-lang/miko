# Operators

This document describes all operators in Geisha, their types, and precedence.

## Operator Precedence

Operators are listed from highest to lowest precedence:

| Precedence | Operators | Associativity | Description |
|------------|-----------|---------------|-------------|
| Highest | Function application | Left | `f(x)` |
| 6 | `*`, `/`, `%` | Left | Multiplicative |
| 5 | `+`, `-` | Left | Additive |
| 4 | `<`, `<=`, `>`, `>=`, `==`, `!=` | Left | Comparison |
| 3 | `&&` | Left | Logical AND |
| 2 | `\|\|` | Left | Logical OR |
| Lowest | `->` | Right | Function type |

## Arithmetic Operators

### Addition `+`

```
Type: forall a. a * a -> a (with numeric constraint)
```

Examples:
```
5 + 3        // 8
3.14 + 2.0   // 5.14
```

### Subtraction `-`

```
Type: forall a. a * a -> a (with numeric constraint)
```

Examples:
```
10 - 4       // 6
5.5 - 2.0    // 3.5
```

### Multiplication `*`

```
Type: forall a. a * a -> a (with numeric constraint)
```

Examples:
```
6 * 7        // 42
2.5 * 4.0    // 10.0
```

### Division `/`

```
Type: forall a. a * a -> a (with numeric constraint)
```

Examples:
```
20 / 4       // 5 (integer division for Int)
10.0 / 4.0   // 2.5
```

### Modulo `%`

```
Type: Int * Int -> Int
```

Examples:
```
17 % 5       // 2
10 % 3       // 1
```

## Comparison Operators

All comparison operators return `Bool`.

### Equality `==`

```
Type: forall a. a * a -> Bool
```

Examples:
```
5 == 5       // true
5 == 6       // false
"hi" == "hi" // true
```

### Inequality `!=`

```
Type: forall a. a * a -> Bool
```

Examples:
```
5 != 6       // true
5 != 5       // false
```

### Less Than `<`

```
Type: forall a. a * a -> Bool
```

Examples:
```
3 < 5        // true
5 < 3        // false
5 < 5        // false
```

### Less Than or Equal `<=`

```
Type: forall a. a * a -> Bool
```

Examples:
```
3 <= 5       // true
5 <= 5       // true
6 <= 5       // false
```

### Greater Than `>`

```
Type: forall a. a * a -> Bool
```

Examples:
```
5 > 3        // true
3 > 5        // false
5 > 5        // false
```

### Greater Than or Equal `>=`

```
Type: forall a. a * a -> Bool
```

Examples:
```
5 >= 3       // true
5 >= 5       // true
3 >= 5       // false
```

## Logical Operators

### Logical AND `&&`

```
Type: Bool * Bool -> Bool
```

Short-circuit behavior: if the left operand is `false`, the right operand is not computed.

Examples:
```
true && true    // true
true && false   // false
false && true   // false (right side not computed)
false && false  // false (right side not computed)
```

### Logical OR `||`

```
Type: Bool * Bool -> Bool
```

Short-circuit behavior: if the left operand is `true`, the right operand is not computed.

Examples:
```
true || true    // true (right side not computed)
true || false   // true (right side not computed)
false || true   // true
false || false  // false
```

## Operator Associativity

All binary operators are left-associative:

```
a + b + c    // ((a + b) + c)
a - b - c    // ((a - b) - c)
a * b * c    // ((a * b) * c)
a && b && c  // ((a && b) && c)
a || b || c  // ((a || b) || c)
```

## Precedence Examples

```
// Multiplicative before additive
2 + 3 * 4        // 2 + (3 * 4) = 14
2 * 3 + 4        // (2 * 3) + 4 = 10

// Additive before comparison
x + 1 < y + 2    // (x + 1) < (y + 2)

// Comparison before logical
x < 5 && y > 0   // (x < 5) && (y > 0)
x == 0 || y == 0 // (x == 0) || (y == 0)

// AND before OR
a || b && c      // a || (b && c)
a && b || c && d // (a && b) || (c && d)
```

## Using Parentheses

Override precedence with parentheses:

```
(2 + 3) * 4      // 20
2 * (3 + 4)      // 14
(a || b) && c    // Compute OR first
```

## Type-Specific Behavior

### Integer Operators

For `Int` operands:

```
10 / 3    // 3 (integer division, truncates)
10 % 3    // 1 (remainder)
```

### Float Operators

For `Float` operands:

```
10.0 / 3.0    // 3.333...
```

Float modulo is not supported.

## Comparison Chaining

Comparisons do not chain; use logical operators:

```
// Wrong (doesn't work as expected)
// 0 < x < 10

// Correct
0 < x && x < 10
```

## Examples

### Absolute Value

```
def abs(x) = if (x < 0) 0 - x else x
```

### Clamp

```
def clamp(x, lo, hi) =
    if (x < lo) lo
    else if (x > hi) hi
    else x
```

### Sign

```
def sign(x) =
    if (x < 0) -1
    else if (x > 0) 1
    else 0
```

### Boolean Logic

```
def xor(a, b) = (a || b) && !(a && b)

def implies(a, b) = !a || b

def nand(a, b) = !(a && b)
```

### Range Check

```
def inRange(x, lo, hi) = x >= lo && x <= hi
```

### Maximum of Three

```
def max3(a, b, c) =
    if (a >= b && a >= c) a
    else if (b >= a && b >= c) b
    else c
```
