# Syntax Reference

This document describes the complete syntax of the Geisha programming language.

## Lexical Elements

### Identifiers

**Lowercase identifiers** (variables, functions, modules):
```
[a-z_][a-zA-Z0-9_]*
```

Examples: `x`, `myFunction`, `list_length`, `_unused`

**Uppercase identifiers** (type constructors, data constructors, concepts):
```
[A-Z][a-zA-Z0-9_]*
```

Examples: `Int`, `Maybe`, `Cons`, `Nothing`, `Eq`

### Literals

**Integers** (32-bit signed):
```
42
0
-123
```

**Floats** (64-bit double precision):
```
3.14
2.0
-1.5e-2
0.001
```

**Characters** (8-bit ASCII / UTF-8 byte):
```
'a'
'\n'
'\t'
'\\'
'\''
```

**Strings** (UTF-8 with escape sequences):
```
"hello"
"world\n"
"tab\there"
"quote\"inside"
"slash\\path"
```

Escape sequences: `\n` (newline), `\r` (carriage return), `\t` (tab), `\"` (quote), `\'` (single quote), `\\` (backslash), `\0` (null).

**Booleans**:
```
true
false
```

**Unit**:
```
()
```

### Keywords

```
def      - Function/value definition
let      - Local binding
in       - Part of let expression
if       - Conditional or pattern guard
else     - Conditional alternative
match    - Pattern matching
data     - ADT definition
type     - Type alias
concept  - Typeclass definition
instance - Typeclass implementation
forall   - Universal quantification
pub      - Public visibility modifier
mod      - Module declaration or inline module
use      - Selective import
open     - Glob import
as       - Import alias
true     - Boolean literal
false    - Boolean literal
```

### Comments

```
// Single-line comment

/* Multi-line
   comment */
```

## Program Structure

A Geisha source file consists of module items (`mod`, `use`, `open`) and top-level definitions (`def`, `data`, `type`, `concept`, `instance`), optionally marked `pub`. Executable programs define a `main` function as the entry point.

## Definition Forms

### Value Definition

```
def name = expression
def name: Type = expression
```

Examples:
```
def pi = 3.14159
def greeting: String = "Hello"
```

### Function Definition

```
def name(param1, param2, ...) = body
def name(param1: Type1, param2: Type2): ReturnType = body
```

Forward type signature annotations are also supported:
```
add: (Int, Int) -> Int
def add(x, y) = x + y
```

### Type Alias

```
type TypeName a b = TypeExpression
```

Examples:
```
type IntList = List Int
type Pair a b = (a, b)
```

### Algebraic Data Type

```
data TypeName a b {
    Variant1,
    Variant2(Type1, Type2),
    Variant3 { field1: Type1, field2: Type2 },
}
```

### Concept Definition

```
concept ConceptName a {
    method1: TypeScheme,
    method2: TypeScheme,
}
```

Example:
```
concept Eq a {
    eq: (a, a) -> Bool
}
```

### Instance Definition

```
instance ConceptName Type {
    def method1(args) = body,
    def method2(args) = body,
}

instance (Constraint a) => ConceptName (Type a) {
    def method1(args) = body,
}
```

## Expression Forms

### Literals, Unit, and Tuples

```
42                  // Int
3.14                // Float
'A'                 // Char
"hello"             // String
true                // Bool
()                  // Void (unit)
(1, "hi")           // Tuple of type (Int, String)
(1, 2, true)        // 3-tuple of type (Int, Int, Bool)
```

### Function Application

```
f()
f(x)
f(x, y, z)
```

### Struct Variant Construction

```
Circle { radius: 3.0 }
Rectangle { width: 4.0, height: 5.0 }
```

### Lambda Expressions

```
() -> 42
(x) -> x + 1
(x: Int, y: Int): Int -> x + y
```

### Let Expressions

```
let variable = value in body
let variable: Type = value in body
```

### Conditional Expressions

```
if (condition) thenExpr else elseExpr
```

### Match Expressions (with Optional Guards)

```
match expr {
    pattern1 -> result1,
    pattern2 if (guardExpr) -> result2,
    _ -> default,
}
```

### List Literals

```
[1, 2, 3, 4]
[]
```

List literals construct values of the standard `List a` ADT (`Cons` / `Nil`).

### Block Expressions

Expressions inside `{ ... }` are separated by `;` or newlines; the value of the block is its final expression:

```
{
    putNumber(42);
    printLn("");
    0
}
```

### Unary and Binary Operators

```
-x
!flag
x + y
n == 0 && m != 1
```

See [Operators](operators.md) for the complete precedence table.

## Type Syntax

### Primitive Types

```
Int
Float
Char
String
Bool
Void    // Also written ()
```

### Function Types (Explicit Arity, Right-Associative `->`)

Multi-parameter functions list parameter types in parentheses; `->` is right-associative:

```
() -> Void                  // Zero-argument function
Int -> Int                  // Single-argument function (shorthand for (Int) -> Int)
(Int, Int) -> Bool          // Two-argument function
Int -> Int -> Int           // Curried: Int -> (Int -> Int)
((Int, String)) -> Int      // Single-argument function taking a 2-tuple
```

### First-Class Tuple Types

```
(Int, String)
(Int, Float, Bool)
(a, b)
```

### Type Application

```
List Int
Maybe String
Either Int String
```

### Polymorphic & Constrained Types

```
forall a. a -> a
forall a b. (a, b) -> a
forall a. (Eq a) => (a, a) -> Bool
forall k v. (Eq k) => (k, List (k, v)) -> Maybe v
```

## Pattern Syntax

- **Wildcard**: `_`
- **Variable**: `x`
- **Literal**: `0`, `42`, `3.14`, `'a'`, `"hello"`, `true`, `()`
- **Tuple**: `(x, y)`, `(a, (b, c))`
- **Constructor**: `Nothing`, `Just(x)`, `Cons(h, Cons(next, rest))`
- **Struct Constructor**: `Circle { radius: r }`, `Rectangle { width, height }`
