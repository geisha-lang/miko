# Built-in Functions

This document describes the built-in functions available in Geisha programs.

## Runtime Library

The runtime library is provided in `base.o` and must be linked with compiled programs:

```bash
cc -o program base.o program.o
```

## I/O Functions

### printLn

Prints a string followed by a newline.

```
Type: String -> Void
```

Usage:
```
def main() = printLn("Hello, World!")
```

### putNumber

Prints a 32-bit integer.

```
Type: Int -> Void
```

Usage:
```
def main() = putNumber(42)
```

Output: `42`

### putFloat

Prints a 64-bit floating point number.

```
Type: Float -> Void
```

Usage:
```
def main() = putFloat(3.14159)
```

Output: `3.141590`

### putChar

Prints a single character.

```
Type: Char -> Void
```

Usage:
```
def main() = putChar('A')
```

Output: `A`

## Runtime Implementation

The runtime functions are implemented in C (`src/lib/base.c`):

```c
#include <stdio.h>
#include <stdint.h>

void printLn(const char *s, void *fv) {
    printf("%s\n", s);
}

void putChar(const char c, void *fv) {
    printf("%c", c);
}

void putNumber(const uint32_t n, void *fv) {
    printf("%u", n);
}

void putFloat(const double f, void *fv) {
    printf("%f", f);
}
```

### Free Variable Parameter

All runtime functions take a `void *fv` parameter for the closure environment. This parameter is unused in the built-in functions but is required for consistency with the calling convention.

## Operators as Built-ins

### Arithmetic Operators

These operators are compiled to LLVM instructions:

| Operator | Int Instruction | Float Instruction |
|----------|-----------------|-------------------|
| `+` | `add` | `fadd` |
| `-` | `sub` | `fsub` |
| `*` | `mul` | `fmul` |
| `/` | `sdiv` | `fdiv` |
| `%` | `srem` | N/A |

### Comparison Operators

| Operator | Int Instruction | Float Instruction |
|----------|-----------------|-------------------|
| `==` | `icmp eq` | `fcmp oeq` |
| `!=` | `icmp ne` | `fcmp one` |
| `<` | `icmp slt` | `fcmp olt` |
| `<=` | `icmp sle` | `fcmp ole` |
| `>` | `icmp sgt` | `fcmp ogt` |
| `>=` | `icmp sge` | `fcmp oge` |

### Logical Operators

| Operator | Implementation |
|----------|----------------|
| `&&` | Short-circuit with branch |
| `\|\|` | Short-circuit with branch |

## Prelude Environment

The type inference phase initializes with these built-in types:

```rust
// Primitive types
Int     // 32-bit signed integer
Float   // 64-bit double
String  // UTF-8 string
Bool    // Boolean
Void    // Unit type

// Built-in functions
printLn  : String -> Void
putNumber: Int -> Void
putFloat : Float -> Void
putChar  : Char -> Void

// Operators (polymorphic)
(+)  : forall a. a * a -> a
(-)  : forall a. a * a -> a
(*)  : forall a. a * a -> a
(/)  : forall a. a * a -> a
(%)  : Int * Int -> Int
(==) : forall a. a * a -> Bool
(!=) : forall a. a * a -> Bool
(<)  : forall a. a * a -> Bool
(<=) : forall a. a * a -> Bool
(>)  : forall a. a * a -> Bool
(>=) : forall a. a * a -> Bool
(&&) : Bool * Bool -> Bool
(||) : Bool * Bool -> Bool
```

## Examples

### Hello World

```
def main() = printLn("Hello, World!")
```

### Printing Numbers

```
def main() = {
    putNumber(42),
    printLn(""),
    putFloat(3.14),
    printLn("")
}
```

### Fibonacci with Output

```
def fib(n) =
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)

def main() = putNumber(fib(10))
```

### Factorial

```
def factorial(n) =
    if (n == 0) 1
    else n * factorial(n - 1)

def main() = putNumber(factorial(10))
```

## Building the Runtime

To rebuild `base.o` from source:

```bash
cc -c -o base.o src/lib/base.c
```

## Adding New Built-ins

To add a new built-in function:

1. Add the C implementation to `src/lib/base.c`:
   ```c
   void myFunction(int arg, void *fv) {
       // implementation
   }
   ```

2. Rebuild `base.o`:
   ```bash
   cc -c -o base.o src/lib/base.c
   ```

3. Add the type signature to the prelude in `src/typeinfer/infer.rs`

4. Add code generation support in `src/codegen/emit.rs`

## Limitations

- No input functions (reading from stdin)
- No file I/O
- No string manipulation functions
- No memory allocation functions exposed to Geisha

These would need to be added to the runtime for more complete programs.
