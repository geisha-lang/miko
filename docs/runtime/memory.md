# Memory Layout

This document describes how Geisha values are represented in memory.

## Primitive Types

### Int

32-bit signed integer.

```llvm
i32
```

Examples:
```
42    → i32 42
-100  → i32 -100
```

### Float

64-bit double-precision floating point.

```llvm
double
```

Examples:
```
3.14  → double 3.14
-2.5  → double -2.5
```

### Bool

1-bit integer.

```llvm
i1
```

Examples:
```
true  → i1 1
false → i1 0
```

### String

Pointer to null-terminated UTF-8 string.

```llvm
i8*
```

String literals are stored in the constant data section.

### Void

No runtime representation. Used only for type-checking functions that perform side effects.

```llvm
void
```

## Function Types

Functions are represented as closures with an entry pointer and environment.

```llvm
; Generic closure type
%closure = type { i8*, i8* }
; Field 0: function pointer
; Field 1: environment pointer
```

For a function `Int -> Int`:

```llvm
{ i32 (i32, i8*)*, i8* }
```

## Algebraic Data Types

ADTs use a tagged representation: a tag identifying the variant, plus the variant's payload.

### Structure

```llvm
%adt = type { i32, i8* }
; Field 0: variant tag (0, 1, 2, ...)
; Field 1: payload pointer
```

### Example: Maybe

```
data Maybe a {
    Nothing,   // tag = 0
    Just(a)    // tag = 1
}
```

Memory layout:
```
Nothing:
    { tag: 0, payload: null }

Just(42):
    { tag: 1, payload: ptr to i32 containing 42 }
```

### Example: List

```
data List a {
    Nil,           // tag = 0
    Cons(a, List a) // tag = 1
}
```

Memory layout:
```
Nil:
    { tag: 0, payload: null }

Cons(1, Cons(2, Nil)):
    { tag: 1, payload: ptr to { i32: 1, List*: ptr to inner } }
    where inner = { tag: 1, payload: ptr to { i32: 2, List*: ptr to nil } }
    where nil = { tag: 0, payload: null }
```

### Variant Tags

Tags are assigned sequentially:
- First variant: 0
- Second variant: 1
- And so on...

```
data Color { Red, Green, Blue }
// Red = 0, Green = 1, Blue = 2
```

## Stack Allocation

Local variables use LLVM's `alloca` instruction:

```llvm
define i32 @example() {
entry:
    %x = alloca i32           ; Allocate space for x
    store i32 42, i32* %x     ; Store value
    %result = load i32, i32* %x  ; Load value
    ret i32 %result
}
```

### Let Bindings

```
let x = 5 in x + 1
```

Compiles to:
```llvm
%x = alloca i32
store i32 5, i32* %x
%x_val = load i32, i32* %x
%result = add i32 %x_val, 1
```

## Closure Environments

Closure environments are structs containing captured values:

```
def makeAdder(n) = (x) -> n + x
```

Environment type:
```llvm
%env.makeAdder.lambda = type { i32 }  ; Captures n
```

Creation:
```llvm
%env = alloca %env.makeAdder.lambda
%env.n = getelementptr %env, 0, 0
store i32 %n, i32* %env.n
```

## Pattern Match Compilation

Pattern matching on ADTs uses tag checks and field extraction.

### Tag Extraction

```llvm
%tag = extractvalue %adt %value, 0
```

### Field Extraction

```llvm
%payload_ptr = extractvalue %adt %value, 1
%field = load i32, i32* %payload_ptr
```

### Switch on Tag

```llvm
switch i32 %tag, label %default [
    i32 0, label %case_Nothing
    i32 1, label %case_Just
]
```

## Memory Safety

### Current State

- Stack allocation for most values
- No garbage collection
- No runtime bounds checking

### Implications

- Closures that escape their defining scope may cause issues
- Recursive data structures may cause stack overflow
- Memory leaks possible with dynamically allocated data

## Example: Complete Layout

For this program:

```
data Point { Point(Int, Int) }

def distance(p) = match p {
    Point(x, y) -> x * x + y * y
}

def main() =
    let p = Point(3, 4) in
    putNumber(distance(p))
```

Memory usage:

1. `Point(3, 4)` construction:
   ```llvm
   %payload = alloca { i32, i32 }
   store { i32, i32 } { i32 3, i32 4 }, %payload
   %point = alloca %adt
   store %adt { i32 0, i8* %payload }, %point
   ```

2. Pattern match in `distance`:
   ```llvm
   %tag = extractvalue %adt %p, 0
   ; tag is 0 for Point
   %payload = extractvalue %adt %p, 1
   %x = extractvalue { i32, i32 } %payload, 0
   %y = extractvalue { i32, i32 } %payload, 1
   ```

3. Computation:
   ```llvm
   %x2 = mul i32 %x, %x      ; 9
   %y2 = mul i32 %y, %y      ; 16
   %result = add i32 %x2, %y2 ; 25
   ```

## Future Considerations

Potential improvements:

1. **Heap Allocation**: For values that escape stack scope
2. **Garbage Collection**: Automatic memory management
3. **Unboxing Optimization**: Avoid pointer indirection for small types
4. **Region-Based Memory**: Efficient allocation and deallocation
