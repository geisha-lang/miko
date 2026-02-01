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

ADTs use a compact tagged representation: a tag identifying the variant, followed by an inline payload struct.

### Structure

```llvm
%adt = type { i32, %payload }
; Field 0: variant tag (0, 1, 2, ...)
; Field 1: payload struct (sized for largest variant)
```

The payload is **inline** (not a pointer), which provides better cache locality and avoids an extra indirection.

### Instantiated Types

Polymorphic ADTs are instantiated with concrete types during monomorphization. For example:

```
data List a { Nil, Cons(a, List a) }

List Int  → { i32, { i32, %List_Int* } }
List Bool → { i32, { i1, %List_Bool* } }
```

The payload struct uses the **concrete types** from instantiation, not generic pointer types.

### Example: Maybe

```
data Maybe a {
    Nothing,   // tag = 0
    Just(a)    // tag = 1
}
```

For `Maybe Int`:
```llvm
%Maybe_Int = type { i32, { i32 } }
; Field 0: tag (0 = Nothing, 1 = Just)
; Field 1: payload struct with one i32 field
```

Memory layout:
```
Nothing:
    { tag: 0, payload: { undef } }

Just(42):
    { tag: 1, payload: { 42 } }
```

### Example: List

```
data List a {
    Nil,           // tag = 0
    Cons(a, List a) // tag = 1
}
```

For `List Int`:
```llvm
%List_Int = type { i32, { i32, %List_Int* } }
; Field 0: tag
; Field 1: payload with head value and tail pointer
```

Memory layout:
```
Nil:
    { tag: 0, payload: { undef, null } }

Cons(1, Cons(2, Nil)):
    { tag: 1, payload: { 1, ptr to inner } }
    where inner = { tag: 1, payload: { 2, ptr to nil } }
    where nil = { tag: 0, payload: { undef, null } }
```

### Payload Sizing

The payload struct is sized for the **largest variant**:

```
data Tree a {
    Leaf(a),              // 1 field
    Node(Tree a, Tree a)  // 2 fields (largest)
}
```

For `Tree Int`:
```llvm
%Tree_Int = type { i32, { %Tree_Int*, %Tree_Int* } }
; Payload sized for Node (2 pointers)
; Leaf uses only first field
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

### Bitcasting for Variant Access

When accessing fields, the payload pointer is bitcast to the variant's specific type:

```llvm
; For Leaf(x) pattern on Tree Int:
%payload_ptr = getelementptr %Tree_Int, %tree, 0, 1
%leaf_payload = bitcast { %Tree_Int*, %Tree_Int* }* %payload_ptr to { i32 }*
%x = load i32, { i32 }* %leaf_payload, 0
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

## Garbage Collection

Geisha uses a mark-and-sweep garbage collector for heap-allocated values.

### Initialization

The GC is initialized at program start with a configurable heap size:

```llvm
; In main():
call void @gc_init(i64 131072)  ; 1MB heap (131072 words × 8 bytes)
```

### Allocation

Heap allocation uses `gc_alloc`:

```llvm
%ptr = call i8* @gc_alloc(i64 %size_bytes)
```

### When GC Runs

Collection is triggered automatically when:
- Heap space is exhausted during allocation
- The allocator cannot find a free block of sufficient size

### Root Scanning

The collector scans:
- Stack frames (using frame pointer introspection)
- Global variables

## Escape Analysis

Escape analysis determines whether values can be stack-allocated or must be heap-allocated.

### Purpose

- Reduce GC pressure by stack-allocating short-lived values
- Improve cache locality for local data
- Eliminate allocation overhead for non-escaping values

### Implementation

Located in `src/core/escape.rs`. Runs after K-conversion, before code generation.

### Escape Rules

A value **escapes** (must be heap-allocated) if:

1. **Captured by closure**: Pointer is copied into a closure's environment
2. **Returned from function**: In tail position of a function
3. **Stored in escaping ADT**: Field of an ADT that itself escapes
4. **Passed to escaping parameter**: Callee's corresponding parameter escapes
5. **Recursive ADT**: Can grow unboundedly (e.g., List, Tree)

A value **does not escape** (can be stack-allocated) if:
- Used only in local computations
- Pattern matched and destructured locally
- Passed to non-escaping function parameters

### Example: Stack-Allocated ADT

```
def main() =
    let p = MkPair(10, 20) in   // p does not escape
    match p {
        MkPair(a, b) -> putNumber(a + b)
    }
```

Generated code uses `alloca`:
```llvm
%adt.stack = alloca { i32, { i32, i32 } }
```

### Example: Heap-Allocated ADT

```
def identity(x) = x    // x escapes via return

def main() =
    let p = MkPair(1, 2) in
    putNumber(match identity(p) { ... })
```

Generated code uses `gc_alloc`:
```llvm
%adt.raw = call i8* @gc_alloc(i64 12)
```

### Recursive ADT Detection

The compiler detects self-referential ADTs using `AdtInfo::is_recursive()`:

```
data List a { Nil, Cons(a, List a) }  // Recursive: Cons contains List a
data Pair a b { MkPair(a, b) }        // Not recursive
```

Recursive ADTs are always heap-allocated because they can grow to arbitrary size.

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

1. **Generational GC**: Separate young/old generations for better performance
2. **Unboxing Optimization**: Avoid pointer indirection for small types
3. **Region-Based Memory**: Efficient allocation and deallocation for related objects
4. **Size-Based Stack Threshold**: Stack-allocate values under a size limit
