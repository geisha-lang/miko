# Memory Layout & Garbage Collection

## Value Representation (LLVM 20 Opaque Pointers)

| Geisha Type | LLVM Type | Notes |
|-------------|-----------|-------|
| `Int` | `i32` | 32-bit signed integer |
| `Float` | `double` | 64-bit IEEE-754 float |
| `Char` | `i8` | 8-bit character |
| `Bool` | `i1` | Boolean |
| `String` | `ptr` | Global/heap null-terminated UTF-8 C string |
| `Void` / `()` | `void` / `i8 0` | Unit value |
| `(T1, T2, ...)` | `{ T1_llvm, T2_llvm, ... }` | Unpacked first-class tuple value |
| Function `(...) -> R` | `ptr` | Pointer to `{ ptr fn_entry, env_struct }` closure |
| ADT `Data ...` | `ptr` | Pointer to `{ i32 tag, [N x i64] }` ABI-sized inline union |
| `Cell T` | `ptr` | Pointer to GC-allocated cell holding `T_llvm` at 8-byte aligned payload start |
| `MutArray T` | `ptr` | Pointer to GC-allocated `{ i64 len, [len x T_llvm] }` buffer (elements start at +8B) |

## ABI-Sized ADT Union Layout

For each instantiated ADT, the compiler computes the **maximum ABI byte size and alignment** across all variant payload structs (`LLVMABISizeOfType` / `LLVMABIAlignmentOfType`) and allocates `{ i32 tag, payload_storage }` large enough and properly aligned for every variant—preventing truncation when a single-field `Float` variant is larger than a multi-field `Bool` variant.

## Fixed-Point Escape Analysis

`src/core/escape.rs` determines stack (`alloca`) vs heap (`gc_alloc`) allocation:
1. Self-recursive and mutually recursive ADTs are detected via graph reachability over `AdtRegistry` and always heap-allocated.
2. Interprocedural parameter escape analysis computes `NoEscape` / `EscapesReturn` / `EscapesCapture` / `EscapesStore` to a fixed point.
3. Intraprocedural fixed-point propagation ensures that if an outer ADT or closure escapes, **every inner allocation stored inside its fields or captured in its environment also escapes to the heap**.

## Mark-and-Sweep Bitmap GC (`src/lib/runtime/gc_bitmap.c`)

- **Initialization**: `main` passes an entry-frame stack anchor to `gc_init_with_stack(default_words, &stack_anchor)`. The initial heap defaults to 128K words (1 MiB) and can be overridden at runtime via the `GEISHA_GC_HEAP_WORDS` environment variable (and grows automatically when needed).
- **Root Scanning**: Before marking, `bitmap_collect` calls `setjmp` to spill callee-saved registers onto the stack, scans the downward-growing stack `[sp, root_start]` word-by-word, and scans registered top-level global pointer roots (`gc_register_root`).
- **Bitmap Marking & Sweeping**: Each 64-bit `bitmap_t` word tracks 64 heap words (`1` bit per 8-byte word). `bitmap_mark_obj` checks the mark bit first to prevent cycles, reads the object's word count from its header (`header >> 1`), recursively marks interior heap pointers, and coalesces swept runs onto the free list.
