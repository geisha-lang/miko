# Recursive ADT Segmentation Fault

## Status: FIXED

The issue has been fixed in the GC runtime.

## Issue

The `adt_list.gs` test program was crashing with a segmentation fault when using heap-allocated ADTs via `gc_alloc()`. Other ADT tests (non-recursive) worked correctly.

## Root Cause

The bug was in `src/lib/runtime/gc_bitmap.c` in the `new_heap()` function. The `gc_heap_t` structure has a flexible array member `bits[]` for the allocation bitmap:

```c
typedef struct {
    uintptr_t base;
    uintptr_t end;
    size_t bitslen;
    bitmap_t bits[];  // Flexible array member
} gc_heap_t;
```

The original code set the heap base (`ret->base`) immediately after `sizeof(gc_heap_t)`, which only accounts for the fixed fields (24 bytes). However, the `bits[]` flexible array lives right after those fixed fields and occupies `bitslen * sizeof(bitmap_t)` bytes.

This caused the bitmap array and the heap space to overlap. When `bitmap_set_alloc()` wrote allocation markers to `_gc_heap->bits[idx]`, it was actually overwriting heap object data, corrupting the ADT structures.

## Fix

The fix was to advance the heap base pointer past both the fixed struct fields AND the bitmap array:

```c
// Before (BUGGY):
INC_PTR_BYTES(base, sizeof(gc_heap_t));
ret->base = (uintptr_t)base;

// After (FIXED):
INC_PTR_BYTES(base, sizeof(gc_heap_t) + bitslen * Bitmap_size);
ret->base = (uintptr_t)base;
```

## Test Case

```geisha
data List a {
    Nil,
    Cons(a, List a)
}

def length(lst) = match lst {
    Nil -> 0,
    Cons(h, t) -> 1 + length(t)
}

def main() = putNumber(length(Cons(1, Cons(2, Cons(3, Nil)))))
```

Expected and actual output after fix: `3`

## Related Files

- `src/lib/runtime/gc_bitmap.c` - Contains the fix in `new_heap()`
- `src/lib/include/gc_bitmap.h` - GC structures and macros
- `test/adt_list.gs` - Test case that was failing
