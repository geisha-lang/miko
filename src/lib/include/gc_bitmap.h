#ifndef GC_BITMAP_H
#define GC_BITMAP_H

#include <stdint.h>
#include <stddef.h>

#include "gc.h"

typedef uint64_t bitmap_t;

#define BITS_PER_BITMAP (sizeof(bitmap_t) * 8)

typedef struct gc_free_t {
    size_t size;
    struct gc_free_t *next;
} gc_free_t;

typedef struct gc_heap_t {
    uintptr_t base;
    uintptr_t end;
    size_t total_words;
    size_t bitslen;
    bitmap_t *alloc_bits;
    bitmap_t *mark_bits;
    struct gc_heap_t *next;
} gc_heap_t;

#define WORD_INDEX(_byte_offset) ((_byte_offset) / WORD_SIZE)
#define OFFSET_TO_IDX(_byte_offset) (WORD_INDEX(_byte_offset) / BITS_PER_BITMAP)
#define OFFSET_TO_MASK(_byte_offset) (((bitmap_t)1) << (WORD_INDEX(_byte_offset) % BITS_PER_BITMAP))

#endif
