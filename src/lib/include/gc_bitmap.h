#ifndef GC_BITMAP_H
#define GC_BITMAP_H

#include <stdint.h>
#include <stddef.h>

#include "gc.h"

// use one word for a single bitmap line
typedef size_t bitmap_t;

#define Bitmap_size sizeof(bitmap_t)

// // linked list manage heaps
// typedef struct {
//     gc_heap_t *heap;
//     gc_hplst_t *next;
//     mark_ctx_t *mark;
// } gc_hplst_t;

typedef struct {
    uintptr_t base;
    uintptr_t end;
    size_t bitslen;
    bitmap_t bits[];
} gc_heap_t;

typedef struct gc_free_t {
    size_t size;
    struct gc_free_t *next;
} gc_free_t;

typedef struct {
    bitmap_t *bits;
    size_t bitslen;
} mark_ctx_t;

#define Get_free_size(p) (((gc_free_t *)(p))->size)
#define Set_free_size(p, s) (Get_free_size(p) = s)
#define Get_free_next(p) (((gc_free_t *)(p))->next)
#define Set_free_next(p, n) (((gc_free_t *)(p))->next = (gc_free_t *)(n))

void visitRootObj(void *obj);


#define INC_PTR_BYTES(p, b) (p = (char *)p + b)
#define OFFSET_TO_IDX(_offset) ((_offset) / WORD_SIZE / sizeof(bitmap_t))
#define OFFSET_TO_MASK(_offset) (1 << ((_offset) % sizeof(bitmap_t)))

#endif